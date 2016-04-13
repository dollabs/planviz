;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns planviz.cli
  "Temporal Planning Network schema command line interface"
  (:require [clojure.string :as string]
            [clojure.java.io :refer :all] ;; for as-file
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :as pp :refer [pprint]]
            [environ.core :refer [env]]
            [clojure.java.shell :refer [sh]]
            [me.raynes.fs :as fs]
            [planviz.server :as server])
  (:gen-class))

(defn hostname []
  (try
    (let [{:keys [exit out err]} (sh "hostname")]
      (if (zero? exit)
        (string/replace out #"\s" "")
        "localhost"))
    (catch Exception e
      (println "unable to run the hostname command:"
        (.. e getCause getMessage))
      "localhost"
      )))


(def default-action "visualize")


(defn save-config
  "Save command line arguments to config/planviz.edn"
  {:added "0.8.0"}
  [options]
  (let [{:keys [help version verbose exchange input
                rmq-host rmq-port host port
                cwd arguments]} options
        cmd (or (first arguments) default-action)
        options (assoc (dissoc options :cwd) :arguments
                  (if (= cmd "save-config") [] [cmd]))
        config-dir (str cwd "/config")
        config-file (str config-dir "/planviz.edn")]
    (if (not (fs/exists? config-dir))
      (fs/mkdir config-dir))
    (spit config-file (with-out-str (pprint options)))
    (println "saved configuration to" config-file)))

(def #^{:added "0.1.0"}
  actions
  "Valid planviz command line actions"
  {"visualize" (var server/visualize)
   "save-config" #'save-config})

(def #^{:added "0.1.0"}
  cli-options
  "Command line options"
  [["-h" "--help" "Print usage"]
   ["-V" "--version" "Print planviz version"]
   ["-v" "--verbose" "Increase verbosity"
    :default 0
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ["-e" "--exchange EXCHANGE" "RMQ Exchange Name"
    :default "tpn-updates"]
   ["-i" "--input INPUT" "Input file(s) {TPN|HTN|HTN=TPN}"
    :default []
    :assoc-fn (fn [m k v]
                (let [oldv (get m k [])
                      oldv (if (= oldv ["-"]) [] oldv)]
                  (assoc m k (conj oldv v))))]
   ["-q" "--rmq-host RMQHOST" "RMQ Host"
    :default "localhost"]
   ["-r" "--rmq-port RMQPORT" "RMQ Port"
    :default 5672 :parse-fn #(Integer/parseInt %)]
   ["-n" "--host HOST" "PLANVIZ server hostname"
    :default (hostname)]
   ["-p" "--port PORT" "PLANVIZ server port"
    :default 8080 :parse-fn #(Integer/parseInt %)]])

(defn usage
  "Print planviz command line help."
  {:added "0.1.0"}
  [options-summary]
  (->> (for [a (sort (keys actions))]
         (str "  " a "\t" (:doc (meta (get actions a)))))
    (concat [""
             "planviz"
             ""
             "Usage: planviz [options] action"
             ""
             "Options:"
             options-summary
             ""
             "Actions:"])
    (string/join \newline)))

(defn exit
  "Exit planviz with given status code (and optional messages)."
  {:added "0.1.0"}
  [status & msgs]
  (if msgs (println (string/join \newline msgs)))
  (when (server/repl?)
    (throw (Exception. (str "DEV MODE exit(" status ")"))))
  (shutdown-agents)
  (System/exit status)
  true)

(defn config-parse-opts [cwd args cli-options]
  (let [config-file (if (= 1 (count args))
                      (str cwd "/config/" (first args) ".edn"))]
    (if (and config-file (fs/exists? config-file))
      (let [options (load-file config-file)
            arguments (:arguments options)
            rv {:options (dissoc options :arguments)
                :arguments arguments
                :summary (:summary (parse-opts args cli-options))}]
        rv)
      (parse-opts args cli-options))))

(defn planviz
  "planviz command line processor. (see usage for help)."
  {:added "0.1.0"
   :version "0.8.0"}
  [& args]
  ;; (println (str "DEBUG planviz args: " (pr-str args)))
  (let [cwd (or (:planviz-cwd env) (:user-dir env))
        {:keys [options arguments errors summary]}
        (config-parse-opts cwd args cli-options)
        {:keys [help version verbose exchange input
                rmq-host rmq-port host port]} options
        options (assoc options :cwd cwd :arguments arguments)
        cmd (or (last arguments) default-action)
        action (get actions cmd)
        verbose? (pos? (or verbose 0))
        exit?
        (cond
          (= action #'save-config)
          (exit 0 (action options))
          help
          (exit 0 (usage summary))
          errors
          (exit 1 (string/join \newline errors) (usage summary))
          version
          (exit 0 (:version (meta #'planviz)))
          (> (count arguments) 1)
          (exit 1 "Specify exactly one action" (usage summary)))]
    (when (and verbose? (not exit?))
      (when (> verbose 1)
        (println "repl?:" (server/repl?))
        (println "cwd:" cwd)
        ;; (println "version:" (:planviz-version env))
        )
      (println "verbosity level:" verbose)
      (println "rmq-host:" rmq-host)
      (println "rmq-port:" rmq-port)
      (println "exchange:" exchange)
      (println "host:" host)
      (println "port:" port)
      (println "input:" input)
      (println "cmd:" cmd (if action "(valid)" "(invalid)")))
    (if-not action
      (if-not exit?
        (exit 1 (str "Unknown action: \"" cmd "\". Must be one of: "
                  (keys actions)))
        (usage summary))
      (try
        (action options)
        (catch Throwable e ;; note AssertionError not derived from Exception
          ;; FIXME: use proper logging
          (binding [*out* *err*]
            (println "ERROR caught exception:" (.getMessage e)))
          (exit 1))))
    (exit 0)))

(defn -main
  "planviz"
  {:added "0.1.0"}
  [& args]
  (apply planviz args))
