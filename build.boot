;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(def project 'dollabs/planviz)
(def version "0.8.9")
(def description "Planning Network Visualization")
(def project-url "https://github.com/dollabs/planviz")
(def main 'planviz.cli)

(set-env!
  :source-paths #{"src" "html"}
  :resource-paths #{"src" "html"}
  :dependencies   '[[org.clojure/clojure "1.8.0"]
                    [org.clojure/clojurescript "1.9.293"]
                    ;; both
                    [avenir "0.2.1"]
                    [dollabs/webtasks "0.2.1"]
                    [dollabs/webkeys "0.2.0"]
                    [org.clojure/core.async "0.2.395"]
                    ;; server
                    [org.clojure/tools.cli "0.3.5"]
                    [me.raynes/fs "1.4.6"]
                    [com.cognitect/transit-clj "0.8.290"]
                    [environ "1.1.0"]
                    [clj-time "0.12.0"]
                    [com.taoensso/timbre "4.7.4"]
                    [org.slf4j/slf4j-api "1.7.21"]
                    [com.fzakaria/slf4j-timbre "0.3.2"]
                    [org.clojure/tools.logging "0.3.1"]
                    [com.novemberain/langohr "3.6.1"]
                    [dollabs/plan-schema "0.2.12"]
                    ;; web server
                    [org.clojure/data.json "0.2.6"]
                    [ring/ring-core "1.5.0"]
                    [ring "1.5.0"]
                    [ring/ring-defaults "0.2.1"]
                    [amalloy/ring-gzip-middleware "0.1.3"]
                    [compojure "1.5.0"]
                    [enlive "1.1.6"]
                    [aleph "0.4.2-alpha8"]
                    ;; client
                    [com.cognitect/transit-cljs "0.8.239"]
                    [cljsjs/react-dom-server "15.3.1-0"]  ;; for sablono
                    [cljsjs/react-dom "15.3.1-0"] ;; for sablono
                    [org.omcljs/om "1.0.0-alpha40"]
                    [sablono "0.7.5"]
                    ;; cljs-dev
                    [com.cemerick/piggieback "0.2.1"     :scope "test"]
                    [weasel                 "0.7.0"      :scope "test"]
                    [org.clojure/tools.nrepl "0.2.12"    :scope "test"]
                    [adzerk/boot-reload     "0.4.13"     :scope "test"]
                    [pandeiro/boot-http "0.7.3" :scope "test"]
                    [adzerk/boot-cljs       "1.7.228-2"  :scope "test"]
                    [adzerk/boot-cljs-repl  "0.3.3"      :scope "test"]
                    ;; testing/development
                    ;; [adzerk/boot-test "1.1.1" :scope "test"]
                    ;; [crisptrutski/boot-cljs-test "0.2.2-SNAPSHOT" :scope "test"]
                    ])

(require
  '[clojure.string :as string]
  '[clojure.java.io :as io]
  '[clojure.pprint :refer [pprint]]
  '[boot.util :as util]
  '[environ.core :refer [env]]
  '[me.raynes.fs :as fs]
  '[adzerk.boot-cljs      :refer [cljs]]
  '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl repl-env]]
  '[pandeiro.boot-http :refer [serve]]
  '[adzerk.boot-reload    :refer [reload]]
  ;; '[adzerk.boot-test :refer [test]]
  ;; '[crisptrutski.boot-cljs-test :refer [test-cljs]]
  )

(task-options!
  pom {:project     project
       :version     version
       :description description
       :url         project-url
       :scm         {:url project-url}
       :license     {"Apache-2.0" "http://opensource.org/licenses/Apache-2.0"}}
  aot {:namespace   #{main}}
  jar {:main        main}
  cljs {:source-map true}
  ;; test-cljs {:js-env :phantom
  ;;            :namespaces #{"testing.planviz.client"}}
  )

;; suboptimal approach to chosing which edn file to use
;; for specifying ClojureScript options
;; https://github.com/adzerk-oss/boot-cljs/wiki/Usage
(deftask set-mode!
  "set-mode!"
  [m mode KW kw "mode: :dev or :prod."]
  (let [mode (or mode :dev)
        ext (str "." (name mode))
        tmp (tmp-dir!)]
    ;; (println (str "MODE =" mode "=") (type mode))
    (fn middleware [next-handler]
      (fn handler [fileset]
        (empty-dir! tmp)
        (let [in-files (input-files fileset)
              mode-files (by-ext [ext] in-files)]
          (doseq [in mode-files]
            (let [in-file  (tmp-file in)
                  in-path  (tmp-path in)
                  out-path (string/replace in-path ext "")
                  out-file (io/file tmp out-path)
                  ]
              ;; (println "in-path" in-path "out-path" out-path)
              (doto out-file
                io/make-parents
                (spit (slurp in-file)))
              ))
          (-> fileset
            (add-resource tmp)
            commit!
            next-handler))))))

(deftask clj-dev
  "Clojure REPL for CIDER"
  []
  (comp
    (cider)
    (repl :server true)
    (wait)))

(deftask server-resources
  "force dev resources"
  []
  (set-env! :resource-paths #{"resources"})
  identity)

(deftask cljs-dev
  "ClojureScript Browser REPL for CIDER"
  []
  (comp
    (sift :include #{#"~$"} :invert true) ;; don't include emacs backups
    (set-mode!)
    (cider)
    (serve :dir "resources/public" :port 3000)
    (watch :verbose false)
    (reload)
    (cljs-repl) ;; before cljs
    (cljs)
    (target :dir #{"resources"})))

(deftask build-cljs
  "Run the project."
  [d dir PATH str "the  directory to write to (resources)."]
  (let [dir (if (string? dir) dir "resources")]
    (comp
      (sift :include #{#"~$"} :invert true) ;; don't include emacs backups
      (set-mode!)
      (cljs)
      (target :dir #{dir}))))

(deftask cider-boot
  "Cider boot params task"
  []
  (if false ;; CIDER works on the client (true) or server (false)
    (cljs-dev)
    (comp
      (server-resources)
      (build-cljs)
      (clj-dev))))

(deftask build-jar
  "Build the project locally as a JAR."
  [d dir PATH #{str} "the set of directories to write to (target)."]
  (let [dir (if (seq dir) dir #{"target"})]
    (comp
      (sift :include #{#"~$"} :invert true) ;; don't include emacs backups
      (set-mode! :mode :prod)
      (cljs)
      (aot)
      ;; (pom)
      (uber)
      (jar :file (str (name project) ".jar"))
      (target :dir dir))))

(deftask cli
  "Run the project."
  [a args ARG [str] "the arguments for the application."]
  (require [main :as 'app])
  (let [argv (if (pos? (count args))
               (clojure.string/split (first args) #" ")
               '())]
    (with-post-wrap [_]
      (apply (resolve 'app/-main) argv))))

(deftask run
  "Run the project."
  [a args ARG [str] "the arguments for the application."]
  (reset! util/*verbosity* 0) ;; quiet output
  (comp
    (build-cljs)
    (cli :args args)))
