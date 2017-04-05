;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns planviz.client
  "planviz main program"
  (:require [clojure.string :as string]
            [goog.object :as gobject]
            [goog.string :as gstring]
            [webtasks.tasks :as tasks :refer [on-realized]]
            [webtasks.ws :as ws]
            [plan-schema.core :as pschema :refer [composite-key-fn]]
            [planviz.actions :as actions :refer [make-url]]
            [weasel.repl :as repl] ;; for CIDER development
            ))

(enable-console-print!)

;; server functions --------------------------------------

(defn read-default [message]
  (println "read-default:" message)
  (cond
    (:remote message)
    (actions/status-msg (str "you are connecting from " (:remote message)))
    :else ;; true
    (actions/status-msg message)))

(defn login [ & args]
  (if (fn? ws/add-rmethod)
    (do
      ;; (ws/add-rmethod ws/ws
      ;;   :network-update actions/network-update)
      (ws/add-rmethod ws/ws
        :network-updates actions/network-updates)
      (ws/add-rmethod ws/ws
        :recv-msg actions/recv-msg)
      (ws/add-rmethod ws/ws
        :user-action actions/user-action)
      (ws/add-rmethod ws/ws
        :add-plan actions/add-plan)
      (ws/add-rmethod ws/ws
        :network-reset actions/network-reset)
      )
    (println "ADD RMETHOD not defined??"))
  (actions/login))

(defn initialize [bounce]
  (tasks/on-started
    #(ws/setup (make-url :protocol "ws" :uri "/ws" :debug-port [3000 8080])
       {:message read-default
        :open login}))
  (if bounce
    (tasks/restart)
    (tasks/initialize))
  (actions/initialize))

;; --------------------------------------------------------

(defn main []
  (println "main ------------------ initialized:" (initialize false)))

(set! (.-onload js/window) main)

(defn reload []
  (main))

;; Set the value below to true if using IntelliJ/Cursive (otherwise set to false)
;; see NOTE below
(def cursive? false)

(defonce weasel? (atom cursive?)) ;; see NOTE below

(when-not @weasel?  ;; see NOTE below
  ;; Connect via Weasel to nREPL for development
  (println "connecting to Weasel..")
  (repl/connect "ws://localhost:9001")
  (reset! weasel? true))
