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
            [planviz.actions :as actions]))

(enable-console-print!)

;; web url ----------------------------------------------------

(def secure-protocols {"https" true
                       "wss" true})

(defn protocol-secure [protocol secure]
  (if secure
    (if (get secure-protocols protocol false)
      protocol ;; already secure
      (str protocol "s")) ;; add the "s"
    (if (get secure-protocols protocol false)
      (subs protocol 0 (dec (count protocol))) ;; remove the "s"
      protocol)))

(defn make-url [& opts]
  (let [opts (if opts (apply hash-map opts))
        {:keys [protocol hostname port uri secure]} opts
        location (.-location js/document)
        p (let [p (.-protocol location)] (subs p 0 (dec (count p)))) ;; no ":"
        secure (or secure (get secure-protocols p false))
        protocol (or protocol p)
        protocol (protocol-secure protocol secure)
        hostname (or hostname (.-hostname location))
        port (str (or port (.-port location)))
        server (str hostname (if-not (empty? port) (str ":" port)))
        uri (or uri "/")
        url (str protocol "://" server uri)]
    url))

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
      (ws/add-rmethod ws/ws
        :network-update actions/network-update)
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
    #(ws/setup (make-url :protocol "ws" :port 8080 :uri "/ws")
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
