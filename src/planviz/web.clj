;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns planviz.web
  "Dynamic web page creation"
  (:require [net.cgrand.enlive-html :refer :all]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.util.response :refer [response content-type]]))

(def public "../../resources/public/")

(def app-css-uri  "css/planviz.css")

(def app-css (str public app-css-uri))

(def app "js/app.js")

(def title "planviz")

(def lf "\n")

(def basic-page
  [{:tag :html
    :attrs nil
    :content
    [lf {:tag :head
         :attrs nil
         :content
         [lf {:tag :title
              :attrs nil
              :content [ title ]}
          lf]}
     lf {:tag :body
         :attrs nil
         :content
         [lf]}
     lf]}])

(defn html5dtd [nodes]
  (if (= (:type (first nodes)) :dtd)
    nodes
    (cons {:type :dtd :data ["html" nil nil]} nodes)))

(defn add-meta [nodes meta]
  (at nodes
    [:head] (append
              (html [:meta meta]) lf)))

(defn charset-utf-8 [nodes]
  (add-meta nodes {:charset "utf-8"}))

(defn viewport [nodes]
  (add-meta nodes {:name "viewport"
                   :content "width=device-width,initial-scale=1.0,maximum-scale=1.0,minimum-scale=1.0,user-scalable=0"}))

(defn html-lang [nodes lang]
  (at nodes
    [:head] (set-attr :lang lang)))

(defn append-at [selector nodes new-nodes]
  (at nodes
    [selector] (append new-nodes lf)))

(defn append-head [nodes new-head-nodes]
  (append-at :head nodes new-head-nodes))

(defn append-body [nodes new-body-nodes]
  (append-at :body nodes new-body-nodes))

(defn add-favicon [nodes]
  (append-head nodes
    (html [:link {:rel "icon" :href "/favicon.ico" :type "image/x-icon"}])))

(defn add-css [nodes style-uri]
  (append-head nodes
    (html [:link {:rel "stylesheet" :href style-uri :type "text/css"}])))

(defn add-js [nodes script-uri]
  (append-head nodes
    (html [:script {:type "text/javascript" :src script-uri}])))

(defn add-h1 [nodes heading]
  (append-body nodes
    (html [:h1 heading])))

(defn add-div [nodes id]
  (append-body nodes
    (html [:div {:id id }])))

(defn add-textarea [nodes id]
  (append-body nodes
    (html [:textarea {:id id }])))

(defn render-snippet [s]
  (-> (apply str (emit* s))
    response
    (content-type "text/html")))

(def basic-html5 (-> basic-page
                   html5dtd
                   (html-lang "en")
                   charset-utf-8
                   ;; viewport
                   ))

(defn create-dev-html [& [req]]
  (-> basic-html5
    (add-favicon)
    (add-css app-css-uri)
    (add-js app)
    (add-div "app")
    (add-div "plans")
    ;; (add-div "status-line")
    render-snippet))

;; for production
(defn create-html [& [req]]
  (-> basic-html5
    (add-favicon)
    (add-css app-css-uri)
    (add-js app)
    (add-div "app")
    (add-div "plans")
    render-snippet))

(defn create-test-html [& [req]]
  (-> basic-html5
    ;; (add-favicon)
    (add-css app-css)
    (add-js app)
    (add-div "app")
    (add-textarea "out")
    render-snippet))

(defn write-html [html-file]
  (let [html-str (create-test-html)]
    (println "writing" app "into" html-file)
    (spit html-file html-str)))
