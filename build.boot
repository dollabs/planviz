;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(def project 'dollabs/planviz)
(def version "0.9.2")
(def description "Planning Network Visualization")
(def project-url "https://github.com/dollabs/planviz")
(def main 'planviz.cli)

(set-env!
  :source-paths #{"src"}
  :resource-paths #{"resources"}
  :dependencies   '[[org.clojure/clojure          "1.8.0"]
                    [org.clojure/clojurescript    "1.9.293"]
                    ;; both
                    [avenir                       "0.2.1"]
                    [dollabs/webtasks             "0.2.2"]
                    [dollabs/webkeys              "0.4.1"]
                    [org.clojure/core.async       "0.2.395"]
                    ;; server
                    [org.clojure/tools.cli        "0.3.5"]
                    [me.raynes/fs                 "1.4.6"]
                    [com.cognitect/transit-clj    "0.8.295"]
                    [environ                      "1.1.0"]
                    [clj-time                     "0.12.2"]
                    [com.taoensso/timbre          "4.7.4"]
                    [org.slf4j/slf4j-api          "1.7.21"]
                    [com.fzakaria/slf4j-timbre    "0.3.2"]
                    [org.clojure/tools.logging    "0.3.1"]
                    [com.novemberain/langohr      "3.6.1"]
                    [dollabs/plan-schema          "0.2.14"]
                    ;; web server
                    [org.clojure/data.json        "0.2.6"]
                    [ring/ring-core               "1.5.0"]
                    [ring                         "1.5.0"]
                    [ring/ring-defaults           "0.2.1"]
                    [amalloy/ring-gzip-middleware "0.1.3"]
                    [compojure                    "1.5.0"]
                    [enlive                       "1.1.6"]
                    [aleph                        "0.4.2-alpha10"]
                    ;; client
                    [com.cognitect/transit-cljs   "0.8.239"]
                    [cljsjs/react-dom-server      "15.3.1-1"]  ;; for sablono
                    [cljsjs/react-dom             "15.3.1-1"] ;; for sablono
                    [org.omcljs/om                "1.0.0-alpha40"]
                    [sablono "0.7.6"]
                    ;; cljs-dev
                    [com.cemerick/piggieback      "0.2.1"     :scope "test"]
                    [weasel                       "0.7.0"     :scope "test"]
                    [org.clojure/tools.nrepl      "0.2.12"    :scope "test"]
                    [adzerk/boot-reload           "0.4.13"    :scope "test"]
                    [pandeiro/boot-http           "0.7.6"     :scope "test"]
                    [adzerk/boot-cljs             "1.7.228-2" :scope "test"]
                    [adzerk/boot-cljs-repl        "0.3.3"     :scope "test"]
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

(deftask build-cljs
  "Compile ClojureScript"
  []
  (comp
    (sift :include #{#"~$"} :invert true) ;; don't include emacs backups
    (cljs)
    (target :dir #{"target"})))

(deftask build-jar
  "Build the project locally as a JAR."
  [d dir PATH #{str} "the set of directories to write to (target)."]
  (let [dir (if (seq dir) dir #{"target"})]
    (comp
      (sift :include #{#"~$"} :invert true) ;; don't include emacs backups
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
  ;; (reset! util/*verbosity* 0) ;; quiet output
  (comp
    (build-cljs)
    (cli :args args)))
