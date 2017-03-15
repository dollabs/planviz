;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(def project 'dollabs/planviz)
(def version "0.9.4-SNAPSHOT")
(def description "Planning Network Visualization")
(def project-url "https://github.com/dollabs/planviz")
(def main 'planviz.cli)

(set-env!
  :source-paths #{"src"}
  :resource-paths #{"resources"}
  :dependencies   '[[org.clojure/clojure          "1.8.0"]
                    [org.clojure/clojurescript    "1.9.473"]
                    ;; both
                    [avenir                       "0.2.2"]
                    [dollabs/webtasks             "0.2.3"]
                    [dollabs/webkeys              "0.4.2"]
                    [org.clojure/core.async       "0.2.395"]
                    ;; server
                    [org.clojure/tools.cli        "0.3.5"]
                    [me.raynes/fs                 "1.4.6"]
                    [com.cognitect/transit-clj    "0.8.297"]
                    [environ                      "1.1.0"]
                    [clj-time                     "0.13.0"]
                    [com.taoensso/timbre          "4.8.0"]
                    [org.slf4j/slf4j-api          "1.7.22"]
                    [com.fzakaria/slf4j-timbre    "0.3.2"]
                    [org.clojure/tools.logging    "0.3.1"]
                    [com.novemberain/langohr      "3.6.1"]
                    [dollabs/plan-schema          "0.2.16"]
                    ;; web server
                    [org.clojure/data.json        "0.2.6"]
                    [ring/ring-core               "1.5.0"]
                    [ring                         "1.5.0"]
                    [ring/ring-defaults           "0.2.3"]
                    [amalloy/ring-gzip-middleware "0.1.3"]
                    [compojure                    "1.5.0"]
                    [enlive                       "1.1.6"]
                    [aleph                        "0.4.2-alpha12"]
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
                    [adzerk/boot-reload           "0.5.1"     :scope "test"]
                    [pandeiro/boot-http           "0.7.6"     :scope "test"
                     :exclusions [org.clojure/clojure]]
                    [adzerk/boot-cljs             "1.7.228-2" :scope "test"]
                    [adzerk/boot-cljs-repl        "0.3.3"     :scope "test"]
                    ;; testing/development
                    ;; [adzerk/boot-test "1.2.0" :scope "test"]
                    ;; [crisptrutski/boot-cljs-test "0.3.0" :scope "test"]
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
  cljs {:optimizations :advanced} ;; for production
  serve {:dir "target/public"}
  ;; test-cljs {:js-env :phantom
  ;;            :namespaces #{"testing.planviz.client"}}
  )

(deftask build-cljs
  "Compile ClojureScript"
  []
  (comp
    (notify
      :visual true
      :title "CLJS"
      :messages {:success "http://localhost:3000 is ready\n"})
    (speak)
    (sift :include #{#"~$"} :invert true) ;; don't include emacs backups
    (cljs)
    (target :dir #{"target"})))

(deftask build-jar
  "Build the project locally as a JAR."
  ;; [d dir PATH #{str} "the set of directories to write to (target)."]
  []
  ;; (let [dir (if (seq dir) dir #{"target"})]
  (comp
    (sift :include #{#"~$"} :invert true) ;; don't include emacs backups
    (cljs)
    (aot)
    (pom)
    (uber)
    (jar :file (str (name project) ".jar"))
    (target :dir #{"target"}))
  ;;)
  )

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

(deftask development
  "Example task to change ClojureScript options for development"
  [p port PORT int "The port for the ClojureScript nREPL"]
  (task-options!
    cljs {:optimizations :none :source-map true}
    reload {;; :ws-port 9001
            :on-jsload 'planviz.client/reload}
    repl {:port (or port 8082)
          :middleware '[cemerick.piggieback/wrap-cljs-repl]})
  identity)

;; This will start an nREPL server on 8082 that a remote IDE
;; can connect to for the CLJS REPL.
(deftask cljs-dev
  "Starts CLJS nREPL"
  [p port PORT int "The port for the ClojureScript nREPL"]
  (comp
    (development :port port)
    (serve)
    (watch)
    (reload)
    (cljs-repl)
    (build-cljs)))

;; This will start an nREPL server on 8081 that a remote IDE
;; can connect to for the CLJ REPL.
(deftask clj-dev
  "Starts CLJ nREPL"
  [p port PORT int "The port for the Clojure nREPL"]
  (comp
    (repl
      :port (or port 8081)
      :server true
      :init "src/planviz/server.clj"
      :init-ns 'planviz.server)
    (wait)))

;; For Emacs if you Customize your Cider Boot Parameters to 'cider-boot'
;; then this task will be invoked upon M-x cider-jack-in-clojurescript
;; which is on C-c M-J
;; CIDER will then open two REPL buffers for you, one for CLJS
;; and another for CLJ. FFI:
;; https://cider.readthedocs.io/en/latest/up_and_running/#clojurescript-usage

;; This task is commented out here for users that have not copied
;; a profile.boot file to ~/.boot/ which defines the cider task:
;;
;; (deftask cider-boot
;;   "Cider boot params task"
;;   []
;;   (comp
;;     (cider) ;; defined in profile.boot
;;     ;; FFI https://github.com/boot-clj/boot/wiki/Cider-REPL
;;     ;;     https://cider.readthedocs.io/en/latest/installation/
;;     (cljs-dev)))
