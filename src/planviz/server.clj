;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;; Prevent aleph from pulling in netty's logging at :debug level
;; before we properly initialize logging
(require '(taoensso timbre))
(taoensso.timbre/set-level! :warn)

(ns planviz.server
  (:gen-class) ;; for :uberjar
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.cli :as cli]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [compojure.core :refer [GET ANY defroutes]]
            [compojure.route :refer [files resources not-found]]
            [ring.util.response
             :refer [resource-response content-type url-response]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.params :as params]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [environ.core :refer [env]]
            [taoensso.timbre :as timbre]
            [clojure.tools.logging :as log]
            [clj-time.core :as time]
            [clj-time.format :as timef]
            [clj-time.local :as timel]
            [clj-time.coerce :as timec]
            [aleph.http :as http]
            [manifold.stream :as s]
            [manifold.deferred :as d]
            [manifold.time :as dtime]
            [planviz.web :as web]
            [clojure.core.async :as async
             :refer [chan <! >! put! go-loop close!]]
            [planviz.transit :refer [transit-to-json transit-from-json]]
            [langohr.core :as rmq]
            [langohr.exchange :as le]
            [langohr.queue :as lq]
            [langohr.consumers :as lc]
            [langohr.channel :as lch]
            [langohr.basic :as lb]
            [plan-schema.core :as pschema
             :refer [composite-key read-json-str write-json-str]]
            [avenir.utils :as au :refer [assoc-if keywordize concatv and-fn]]
            [me.raynes.fs :as fs]
            [planviz.net :as net])
  (:import [java.lang
            IllegalArgumentException]))

(defn sleep
  "sleeps for the given number of seconds (may be fractional)"
  [s]
  (Thread/sleep (* 1000 s)))

;; partition m into n parts
(defn partition-map [n m]
  (mapv (partial into {}) (partition-all n m)))

(def settings-suffix ".settings.edn")

(def planviz-settings {:settings/auto false
                       :settings/tooltips true
                       :settings/font-family "font-family-helvetica"
                       :settings/font-size "font-size-9"
                       :settings/font-weight "font-weight-light"
                       :settings/rewrap-htn-labels true
                       :settings/xchar 5
                       :settings/ychar 10
                       :settings/filename "default"
                       :settings/filenames ["default"]
                       })

(defn repl?
  "Helper function returns true if on the REPL (for development)"
  {:added "0.2.0"}
  []
  (= (:pager env) "cat"))

;; ---------------------------------------

;; (defn hostname []
;;   (try
;;     (let [{:keys [exit out err]} (sh "hostname")]
;;       (if (zero? exit)
;;         (string/replace out #"\s" "")
;;         "localhost"))
;;     (catch Exception e
;;       (println "unable to run the hostname command:"
;;         (.. e getCause getMessage))
;;       "localhost")))

(def rmq-default-host "localhost")
(def rmq-default-port 5672)
(def rmq-default-exchange "tpn-updates")
;; (def planviz-default-host (hostname))
(def planviz-default-host "localhost")
(def planviz-default-port 8080)
(def pamela-visualization-key "pamela.viz")

(def state-initial
  {:server nil ;; Aleph server
   :logging false ;; true if logging has been initialized
   :heartbeat-cancel nil ;; if running, else nil
   :msgs nil ;; core async channel
   :rmq {:rmq-host rmq-default-host
         :rmq-port rmq-default-port
         :exchange rmq-default-exchange
         :channel nil
         :connection nil}
   :plans {}
   :url-config []
   }
  )

;; The server is the http server which will serve the application
(defonce state (atom state-initial))
;; {:server server ;; Aleph server
;;  :logging true ;; true if logging has been initialized
;;  :clients
;;    {"remote address" ;; client
;;      {:conn conn}}
;;  :heartbeat-cancel cancel-fn ;; if running, else nil
;;  :rmethods {:fn-name fn}
;;  :msgs msgs ;; core async channel
;;  :rmq {
;;        :host rmq-host
;;        :port rmq-port
;;        :exchange "tpn-updates"
;;        :channel rmq-channel
;;        :connection rmq-connection
;;  :plans
;;    {:plan-id
;;
;;    }
;; }

(def log-config
  {;; :level :debug  ; e/o #{:trace :debug :info :warn :error :fatal :report}
   ;; Control log filtering by namespaces/patterns. Useful for turning off
   ;; logging in noisy libraries, etc.:
   :ns-whitelist  [] #_["my-app.foo-ns"]
   :ns-blacklist  [] #_["taoensso.*"]
   :middleware [] ; (fns [data]) -> ?data, applied left->right
   ;; {:pattern _ :locale _ :timezone _}
   ;; :timestamp-opts log/default-timestamp-opts
   :timestamp-opts   {:pattern  "yy-MMM-dd HH:mm:ss.SSS"
                      :locale   (java.util.Locale. "en")
                      :timezone (java.util.TimeZone/getDefault)
                      ;; (java.util.TimeZone/getTimeZone "UTC")
                      }

   :output-fn timbre/default-output-fn ; (fn [data]) -> string
   ;; :appenders
   ;; {:spit (log/spit-appender {:fname "./logs/planviz.log"})}
   })

(defn log-initialize [port log-level & [debug-args]]
  (let [logfile (str "./logs/planviz-" port ".log")
        appenders {:println (timbre/println-appender {:stream :*err*})
                   :spit (timbre/spit-appender {:fname logfile})}
        config (assoc log-config
                 :level log-level
                 :appenders appenders)
        debug? (and (#{:trace :debug} log-level) debug-args)
        banner? (or debug? (= :info log-level))
        line-break "\n--------------------------------------------------------------------------------"]
    (timbre/set-config! config)
    (if banner?
      (log/warn "PLANVIZ logging initialized at level" log-level
        (if debug?
          (str line-break "\nargs: " debug-args)
          line-break)))
    ;; Configure plan-schema to user our logging
    (pschema/set-logger! :debug (fn [& msgs]
                                  (log/debug (string/join " " msgs))))
    (pschema/set-logger! :info (fn [& msgs]
                                  (log/debug (string/join " " msgs))))
    (pschema/set-logger! :warn (fn [& msgs]
                                  (log/debug (string/join " " msgs))))
    (pschema/set-logger! :error (fn [& msgs]
                                  (log/debug (string/join " " msgs))))
    (swap! state assoc :logging log-level)))

(defn log-if-possible [msg]
  (if(:logging @state)
    (log/error msg)
    (println msg)))

(def non-websocket-request
  {:status 400
   :headers {"content-type" "application/text"}
   :body "Expected a websocket request."})

(def #^{:dynamic true :added "0.0.0"}
  *remote*
  "current remote for this operation"
  nil)

;; local functions exported to clients --------------------------------------

(defn echo [& args]
  (log/debug "(echo" (apply pr-str args) ")")
  (cond
    (zero? (count args))
    true
    (= 1 (count args))
    (first args)
    :else
    args))

(defn inc-even [v]
  (log/debug "(inc-even" v ")")
  (if (even? v) (inc v) false)) ;; fail on odd

(defn double-positive [v]
  (log/debug "(double-positive" v ")")
  (if (pos? v) (* 2 v) false)) ;; fail on negative

;; better error handling.. like [:error msg]
;; this function returns a pre-baked msg!!!
(defn request-plan-part [plan-id part]
  (log/debug "(request-plan-part" plan-id part ")")
  (let [{:keys [plan n-keys parts n-parts]} (get-in @state [:plans plan-id])]
    (if (and (vector? parts) (not (neg? part)) (< part n-parts))
      [:msg-json (get parts part)]
      false))) ;; error

(defn list-plans []
  (log/debug "(list-plans)")
  (mapv #(vector (first %)
           (dissoc (second %) :plan :parts)) (seq (:plans @state))))

(defn get-url-config []
  (log/debug "(get-url-config)")
  (:url-config @state))

(defn get-client [remote]
  (get-in @state [:clients remote]))

(defn whoami []
  (if *remote*
    (let [{:keys [nick follow]} (get-client *remote*)
          my-nick nick
          {:keys [nick]} (if follow (get-client follow))
          follow (or nick follow)]
      (str "You are " *remote* " nick \"" my-nick "\" following \"" follow "\""))
      ;; (str "You are " *remote* " nick \"" my-nick "\""))
    false))

(defn login []
  (if *remote*
    (let [login {:login/remote *remote*
                 :login/settings (:settings @state)}]
      (log/info "LOGIN" *remote*)
      login)
    false))

;; return remote
(defn get-nick [nickname]
  (let [clients (:clients @state)
        other (first
                (remove nil?
                  (for [[remote client] clients
                        :let [nick (:nick client)]]
                    (if (or (= remote nickname) (= nick nickname))
                      remote))))]
    other))

(defn set-nick [remote nickname]
  (swap! state update-in [:clients remote]
    assoc :nick nickname))

(defn set-follow [remote other]
  (swap! state update-in [:clients remote]
    assoc :follow other))

(defn nick [nickname]
  (if *remote*
    (if (= nickname "-")
      (do
        (set-nick *remote* nil)
        (whoami))
      (if (get-nick nickname)
        {:error (str "Sorry the nick \"" nickname "\" has been taken")}
        (do
          (set-nick *remote* nickname)
          (whoami))))
    false))

(defn follow [nickname]
  (if *remote*
    (if (= nickname "-")
      (do
        (set-follow *remote* nil)
        (whoami))
      (if-let [other (get-nick nickname)]
        (if (= other *remote*)
          {:error (str "You cannot follow yourself!")}
          (do
            (set-follow *remote* other)
            (whoami)))
        {:error (str "Sorry the nick \"" nickname "\" does not exist")}))
    false))

(defn who []
  (vec
    (for [[remote client] (:clients @state)
          :let [nick (or (:nick client) remote)]]
      nick)))

(defn safe-char [ch]
  (if (#{\- \_} ch)
    ch
    (let [c (int ch)]
      (if (or (and (>= c 48) (<= c 57)) ;; digit
            (and (>= c 65) (<= c 90)) ;; upper
            (and (>= c 97) (<= c 122))) ;; lower
        ch
        \_))))

(defn safe-filename [filename]
  (apply str (map safe-char (seq filename))))

(defn make-config-dir []
  (let [cwd (or (:cwd @state) (:planviz-cwd env) (:user-dir env))
        config-dir (str cwd "/config")]
    (when (not (fs/exists? config-dir))
      (log/trace "Making config directory:" config-dir)
      (fs/mkdir config-dir))))

(defn list-settings-filenames []
  (let [cwd (or (:cwd @state) (:planviz-cwd env) (:user-dir env))
        config-dir (fs/file (str cwd "/config"))
        glob (str "*" settings-suffix)]
    (mapv #(string/replace (.getName %) settings-suffix "")
      (fs/glob config-dir glob))))

;; assume filename is safe
(defn save-settings-filename [filename settings]
  (let [cwd (or (:cwd @state) (:planviz-cwd env) (:user-dir env))
        pathname (str cwd "/config/" filename settings-suffix)
        out (with-out-str (pprint (dissoc settings
                                    :settings/filename
                                    :settings/filenames)))
        _ (make-config-dir) ;; ensure /config exists
        _ (spit pathname out)
        filenames (list-settings-filenames)
        settings (assoc settings
                     :settings/filename filename
                     :settings/filenames filenames)]
    (swap! state assoc :settings settings)
    settings))

;; add auto to specify the setting of --auto
(defn load-settings-filename [filename & [auto]]
  (let [cwd (or (:cwd @state) (:planviz-cwd env) (:user-dir env))
        pathname (str cwd "/config/" filename settings-suffix)
        filenames (list-settings-filenames)
        settings (if (fs/exists? pathname)
                   (load-file pathname)
                   (if (= filename "default") planviz-settings))
        settings (if settings
                   (assoc settings
                     :settings/filename filename
                     :settings/filenames filenames
                     :settings/auto (boolean
                                      (or auto
                                        (:settings/auto settings)))))]
    (if settings
      (swap! state assoc :settings settings))
    settings))

(defn load-settings [filename]
  (if *remote*
    (let [settings (load-settings-filename filename)]
      (if settings
        settings
        {:error (str "The settings file named \"" filename "\" is not found")}))
    false))

(defn save-settings [settings]
  (if *remote*
    (let [{:keys [settings/filename]} settings
          filename (safe-filename (or filename "default"))
          settings (save-settings-filename filename settings)]
      settings)
    false))

(declare xmit-to-client)

;; reply with.. to sender and receipient
;; <sender> recipient: comment
(defn msg [other comment]
  (if *remote*
    (let [{:keys [nick]} (get-client *remote*)
          sender (or nick *remote*)
          remote (get-nick other)
          {:keys [nick]} (if remote (get-client remote))
          recipient (or nick remote)
          line (if recipient (str "<" sender "> " recipient ": " comment))]
      (if-not remote
        {:error (str "Sorry the user \"" other "\" does not exist")}
        (do
          (xmit-to-client remote {:rmethod :recv-msg
                                  :args [line]})
          line)))
    false))

(defn get-followers [leader]
  (let [remotes (keys (:clients @state))]
    (loop [followers [] remote (first remotes) more (rest remotes)]
      (if-not remote
        followers
        (let [client (get-client remote)
              {:keys [follow]} client]
          (if (or (= remote leader) (not follow) (not= follow leader))
            (recur followers (first more) (rest more))
            (recur (conj followers remote) (first more) (rest more))))
        ))))

;; expect msg as a clojure form
(defn publish [msg]
  (log/debug "PUBLISH PLANVIZ" msg)
  (let [{:keys [channel exchange]} (get @state :rmq)
        routing-key pamela-visualization-key
        app-id "planviz"
        msg-json (write-json-str msg)]
    (lb/publish channel exchange routing-key msg-json {:app-id "planviz"})))

(defn user-action [action]
  (if *remote*
    (let [{:keys [nick]} (get-client *remote*)
          {:keys [host port]} @state
          planviz (str host ":" port)
          nick-action (assoc-if action
                        :remote *remote*
                        :nick nick
                        :planviz planviz
                        )]
          (publish nick-action)
      true)
    false))

(declare broadcast-clients)

(defn broadcast-user-action [action]
  (try
    (let [{:keys [host port]} @state
          server (str host ":" port)
          {:keys [planviz remote nick type plid selection pan zoom chat
                  tag node edge url tab info network-flows]} action
          planviz (or planviz server)
          remote (or remote server)
          type (if type (keyword type))
          plid (if plid (keyword plid))
          tag (if tag (keyword tag))
          node (if node (keyword node))
          edge (if edge (keyword edge))
          selection (if selection
                      (doall (mapv #(vector (keyword (first %)) (keyword (second %)))
                               selection)))
          action (assoc-if action
                   :planviz planviz :remote remote :nick nick
                   :type type :plid plid
                   :selection selection
                   :tag tag :node node :edge edge)]
      (broadcast-clients nil {:rmethod :user-action
                              :args [action]}))
    (catch Exception e
      (log/error "broadcast-user-action ERROR:" (.. e getCause getMessage)))))

(defn broadcast-chat [msg]
  (let [{:keys [host port]} @state
        server (str host ":" port)
        action {:type :chat
                :planviz server
                :remote server
                ;; :nick "server"
                :chat msg}]
    (broadcast-clients nil {:rmethod :user-action
                            :args [action]})))

(defn broadcast-add-plan [plan-id plan-details]
  (broadcast-clients nil {:rmethod :add-plan
                          :args [plan-id plan-details]}))

;; these are server methods
(def rmethods {:echo echo
               :inc-even inc-even
               :double-positive double-positive
               :list-plans list-plans
               :request-plan-part request-plan-part
               :whoami whoami
               :who who
               :nick nick
               :follow follow
               :msg msg
               :user-action user-action
               :login login
               :get-url-config get-url-config
               :save-settings save-settings
               :load-settings load-settings
               :list-settings-filenames list-settings-filenames
               })

;; server functions -------------------------------------------

(defn client-count []
  (log/info "num clients now" (count (:clients @state))))

(defn get-msgs []
  (get-in @state [:msgs]))

(defn get-client-keys []
  (let [{:keys [clients]} @state
        remotes (keys clients)]
    remotes))

(defn client0 []
  (first (get-client-keys)))

(defn get-rmethod [rmethod]
  (get-in @state [:rmethods rmethod]))

(defn add-client [conn]
  (let [desc (s/description conn)
        remote (get-in desc [:source :connection :remote-address])]
    (log/debug "REMOTE:" remote "DESC:" (with-out-str (pprint desc)))
    (swap! state update-in [:clients remote]
      assoc :conn conn :nick nil :follow nil)
    (client-count)
    remote))

(defn remove-client [remote]
  (swap! state
    (fn [st]
      (let [clients (:clients st)
            client (get clients remote)
            {:keys [conn]} client
            error (transit-to-json {:error {:code 1001}})
            clients (if client (dissoc clients remote) clients)]
        (when (and conn (not (s/closed? conn)))
          (log/debug "sending error to figwheel for " remote)
          (s/put! conn error)
          (s/close! conn))
        (assoc st :clients clients))))
  (client-count))

(def heartbeat-period (* 60 1000))

(defn heartbeat []
  (let [{:keys [clients]} @state
        remotes (keys clients)]
    ;; (log/debug "HEARTBEAT")
    (when (pos? (count remotes))
      (doseq [remote remotes]
        (let [client (get-client remote)
              {:keys [conn]} client]
          (when (or (nil? conn) (s/closed? conn))
            (log/debug "client disappeared:" remote)
            (remove-client remote)))))))

(defn heartbeat-start []
  (let [heartbeat-cancel (:heartbeat-cancel @state)]
    (when (fn? heartbeat-cancel)
      (log/debug "stopping heartbeat for restart")
      (heartbeat-cancel))
    (log/debug "starting heartbeat")
    ;; NOTE every returns a cancel-fn, (cancel-fn) to stop
    (swap! state assoc :heartbeat-cancel
      (dtime/every heartbeat-period heartbeat))))

(defn heartbeat-stop []
  (let [heartbeat-cancel (:heartbeat-cancel @state)]
    (when (fn? heartbeat-cancel)
      (log/debug "stopping heartbeat")
      (heartbeat-cancel)
      (swap! state assoc :heartbeat-cancel nil))))

(defn xmit-to-client [remote msg & [msg-json]]
  (let [client (get-client remote)
        {:keys [conn]} client
        {:keys [message error]} msg
        message (if-not msg-json
                  (if (or message error)
                    msg
                    {:message msg}))
        msg-json (or msg-json (transit-to-json message))]
    (if (or (nil? conn) (s/closed? conn))
      (remove-client remote)
      (do
        ;; (log/debug "xmit-to-client" remote "MESSAGE" msg)
        (s/put! conn msg-json)))
    msg-json))

(defn xmit0 [msg]
  (xmit-to-client (client0) msg))

;; call a method on the client
;; return is the deferred
;; rmethod is the function name keyword
(defn rmethod [remote return rmethod & args]
  (let [remote (or remote (client0))
        message (assoc-if {:rmethod rmethod :args args}
                  :return return)]
    (xmit-to-client remote message)
    return))

(defn success! [d v]
  (log/debug "success!" d v))

(defn error! [d v]
  (log/debug "error!" d v))

;; client calls method on server
(defn invoke-rmethod [remote return rmethod args]
  (if (= rmethod :success-fn)
    (apply success! args)
    (if (= rmethod :error-fn)
      (apply error! args)
      (binding [*remote* remote]
        (let [rmethod-fn (get-rmethod rmethod)
              rv (if (fn? rmethod-fn)
                   (apply rmethod-fn args)
                   :invalid-method)
              msg-json (if (and (vector? rv) (= (first rv) :msg-json))
                         (second rv))
              result (if (or (not rv) (= rv :invalid-method))
                       :error-fn
                       :success-fn)
              msg (if-not msg-json
                    {:rmethod result :args [return rv]})]
          (log/debug "invoke-rmethod" remote return rmethod
            (apply pr-str args) "->" result "=" rv)
          (when return ;; client has a deferred
            (xmit-to-client remote msg msg-json)))))))

;; we might have pre-determined the outgoing message in msg-json
;; if not, we'll use it from the first client and send that
;; to the subsequent clients
(defn broadcast-clients [from msg & [msg-json]]
  (let [remotes (get-client-keys)]
    ;; (log/debug "BROADCAST" new-message)
    (loop [msg-json msg-json r (first remotes) more (rest remotes)]
      (let [mj (xmit-to-client r msg msg-json)]
        (if-not (empty? more)
          (recur mj (first more) (rest more)))))))

;; (defn network-update [update]
;;   (broadcast-clients nil {:rmethod :network-update :args [update]}))

(defn new-network-notification [network]
  (broadcast-clients nil {:rmethod :new-network-notification :args [network]}))

(defn recv-from-client [remote msg-json]
  (let [{:keys [message error]} (transit-from-json msg-json)]
    ;; (log/debug "msg from" remote)
    (if error
      (do
        (log/debug "CLIENT" remote "ERROR:" error)
        (remove-client remote))
      (let [{:keys [rmethod args return]} message]
        (log/debug "CLIENT" remote "MESSAGE:" message)
        (if rmethod
          (invoke-rmethod remote return rmethod args)
          (broadcast-clients remote message))))))

(defn ws-handler
  [req]
  ;; (let [remote-addr (:remote-addr req)

  ;;       remote (or figremote remote-addr)]
  ;;   (log/debug (str "remote: " remote " figremote? " (not (nil? figremote))))
  (do
    (-> (http/websocket-connection req)
      (d/chain
        (fn [conn]
          (log/debug "NEW /ws REQ:" (with-out-str (pprint req)))
          (let [remote (add-client conn)]
            (s/consume #(recv-from-client remote %)
              (s/buffer 64 conn)))))
      (d/catch
          (fn [_]
            non-websocket-request)))))

(defn web-html [& [req]]
  ;; (if (production?)
  ;;   (web/create-html req)
  ;;   (web/create-dev-html req)))
  (web/create-dev-html req))

(def web-html-memo (memoize web-html))

(defroutes routes
  (GET "/ws" [] ws-handler)
  (GET "/" req web-html-memo)
  (ANY "*" []
    (not-found (slurp (io/resource "404.html")))))

(defn log-request-handler [handler]
  (fn [req]
    (log/debug "REQ" (:uri req))
    (handler req)))

(defn debug-handler [handler step]
  (fn [req]
    ;; (log/debug "DEBUG HANDLER BEFORE" step)
    (try
      (let [rv (handler req)]
      ;; (log/debug "DEBUG HANDLER AFTER" step)
        rv)
      (catch IllegalArgumentException e
        (log/debug "BROWSER CLOSED")
        {}))))

;; https://github.com/ring-clojure/ring-defaults
;; When (false? not-modified?) the following will help ensure the most recent
;; ClojureScript are re-sent to the browser.
(defn http-handler []
  (let [not-modified? (if (#{:trace :debug} (:logging @state))
                        false true)
        defaults (-> site-defaults
                   (dissoc :security)
                   (assoc-in [:responses :not-modified-responses]
                     not-modified?))]
    (-> routes
      (debug-handler :routes)
      (log-request-handler)
      ;; (debug-handler :log-request-handler)
      (wrap-defaults defaults)
      ;; (debug-handler :wrap-defaults)
      ;; (wrap-cors
      ;;   :access-control-allow-origin #"localhost:*"
      ;;   :access-control-allow-methods [:get]
      ;;   :access-control-allow-headers ["Origin" "X-Requested-With"
      ;;                                  "Content-Type" "Accept"])
      (wrap-gzip)
      ;; (debug-handler :wrap-gzip)
      )))

;; TPN functions
;; NOTE: assumes only one active network for now

;; (defn get-end-node []
;;   (get-in @state [:networks "rmq-tpn" :end-node]))

;; (defn update-object-state [uid tpn-object-state]

;;   ;; do NOT attempt to update local state --> just send updates to clients
;;   ;; in any case the parts would become obosolete
;;   ;; (swap! state assoc-in [:networks "rmq-tpn" :objects uid :tpn-object-state]
;;   ;;   tpn-object-state)
;;   (network-update {:plid (:rmq-plan-id @state) :update-uid uid :state tpn-object-state})
;;   )
;;   ;; (if (and (= tpn-object-state :reached) (= uid (get-end-node)))
;;   ;;   (log/debug "end-node reached:" uid "\n+++++++++++++++++++++++++++")))

;; returns the path of the planviz tmpdir or false if unable to create
(defn user-tmpdir [program]
  (let [tmp (:java-io-tmpdir env)
        user (:user env)
        tmpdir (str tmp "/" user "/" program)]
    (if (fs/exists? tmpdir)
      tmpdir
      (do
        (fs/mkdirs tmpdir)
        tmpdir))))

;; return plan-id if one found with begin
(defn find-plan [begin]
  (let [plans (:plans @state)
        plan-ids (keys plans)
        plan-begins (map #(vector % (get-in plans [% :begin])) plan-ids)]
    (ffirst
      (remove nil?
        (filter #(= begin (second %)) plan-begins)))))

(defn remove-plan [plan-id]
  (swap! state update-in [:plans] dissoc plan-id)
  true) ;; don't return the current value of @state

(defn rename-key [plan-id plid-thing]
  (let [thing (keyword (second (string/split (name plid-thing) #":")))]
    (composite-key plan-id thing)))

(defn rename-plan [plan-id by-plid]
  (let [{:keys [plan/type plan/begin plan/networks]} (first (vals by-plid))]
    {plan-id
     {:plan/plid plan-id
      :plan/name (name plan-id)
      :plan/type type
      :plan/begin (rename-key plan-id begin)
      :plan/networks (doall (mapv (partial rename-key plan-id) networks))}}))

(defn rename-networks [plan-id network-by-plid-id]
  (let [kvs (seq network-by-plid-id)]
    (loop [nbpi {} kv (first kvs) more (rest kvs)]
      (if-not kv
        nbpi
        (let [[k v] kv
              k (rename-key plan-id k)
              {:keys [network/id network/type network/begin network/end
                      network/nodes network/edges]} v
              v {:plan/plid plan-id
                 :network/id id
                 :network/type type
                 :network/begin (rename-key plan-id begin)
                 :network/end (rename-key plan-id end)
                 :network/nodes
                 (doall (mapv (partial rename-key plan-id) nodes))
                 :network/edges
                 (doall (mapv (partial rename-key plan-id) edges))}]
          (recur (assoc nbpi k v) (first more) (rest more))
          )))))

(defn rename-nodes [plan-id node-by-plid-id]
  (let [kvs (seq node-by-plid-id)]
    (loop [nbpi {} kv (first kvs) more (rest kvs)]
      (if-not kv
        nbpi
        (let [[k v] kv
              k (rename-key plan-id k)
              {:keys [node/parent node/end]} v
              v (assoc-if v
                  :plan/plid plan-id
                  :node/parent (if parent (rename-key plan-id parent))
                  :node/end (if end (rename-key plan-id end)))]
          (recur (assoc nbpi k v) (first more) (rest more))
          )))))

(defn rename-edges [plan-id edge-by-plid-id]
  (let [kvs (seq edge-by-plid-id)]
    (loop [ebpi {} kv (first kvs) more (rest kvs)]
      (if-not kv
        ebpi
        (let [[k v] kv
              k (rename-key plan-id k)
              {:keys [edge/from edge/to]} v
              v (assoc-if v
                  :plan/plid plan-id
                  :edge/from (rename-key plan-id from)
                  :edge/to (rename-key plan-id to))]
          (recur (assoc ebpi k v) (first more) (rest more))
          )))))

(def message-max (int (* 65536 0.9)))
(def size-per-key 192) ;; emperical
(def keys-per-msg (quot message-max size-per-key))

(defn create-plan-parts [plan-id plan return]
  (let [plan-parts (partition-map keys-per-msg plan)
        n-parts (count plan-parts)]
    (loop [i 0 parts []]
      (if (= i n-parts)
        parts
        (let [rv {:plan-id plan-id :part (get plan-parts i) :n i}
              msg {:rmethod :success-fn :args [return rv]}
              message {:message msg}
              msg-json (transit-to-json message)]
          (recur (inc i) (conj parts msg-json)))))))

;; redefine plan name as :rmq-BEGIN where begin is the first
;; network of the plan
(defn load-rmq-plan [rmq-plan old-plan-id begin]
  (log/debug "START LOAD-RMQ-PLAN")
  (let [plan-id (keyword (str "rmq-" (name begin)))
        {:keys [plan/by-plid network/network-by-plid-id
                node/node-by-plid-id edge/edge-by-plid-id]} rmq-plan
        by-plid (rename-plan plan-id by-plid)
        network-by-plid-id (rename-networks plan-id network-by-plid-id)
        node-by-plid-id (rename-nodes plan-id node-by-plid-id)
        edge-by-plid-id (rename-edges plan-id edge-by-plid-id)
        merged-plan (merge by-plid network-by-plid-id
                      node-by-plid-id edge-by-plid-id)
        n-keys (count (keys merged-plan))
        n-parts (inc (quot n-keys keys-per-msg))
        return :deferring-request-plan-part
        parts (create-plan-parts plan-id merged-plan return)
        plan-details {:plan merged-plan
                      :begin begin
                      :n-keys n-keys
                      :parts parts
                      :n-parts n-parts
                      :type :tpn-network
                      ;; NOTE: we do NOT have a corresponding HTN!
                      }]
    (log/info "NEW RMQ PLAN, switch plan-id to" plan-id)
    (swap! state assoc :rmq-plan-id plan-id) ;; most recent rmq plan
    (swap! state assoc-in [:plans plan-id] plan-details)
    (broadcast-add-plan plan-id (dissoc plan-details :plan :parts))))

;; WebSockets generally do not like messages bigger than 64k bytes
;; We'll define message-max to be 90% of that
(defn new-rmq-tpn [json-str]
  (if-let [tmpdir (user-tmpdir "planviz")]
    (let [rmq-tpn-json (str tmpdir "/rmq.tpn.json")
          _ (spit rmq-tpn-json json-str)
          tpn (pschema/tpn-plan {:input [rmq-tpn-json]})
          error (:error tpn)]
      (if error
        (do
          (log/error "Received invalid TPN over RMQ" rmq-tpn-json)
          (log/error "ERROR" error)
          (broadcast-chat "Received invalid TPN over RMQ"))
        (let [plan-id (first (:plans/plans tpn))
              plan-begin (get-in tpn [:plan/by-plid plan-id :plan/begin])
              begin (keyword (second (string/split (name plan-begin) #":")))
              existing-plan-id (find-plan begin)]
          (log/debug "NEW RMQ begin" begin "existing-plan-id" existing-plan-id)
          (if existing-plan-id
            (let [plan-details (get-in @state [:plans existing-plan-id])]
              (log/debug "Updating TPN over RMQ" existing-plan-id)
              ;; (log/debug (with-out-str (pprint tpn)))
              ;; most recent rmq plan
              (swap! state assoc :rmq-plan-id existing-plan-id)
              (broadcast-add-plan existing-plan-id
                (dissoc plan-details :plan :parts)))
            (load-rmq-plan tpn plan-id begin)))))))

(defn network-reset []
  (let [plan-id (:rmq-plan-id @state)]
    (when plan-id
      (broadcast-clients nil {:rmethod :network-reset
                              :args [plan-id]}))))

(defn tpn-object-update [json-str]
  (let [m (pschema/read-json-str json-str)
        plid (:rmq-plan-id @state)
        os->nu (fn [os] ;; object-state -> network-update
                 (let [{:keys [uid tpn-object-state]} os]
                   {:plid plid :update-uid (keyword uid)
                    :state (keyword tpn-object-state)}))
        updates (mapv os->nu (filter map? (vals m)))]
    (log/debug "UPDATES" updates)
    (broadcast-clients nil {:rmethod :network-updates :args [updates]})))

(defn unknown-update [routing-key json-str]
  (log/info "UNKNOWN routing-key" routing-key
    "VALUE" (with-out-str (pprint json-str))))

(defn wants-updates-from [r remote]
  (let [{:keys [follow]} (get-client r)]
    (or (nil? follow) (= follow remote))))

;; was (broadcast-user-action action)
;; NOW only send to those users which are not following anyone
;; OR are following THIS user
(defn planviz-update [json-str]
  (let [action (read-json-str json-str)
        _ (log/info "PLANVIZ\n" (with-out-str (pprint action)))
        {:keys [host port]} @state
        server (str host ":" port)
        {:keys [planviz remote nick type plid selection pan zoom chat
                tag node edge url tab info network-flows]} action
        planviz (or planviz server)
        remote (or remote server)
        type (if type (keyword type))
        plid (if plid (keyword plid))
        selection (if selection
                    (doall (mapv #(vector (keyword (first %))
                                    (keyword (second %)))
                             selection)))
        tag (if tag (keyword tag))
        node (if node (keyword node))
        edge (if edge (keyword edge))
        action (assoc-if action
                 :planviz planviz :remote remote :nick nick
                 :type type :plid plid
                 :selection selection
                 :tag tag :node node :edge edge)
        msg {:rmethod :user-action :args [action]}
        remotes (get-client-keys)]
    (loop [msg-json nil r (first remotes) more (rest remotes)]
      (let [mj (if (wants-updates-from r remote)
                 (xmit-to-client r msg msg-json))]
        (if-not (empty? more)
          (recur (or msg-json mj) (first more) (rest more)))))))

;; ensure serialized messages from RMQ --------------------------

;; here we know that incoming-msg will be called serially
(defn incoming-msg [msg]
  (let [[metadata json-str] msg
        {:keys [exchange routing-key app-id]} metadata
        details (str "MSG from exchange: " exchange " routing-key: " routing-key
                  " app-id: " app-id)]
    (log/info details)
    (log/trace (str \newline (with-out-str (clojure.pprint/pprint json-str))))
    (condp = routing-key ;; case does not work with a symbol below
      "network.new" (new-rmq-tpn json-str)
      "network.reset" (network-reset) ;;(tpn-object-update json-str)
      "tpn.object.update" (tpn-object-update json-str)
      "tpn.activity.active" (tpn-object-update json-str)
      "tpn.activity.finished" (tpn-object-update json-str)
      "tpn.activity.negotiation" (tpn-object-update json-str)
      pamela-visualization-key (planviz-update json-str)
      (unknown-update routing-key json-str))))

;; This is the message processing loop
(defn start-msgs [port log-level]
  (if (get-msgs)
    (log/debug "msgs already running!")
    (let [msgs (chan 10)]
      (swap! state assoc :msgs msgs)
      (if-not (:logging @state)
        (log-initialize port log-level))
      (log/info "msgs started")
      (go-loop [msg (<! msgs)]
        (if-not msg
          (log/info "msgs stopped")
          (do
            (incoming-msg msg)
            (recur (<! msgs))))))))

;; stop the message processing loop
(defn stop-msgs []
  (if-not (get-msgs)
    (log/debug "msgs not running!")
    (do
      (close! (get-msgs))
      (swap! state assoc :msgs nil))))

;; publish a message to handle (serially)
(defn push-msg [msg]
  (if-not (get-msgs)
    (log/debug "msgs not running!")
    (put! (get-msgs) msg)))

;; RabbitMQ -------------------------------------------

(defn incoming-msgs [_ metadata ^bytes payload]
  (push-msg [metadata (String. payload "UTF-8")])
  true) ;; FIXME

(defn running? []
  (not (nil? (:server @state))))

(defn shutdown []
  (when (get-in @state [:rmq :connection])
    (rmq/close (get-in @state [:rmq :channel]))
    (rmq/close (get-in @state [:rmq :connection]))
    (swap! state update-in [:rmq] assoc :connection nil :channel nil)
    (log/info "RMQ connection shutdown"))
  (stop-msgs)
  (if (running?)
    (let [{:keys [server clients]} @state]
      (log/info "Stopping web server")
      (heartbeat-stop)
      (doseq [r (keys clients)]
        (remove-client r))
      (.close server)
      (log/warn "PLANVIZ server stopped"))
    (log/warn "PLANVIZ server already stopped")))


;; plan input is in normalized (validated) format
;; convert to a flat map and then chunk it
;; everyone has :plan/plid
;; :network/id
;; :edge/id
;; :node/id
(defn load-plan [plan]
  (let [{:keys [plan/by-plid network/network-by-plid-id
                node/node-by-plid-id edge/edge-by-plid-id]} plan
        plan-id (first (keys by-plid))
        plan (get by-plid plan-id)
        {:keys [plan/begin plan/type plan/corresponding]} plan
        plan-begin begin]
    (if-not plan-begin
      (log/error "LOAD-PLAN failed for plan" plan)
      (let [begin (keyword (second (string/split (name plan-begin) #":")))
            merged-plan (merge by-plid network-by-plid-id
                          node-by-plid-id edge-by-plid-id)
            n-keys (count (keys merged-plan))
            n-parts (inc (quot n-keys keys-per-msg))
            return :deferring-request-plan-part
            parts (create-plan-parts plan-id merged-plan return)]
        (log/info "LOAD-PLAN" plan-id)
        (swap! state assoc-in [:plans plan-id]
          {:plan merged-plan
           :begin begin
           :n-keys n-keys
           :parts parts
           :n-parts n-parts
           :type type
           :corresponding corresponding})))))

;; returns true on success
(defn load-input [filename cwd]
  (cond
    (= filename "-") ;; ignore
    true
    (string/index-of filename "=")
    (let [[htn-name tpn-name] (string/split filename #"=")
          htn-filename (if (and htn-name (pschema/htn-filename? htn-name))
                         htn-name
                         (if (and tpn-name (pschema/htn-filename? tpn-name))
                           tpn-name))
          tpn-filename (if (and tpn-name (pschema/tpn-filename? tpn-name))
                         tpn-name
                         (if (and htn-name (pschema/tpn-filename? htn-name))
                           htn-name))
          plans (pschema/merge-networks
                  {:input [htn-filename tpn-filename] :cwd cwd})]
      (cond
        (and (map? plans) (:error plans))
        (let [msg (str "error parsing plans: " htn-filename " = " tpn-filename)]
          (log/error msg)
          (log/error (:error plans))
          false)
        (and (vector? plans) (= 2 (count plans)))
        (do
          (log/info "plans parsed correctly for:" htn-filename
            " = " tpn-filename)
          (doall (map load-plan plans))
          true)
        :else
        (let [msg (str "ERROR parsing: " htn-filename " = " tpn-filename)]
          (log/error msg)
          ;; (log/error "PLANS" plans)
          false)))
    (pschema/htn-filename? filename)
    (let [plan (pschema/htn-plan {:input [filename] :cwd cwd})]
      (if (and (map? plan) (:error plan))
        (let [msg (str "error parsing HTN plan: " filename)]
          (log/error msg)
          (log/error (:error plan))
          false)
        (do
          (log/info "HTN plan parsed correctly:" filename)
          (load-plan plan)
          true)))
    (pschema/tpn-filename? filename)
    (let [plan (pschema/tpn-plan {:input [filename] :cwd cwd})]
      ;; (log/error "RV plan" plan)
      (if (and (map? plan) (:error plan))
        (let [msg (str "error parsing TPN plan: " filename)]
          (log/error msg)
          (log/error (:error plan))
          false)
        (do
          (log/info "TPN plan parsed correctly:" filename)
          (load-plan plan)
          true)))
    :else
    (let [msg (str "Invalid filename, does not contain tpn nor htn: " filename)]
      (log/error msg)
      false)))

(defn connect-to-rmq [host port]
  (try
    (rmq/connect {:host host :port port})
    (catch Exception e
      (log/error "RabbitMQ connection error:" e))))

;; input is a vector of files to read
;; {TPN|HTN|HTN=TPN}
(defn startup [host port input cwd log-level]
  (if-not (get-msgs)
    (start-msgs port log-level)) ;; lazily does log-initialize
  (if (get-in @state [:rmq :connection])
    (shutdown))
  (let [{:keys [rmq-host rmq-port exchange]} (:rmq @state)
        connection (connect-to-rmq rmq-host rmq-port)]
    (if-not connection
      (log/error "PLANVIZ cannot connect to RabbitMQ at" rmq-host ":" rmq-port)
      (let [channel (lch/open connection)
            _ (le/declare channel exchange "topic")
            queue (lq/declare channel)
            qname (.getQueue queue)
            ;; FIXME convert host into Aleph :socket-address
            ;; a `java.net.SocketAddress` specifying both the port and
            ;; interface to bind to.
            ;; server (http/start-server http-handler
            ;;          {:shutdown-executor? false :port port})
            server (http/start-server (http-handler) {:port port})]
        (lq/bind channel qname exchange {:routing-key "#"})
        (lc/subscribe channel qname incoming-msgs {:auto-ack true})
        (swap! state update-in [:rmq]
          assoc :connection connection :channel channel)
        (log/info (str "RMQ host: " rmq-host " port: " rmq-port
                    " exchange: " exchange))
        (heartbeat-start)
        (swap! state assoc :server server
          :host host :port port :rmethods rmethods)
        (if-not (reduce and-fn true (for [i input] (load-input i cwd)))
          (log/info "PLANVIZ not starting due to errors!")
          (do
            (log/warn "\nPLANVIZ server ready")
            (when (and (not (repl?)) (get-in @state [:rmq :connection]))
              (while true
                ;; (print ".")
                ;; (flush)
                (sleep 10)))))))))

(defn- compare-url-config [a b]
  (let [{:keys [plan node edge]} a
        a-plan (or plan :*)
        a-node (or node :*)
        a-edge (or edge :*)
        {:keys [plan node edge]} b
        b-plan (or plan :*)
        b-node (or node :*)
        b-edge (or edge :*)
        c-plans (compare a-plan b-plan)
        c-nodes (compare a-node b-node)
        c-edges (compare a-edge b-edge)]
    (if (zero? c-plans)
      (if (zero? c-edges)
        c-nodes
        c-edges)
      c-plans)))

;; take in a raw vector url-config and return an unique, sorted vector
(defn sort-url-config [url-config]
  (vec (sort compare-url-config (set url-config))))

(defn load-default-settings [cwd]
  (let [filename "default"
        config-dir (str cwd "/config")
        path (str config-dir "/" filename settings-suffix)]
    (if (fs/exists? path)
      (log/error "LOAD-DEFAULT-SETTINGS\n"
        (with-out-str (pprint (load-file path))))))
  {:auto false}) ;; FIXME

(defn setup-url-config [cwd url-config]
  (doseq [url-filename url-config]
    (let [url-filename (pschema/validate-input url-filename cwd)]
      (if (map? url-filename)
        (log/error (:error url-filename))
        (let [urls (load-file url-filename)]
          (when (vector? urls)
            (swap! state update-in [:url-config] concatv urls))))))
  (swap! state update-in [:url-config] sort-url-config))

;; main program --------------------------------------------------

(defn visualize
  "Visualize HTN and TPN plans"
  {:added "0.8.0"}
  [options]
  (let [{:keys [cwd verbose log-level auto exchange rmq-host rmq-port
                host port input url-config strict settings]} options
        _ (swap! state assoc-in [:cwd] cwd)
        settings (load-settings-filename (or settings "default") auto)
        exchange (or exchange rmq-default-exchange)
        rmq-host (or rmq-host rmq-default-host)
        rmq-port (or rmq-port rmq-default-port)
        host (or host planviz-default-host)
        port (or port planviz-default-port)]
    (pschema/set-strict! strict)
    (if-not (net/port-available? port)
      (let [msg (str "The port " port " is in use cannot be bound by PLANVIZ")]
        (log-if-possible msg))
      (do
        (setup-url-config cwd url-config)
        (swap! state update-in [:rmq]
          assoc :rmq-host rmq-host :rmq-port rmq-port :exchange exchange)
        (startup host port input cwd log-level))))) ;; lazily does start-msgs
