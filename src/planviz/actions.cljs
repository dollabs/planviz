;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns planviz.actions
  "planviz user actions"
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [cljs.pprint :refer [pprint]]
            [avenir.utils :as au :refer [assoc-if concatv as-boolean as-int
                                         as-float remove-fn vec-index-of]]
            [avenir.math :as am :refer [log2]]
            [webtasks.tasks :as tasks
             :refer [on-realized chain sleep success! error!]]
            [webtasks.ws :as ws]
            [webkeys.keys :as keys]
            [planviz.ui :as ui :refer [node-key-fn edge-key-fn network-key-fn
                                       activity-type? begin?
                                       edge-states node-states
                                       parallel-edge-states
                                       choice-edge-states]]
            [planviz.state :as st]
            [planviz.tplan :as tplan :refer [min-max-fn]]
            [plan-schema.core :as pschema :refer [composite-key]]
            [goog.dom :as gdom]
            [goog.crypt.base64 :as base64]
            [goog.net.XhrIo :as xhr]))

;; web url ----------------------------------------------------

(defonce aggregated-edges (atom {}))

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

;; debug-port is a vector of [debug-from debug-to], both integers
;; which will set the port to debug-to if the current port is debug-from
;; AND port is not explicitly set in the options
(defn make-url [& opts]
  (let [opts (if opts (apply hash-map opts))
        {:keys [protocol hostname port uri secure debug-port]} opts
        location (.-location js/document)
        p (let [p (.-protocol location)] (subs p 0 (dec (count p)))) ;; no ":"
        secure (or secure (get secure-protocols p false))
        protocol (or protocol p)
        protocol (protocol-secure protocol secure)
        hostname (or hostname (.-hostname location))
        [debug-from debug-to] debug-port
        default-port (as-int (.-port location))
        default-port (if (= default-port debug-from) debug-to default-port)
        port (str (or port default-port))
        server (str hostname (if-not (empty? port) (str ":" port)))
        uri (or uri "/")
        url (str protocol "://" server uri)]
    url))

(defn rmethod [{:keys [return success-fn error-fn] :as opts} method & args]
  (let [return (or return (tasks/deferred))
        success-fn (or success-fn
                     #(println "  ANSWER for" method "->"  %))
        error-fn (or error-fn
                   #(println "  ERROR for" method "->"  %))]
    ;; (println "RMETHOD" return method args) ;; DEBUG
    (tasks/on-realized return success-fn error-fn)
    (apply ws/rmethod ws/ws return method args)
    return))

(defn chain-rmethod [d method & args]
  (let [drmethod (tasks/deferred)]
    (on-realized d
      #(apply ws/rmethod ws/ws drmethod method args)
      #(error! drmethod %))
    drmethod))

(defn echo [& args]
  (apply rmethod {} :echo args))

(defn get-end-node-ref [begin-id]
  (let [node (st/plans-get-node begin-id)
        {:keys [node/end]} node]
    [:node/node-by-plid-id end]))

(defn get-begin-node-ref [end-id]
  (let [node (st/plans-get-node end-id)
        {:keys [node/begin]} node]
    [:node/node-by-plid-id begin]))

(defn normalize-tpn-selection [node]
  (let [tpn-selection (:node/tpn-selection node)]
    (loop [node (assoc node :node/tpn-selection [])
           sel (first tpn-selection) more (rest tpn-selection)]
      (if-not sel
        node
        (let [[type id] sel
              sel (if (keyword-identical? type :node)
                    (get-end-node-ref id)
                    [:edge/edge-by-plid-id id])
              tpn-selection (conj (:node/tpn-selection node) sel)
              node (assoc node :node/tpn-selection tpn-selection)]
          (recur node (first more) (rest more)))))))

(defn normalize-plan [plan-id plan]
  (let [p (atom {:plans/plans [plan-id]
                 :plan/by-plid {}
                 :network/network-by-plid-id {}
                 :edge/edge-by-plid-id {}
                 :node/node-by-plid-id {}})]
    (doseq [[k v] (seq plan)]
      (cond
        (:node/id v)
        (let [v (if (:node/tpn-selection v)
                  (let [ts (normalize-tpn-selection v)]
                    ;; (println "DEBUG" (:node/id v) "TS" ts)
                    ts)
                  v)]
          (swap! p assoc-in [:node/node-by-plid-id (node-key-fn v)] v))
        (:edge/id v)
        (swap! p assoc-in [:edge/edge-by-plid-id (edge-key-fn v)] v)
        (:network/id v)
        (swap! p assoc-in [:network/network-by-plid-id (network-key-fn v)] v)
        :else ;; plan
        (swap! p assoc-in [:plan/by-plid plan-id] v)
        ))
    @p))

;; true is a special case for chained deferreds
(defn status-msg [& args]
  ;; (println "STATUS-MSG" args)
  (let [args (if (and (= 1 (count args)) (true? (first args)))
               "" args)
        msg-new (apply str (interpose " " args))]
    (st/app-set-message msg-new)
    (println msg-new) ;; DEBUG
    true)) ;; for deferreds

(defn my-user-action [action]
  (let [action-type (:type action)
        chat? (keyword-identical? action-type :chat)
        plid (or (:plid action) (:ui/show-plan (st/get-ui-opts)))
        action (if chat? action (assoc action :plid plid))]
    (when (or chat? (not= plid :none))
      (println "MY USER-ACTION" action)
      ;; (rmethod {:success-fn generic-reply :error-fn generic-reply}
      (rmethod {}
        :user-action action))))

(defn update-tpn-end [plid]
  (let [end (if (keyword-identical? (st/app-get-plan-value plid :type) :tpn-network)
              (st/get-network-end plid))]
    (when end
      ;; (println "TPN-END" plid end)
      (st/app-set-plan-value plid :tpn-end end))
    true)) ;; for deferred

(defn new-plan [plan]
  (let [plan-id (first (keys (:plan/by-plid plan)))
        start (tasks/deferred)
        finish (-> start
                 (sleep 100)
                 (chain tplan/layout)
                 (sleep 100)
                 (chain st/new-plan)
                 (sleep 100)
                 (chain update-tpn-end)
                 (sleep 200))]
    (success! start plan)
    finish))

(defn network-reset [plid]
  (println "NETWORK-RESET" plid)
  (let [{:keys [loaded? corresponding]} (st/app-get-plan plid)]
    (if-not loaded?
      (if (st/app-get :app/loading)
        (tasks/timeout #(network-reset plid) 500) ;; try again
        (println "ERROR network-reset for not loaded plan:" plid))
      (do
        (st/all-normal plid)
        (if corresponding (st/all-normal corresponding))))))

(declare set-aggregated?)

(defn network-updates [updates]
  (let [d (st/app-get :app/loading)
        defer (or (st/app-get :app/defer) [])]
    (println "NETWORK-UPDATES" (count updates) "d" d)
    (if d
      (st/app-set :app/defer (concatv defer updates))
      (let [{:keys [plid update-uid state]} (last updates)
            plans (st/app-get :app/plans)
            plan-data (if plid (get plans plid))
            {:keys [tpn-end]} plan-data]
        (st/network-updates updates)
        (when (and (keyword-identical? state :reached)
                (keyword-identical? (composite-key plid update-uid) tpn-end))
          (status-msg "finished" plid))
        ;; recalculate aggregated edge states
        (doseq [aggregated-id (keys @aggregated-edges)]
          (let [{:keys [begin-type begin end]}
                (get @aggregated-edges aggregated-id)
                aggregated (st/plans-get-edge aggregated-id)
                rollup (set-aggregated? begin-type begin end
                         :network-updates)]
            (when-not (keyword-identical? (:edge/state aggregated) rollup)
              ;; (println "aggregated" aggregated-id "state change" rollup)
              (st/plans-merge-edge
                (assoc (dissoc aggregated :plans/ui-opts :edge/from :edge/to)
                  :edge/state rollup)))))))))

(defn apply-network-updates [success?]
  (let [defer (or (st/app-get :app/defer) [])]
    (when (and success? (pos? (count defer)))
      (println "APPLYING" (count defer) "deferred network updates...")
      ;; (doseq [update defer]
      ;;   (network-update update))
      (network-updates defer)
      )
    (st/app-set :app/defer [])))

(def d-request-plan-part (tasks/deferred-persistent "request-plan-part"))

(declare request-plan-part)

(defn new-plan-part [new-part]
  (let [d (st/app-get :app/loading)
        {:keys [plan-id n part]} new-part
        plans (st/app-get :app/plans)
        plan-data (get plans plan-id)
        {:keys [n-keys parts n-parts]} plan-data
        parts (or parts [])
        have-parts (count parts)]
    (cond
      (not d)
      (println "NEW PLAN PART, but not loading")
      (not= n have-parts)
      (do
        (error! d (str "NEW-PLAN-PART plan-id" plan-id
                    "UNEXPECTED part" n "of" n-parts))
        (st/app-set :app/loading nil)
        (apply-network-updates false))
      :else
      (let [have-parts (inc have-parts)
            parts (conj parts part)
            plan-data (assoc plan-data :parts parts)]
        (if (= have-parts n-parts)
          (let [plan (apply merge parts)
                p-keys (count (keys plan))
                plan-data (assoc (dissoc plan-data :parts)
                            :loaded? true) ;; :plan plan
                plans (assoc plans plan-id plan-data)]
            (if (= p-keys n-keys)
              (do
                (println "RECEIVED ALL" have-parts "parts for" plan-id)
                (st/app-set :app/plans plans)
                (on-realized (new-plan (normalize-plan plan-id plan))
                  #(do
                     (success! d %)
                     ;; (status-msg "loaded" plan-id)
                     (st/app-set :app/loading nil)
                     (apply-network-updates true))
                  #(do
                     (error! d %)
                     (status-msg "failed to load" plan-id)
                     (st/app-set :app/loading nil)
                     (apply-network-updates false))))
                (do
                  (error! d (str "NEW-PLAN-PART plan-id" plan-id
                              "n-keys MISMATCH" p-keys))
                  (st/app-set :app/plans plans)
                  (st/app-set :app/loading nil)
                  (apply-network-updates false))))
          (let [start (tasks/deferred)
                finish (-> start
                         (sleep (+ 30 (* 5 (log2 n-keys)))) ;; pace the loading
                         (chain #(do
                                   (request-plan-part plan-id have-parts)
                                   true)))]
            (st/app-set :app/plans (assoc plans plan-id plan-data))
            (println "RECEIVED part" n "of" n-parts "parts for" plan-id)
            (tasks/on-realized finish #(identity true)) ;; consume finish
            (success! start true)))))))

(defn failed-plan-part [err]
  (let [d (st/app-get :app/loading)]
    (if-not d
      (println "FAILED PLAN PART, but not loading")
      (do
        (error! d err)
        (st/app-set :app/loading nil)
        (apply-network-updates false)))))

(defn request-plan-part [plid part]
  (println "REQUEST part" part "for" plid)
  (rmethod {:return d-request-plan-part
            :success-fn new-plan-part
            :error-fn failed-plan-part}
    :request-plan-part plid part))

(defn list-plans
  "Shown known plan-id's"
  []
  (let [plan-ids (vec (sort (keys (or (st/app-get :app/plans) {}))))]
    (apply status-msg "plans " plan-ids)))

;; may have value passed in from previous thing in chain
(defn update-plan-list
  "Refresh the known plan list"
  [ & [v]]
  ;; (println "UPLR" v)
  (let [start (tasks/deferred)
        finish (-> start
                 (chain-rmethod :list-plans)
                 (chain
                   (fn [plist]
                     (let [plans (or (st/app-get :app/plans) {})]
                       (loop [updated false plans plans
                              p-plan (first plist) more (rest plist)]
                         (if-not p-plan
                           (do
                             (if updated (st/app-set :app/plans plans))
                             (vec (keys plans)))
                           (let [[plid plan] p-plan]
                             (if (get plans plid)
                               (do
                                 ;; (println "UPLR: already have" plid)
                                 (recur updated plans
                                   (first more) (rest more)))
                               (do
                                 ;; (println "UPLR: adding" plid)
                                 (recur true (assoc plans plid plan)
                                   (first more) (rest more)))))))))))]
    (success! start true)
    finish))

(defn update-url-config [ & [v]]
  (let [start (tasks/deferred)
        finish (-> start
                 (chain-rmethod :get-url-config)
                 (chain
                   (fn [url-config]
                     (st/app-set :app/url-config url-config)
                     ;; (println "DEBUG update-url-config loaded"
                     ;;   (count url-config) "url entries")
                     true)))]
    (success! start true)
    finish))

(defn generic-reply [message]
  (if (:error message)
    (status-msg "ERROR" (:error message))
    (status-msg message)))

(defn whoami
  "Show my remote-id, nickname and following"
  []
  (println "whoami")
  (rmethod {:success-fn generic-reply :error-fn generic-reply}
    :whoami))

(defn who
  "Show list of PLANVIZ users"
  []
  (println "who")
  (rmethod {:success-fn generic-reply :error-fn generic-reply}
    :who))

(defn nick
  "Set my nickname"
  {:details {:nickname "my short name for /follow or /msg"}}
  [nickname]
  (println "nick" nickname)
  (rmethod {:success-fn generic-reply :error-fn generic-reply}
    :nick nickname))

(defn follow-error [x]
  (status-msg x)
  (st/app-set :app/following nil))

(defn follow-success [x]
  (if-let [error (:error x)]
    (follow-error error)
    (status-msg x)))

(defn follow
  "Follow another user"
  {:details {:nickname "remote-id or nick for user to follow"}}
  [nickname]
  (println "FOLLOW" nickname)
  (st/app-set :app/following (if (not= nickname "-") nickname))
  (rmethod {:success-fn follow-success :error-fn follow-error}
    :follow nickname))

(defn unfollow []
  (follow "-"))

(defn save-settings
  "Save settings on the server"
  [& [new-filename]]
  (let [{:keys [settings/shown? settings/settings-action
                settings/filename] :as settings} (st/app-get-settings)
        new-filename (or new-filename filename "default")
        settings (assoc (apply dissoc settings ui/planviz-settings-ignore)
                   :settings/filename new-filename)
        start (tasks/deferred)
        finish (-> start
                 (chain-rmethod :save-settings settings)
                 (chain
                   (fn [settings]
                     (if (and (map? settings) (not (:error settings)))
                       (let [{:keys [settings/filename]} settings
                             was-unsafe? (not= filename new-filename)
                             settings (assoc settings
                                        :settings/shown? shown?
                                        :settings/settings-action settings-action)]
                         (if was-unsafe?
                           (st/app-merge-input-box {:input-box/id :filename
                                                    :input-box/value filename}))
                         (st/app-set-settings settings)
                         (st/set-ui-opts-settings settings))
                       (status-msg "could not save settings:"
                         (or (:error settings) new-filename)))
                     true)))]
    (success! start true)
    finish
    ;; (println "SETTINGS" settings)
    ))

(defn load-settings
  "Load settings from the server"
  [filename]
  (let [{:keys [settings/shown?
                settings/settings-action]} (st/app-get-settings)
        start (tasks/deferred)
        finish (-> start
                 (chain-rmethod :load-settings filename)
                 (chain
                   (fn [settings]
                     (if (and (map? settings) (not (:error settings)))
                       (let [settings (assoc settings
                                        :settings/shown? shown?
                                        :settings/settings-action settings-action)]
                         (st/app-set-settings settings)
                         (st/set-ui-opts-settings settings))
                       (status-msg "could not load settings:"
                         (or (:error settings) filename)))
                     true)))]
    (success! start true)
    finish))

(defn list-settings-filenames
  "List settings filenames from the server"
  []
  (rmethod {:success-fn generic-reply :error-fn generic-reply}
    :list-settings-filenames))

(defn auto? []
  (keyword-identical? (or (st/app-get :app/mode) :manual) :auto))

(defn manual
  "Switch to manual mode"
  []
  (when (auto?)
    (st/app-set :app/mode :manual)
    (unfollow)
    (when (:settings/auto (st/app-get-settings))
      (st/app-merge-settings {:settings/auto false})
      (st/merge-ui-opts-settings {:settings/auto false}))
    (status-msg "switching to manual mode")))

(defn auto
  "Switch to automatic mode"
  []
  (when-not (auto?)
    (st/app-set :app/mode :auto)
    (when-not (:settings/auto (st/app-get-settings))
      (st/app-merge-settings {:settings/auto true})
      (st/merge-ui-opts-settings {:settings/auto true}))
    (status-msg "switching to automatic mode")))

(defn zoom! [z]
  (st/app-merge-pan-zoom {:pan-zoom/zoom z})
  nil)

(def max-zoom 256.0)
(def min-zoom 0.5)

(defn zoom-in
  "Zoom in"
  []
  (let [zoom (:pan-zoom/zoom (st/app-get :app/pan-zoom))
        zoom (* zoom 1.2)
        zoom (min (max zoom min-zoom) max-zoom)]
    (zoom! zoom)
    (my-user-action {:type :pan-zoom :zoom zoom})))

(defn zoom-out
  "Zoom out"
  []
  (let [zoom (:pan-zoom/zoom (st/app-get :app/pan-zoom))
        zoom (* zoom 0.8)
        zoom (min (max zoom min-zoom) max-zoom)]
    (zoom! zoom)
    (my-user-action {:type :pan-zoom :zoom zoom})))

(defn pan! [p]
  (st/app-merge-pan-zoom {:pan-zoom/pan p})
  nil)

(defn pan-left
  "Pan left"
  []
  (let [{:keys [pan-zoom/zoom pan-zoom/pan]} (st/app-get :app/pan-zoom)
        [pan-x pan-y] pan
        pan-min (if (>= zoom 1.0)
                  0
                  -0.75)
        pan-max (if (>= zoom 1.0)
                  (- 1 (/ 1 zoom))
                  0.75)
        pan-delta (if (>= zoom 1.0)
                    (/ 1 (* 4 zoom))
                    (/ 1 (* 8 zoom)))
        pan-x (- pan-x pan-delta)
        pan-x (min (max pan-x pan-min) pan-max)
        pan [pan-x pan-y]]
    (pan! pan)
    (my-user-action {:type :pan-zoom :pan pan})))

(defn pan-right
  "Pan right"
  []
  (let [{:keys [pan-zoom/zoom pan-zoom/pan]} (st/app-get :app/pan-zoom)
        [pan-x pan-y] pan
        pan-min (if (>= zoom 1.0)
                  0
                  -0.75)
        pan-max (if (>= zoom 1.0)
                  (- 1 (/ 1 zoom))
                  0.75)
        pan-delta (if (>= zoom 1.0)
                    (/ 1 (* 4 zoom))
                    (/ 1 (* 8 zoom)))
        pan-x (+ pan-x pan-delta)
        pan-x (min (max pan-x pan-min) pan-max)
        pan [pan-x pan-y]]
    (pan! pan)
    (my-user-action {:type :pan-zoom :pan pan})))

(defn pan-up
  "Pan up"
  []
  (let [{:keys [pan-zoom/zoom pan-zoom/pan]} (st/app-get :app/pan-zoom)
        [pan-x pan-y] pan
        pan-min (if (>= zoom 1.0)
                  0
                  -0.75)
        pan-max (if (>= zoom 1.0)
                  (- 1 (/ 1 zoom))
                  0.75)
        pan-delta (if (>= zoom 1.0)
                    (/ 1 (* 4 zoom))
                    (/ 1 (* 8 zoom)))
        pan-y (- pan-y pan-delta)
        pan-y (min (max pan-y 0) pan-max)
        pan [pan-x pan-y]]
    (pan! pan)
    (my-user-action {:type :pan-zoom :pan pan})))

(defn pan-down
  "Pan down"
  []
  (let [{:keys [pan-zoom/zoom pan-zoom/pan]} (st/app-get :app/pan-zoom)
        [pan-x pan-y] pan
        pan-min (if (>= zoom 1.0)
                  0
                  -0.75)
        pan-max (if (>= zoom 1.0)
                  (- 1 (/ 1 zoom))
                  0.75)
        pan-delta (if (>= zoom 1.0)
                    (/ 1 (* 4 zoom))
                    (/ 1 (* 8 zoom)))
        pan-y (+ pan-y pan-delta)
        pan-y (min (max pan-y 0) pan-max)
        pan [pan-x pan-y]]
    (pan! pan)
    (my-user-action {:type :pan-zoom :pan pan})))

(defn pan-zoom! [pan zoom]
  (st/app-merge-pan-zoom {:pan-zoom/pan pan :pan-zoom/zoom zoom})
  nil)

(defn reset
  "Reset view to see entire plan"
  [&[opts]]
  (let [from-auto? (:auto opts)]
    (pan-zoom! [0.0 0.0] 1.0)
    (if-not from-auto?
      (my-user-action {:type :pan-zoom
                       :pan [0.0 0.0] :zoom 1.0}))
    true))

(defn visit-nodes [node-id prev-visited node-fn]
  (if (prev-visited node-id)
    prev-visited
    (let [node (st/plans-get-node node-id)
          visited (atom (conj prev-visited node-id))
          tovisit (remove nil? (node-fn node))] ;; visit any nodes returned
      (loop [visit (first tovisit) more (rest tovisit)]
        (when visit
          (swap! visited set/union (visit-nodes visit @visited node-fn))
          (recur (first more) (rest more))))
      @visited)))

(defn map-outgoing [node edge-fn]
  (doall (map (comp edge-fn st/plans-get-edge) (:node/outgoing node))))

;; new generation using :node/number and :edge/number
;; is sel within node-id?
(defn is-within-node? [sel node-id]
  (let [node (st/plans-get-node node-id)
        {:keys [node/number]} node
        [element id] sel
        sel-number (if (keyword-identical? :node element)
                     (:node/number (st/plans-get-node id))
                     (:edge/number (st/plans-get-edge id)))
        number-n (count number)
        sel-number-n (count sel-number)]
    (and (> sel-number-n number-n)
      (= number
        (vec (take number-n sel-number))))))

;; return the parts of sub in selection
(defn selection-subset [sub selection]
  (loop [a-subs [] a (first sub) a-more (rest sub)]
    (if-not a
      a-subs
      (let [within? (loop [b (first selection) b-more (rest selection)]
                      (if-not b
                        false
                        (if (or (= a b) (and (= (first b) :node)
                                          (is-within-node? a (second b))))
                          true
                          (recur (first b-more) (rest b-more)))))
            a-subs (if within? (conj a-subs a) a-subs)]
        (recur a-subs (first a-more) (rest a-more))))))

(defn set-selected? [new-selected? plid sel opts]
  (let [{:keys [edge node]} opts
        [type id] sel]
    ;; (println "SET-SELECTED?" new-selected? type id
    ;;   "EDGE" (:edge/id edge) "NODE" (remove-fn node))
    (case type
      :edge ;; only a TPN activity for now
      (let [o-id (edge-key-fn edge)
            edge (if (keyword-identical? id o-id)
                   edge (st/plans-get-edge id))
            {:keys [edge/selected? edge/type edge/from edge/to]} edge]
        (when (not= selected? new-selected?)
          (st/plans-merge-edge
            (assoc
              (dissoc edge :plans/ui-opts :edge/from :edge/to)
              :edge/selected? new-selected?))
          (st/plans-merge-node
            (assoc
              (dissoc from :plans/ui-opts :node/tpn-selection)
              :node/selected? new-selected?))
          (st/plans-merge-node
            (assoc
              (dissoc to :plans/ui-opts :node/tpn-selection)
              :node/selected? new-selected?))
          (if (keyword-identical? type :aggregation) ;; set interior
            (let [id (node-key-fn from)
                  node (st/plans-get-node id)
                  {:keys [node/end]} node]
              (visit-nodes id #{end}
                (fn [node]
                  (st/plans-merge-node
                    (assoc
                      (dissoc node :plans/ui-opts :node/tpn-selection)
                      :node/selected? new-selected?))
                  (map-outgoing node
                    (fn [edge]
                      (let [{:keys [edge/type edge/to
                                    edge/weight edge/hidden]} edge]
                        (when (or (activity-type? type)
                                (keyword-identical? type :aggregation))
                          (st/plans-merge-edge
                            (assoc
                              (dissoc edge
                                :plans/ui-opts :edge/from :edge/to)
                              :edge/selected? new-selected?))
                          (node-key-fn to)))))))))))
      :node
      (let [o-id (node-key-fn node)
            node (if (keyword-identical? id o-id)
                   node (st/plans-get-node id))
            {:keys [node/end node/type node/selected?]} node]
        (when (not= selected? new-selected?)
          (st/plans-merge-node
            (assoc
              (dissoc node :plans/ui-opts :node/tpn-selection)
              :node/selected? new-selected?))
          (when (begin? type)
            (st/plans-merge-node
              (assoc
                (dissoc (st/plans-get-node end)
                  :plans/ui-opts :node/tpn-selection)
                :node/selected? new-selected?))
            (visit-nodes id #{end}
              (fn [node]
                (st/plans-merge-node
                  (assoc
                    (dissoc node :plans/ui-opts :node/tpn-selection)
                    :node/selected? new-selected?))
                (map-outgoing node
                  (fn [edge]
                    (let [{:keys [edge/type edge/to
                                  edge/weight edge/hidden]} edge]
                      (when (or (activity-type? type)
                              (keyword-identical? type :aggregation))
                        (st/plans-merge-edge
                          (assoc
                            (dissoc edge :plans/ui-opts :edge/from :edge/to)
                            :edge/selected? new-selected?))
                        (node-key-fn to))))))))))
      (println "Don't know how to select a" type))))

(defn plan-selection [plid s &[opts]]
  (let [old-selection (st/app-get-plan-value plid :selection)
        from-auto? (:auto opts)]
    (when (not= s old-selection)
      (st/app-set-plan-value plid :selection s))
    (if-not from-auto?
      (my-user-action {:type :selection
                       :plid plid
                       :selection s}))))

(defn clear-selection [plid & [opts]]
  (let [selection (st/app-get-plan-value plid :selection)]
    (when (and (vector? selection) (pos? (count selection)))
      (doseq [sel selection]
        (set-selected? false plid sel opts)))
    (plan-selection plid [])))

(defn add-to-selection [plid sel & [opts]]
  (let [selection (st/app-get-plan-value plid :selection)]
    (when-not (some #(keyword-identical? % sel) selection)
      (set-selected? true plid sel opts)
      (plan-selection plid (conj selection sel)))))

;; sel MAY be multiple
(defn remove-from-selection [plid sel & [opts]]
  (let [selection (st/app-get-plan-value plid :selection)
        multiple? (vector? (first sel))
        remove (if (or multiple? (empty? sel)) sel [sel])]
    (loop [selection selection r (first remove) more (rest remove)]
      (if-not r
        ;; can't trust opts here.. may not be the same node/edge
        (plan-selection plid selection)
        (do
          ;; (println "REMOVE-FROM-SELECTION" r)
          (set-selected? false plid r nil)
          (recur (vec (filter #(not= % r) selection))
            (first more) (rest more)))))))

(defn remove-subset [selection remove]
  (loop [selection selection r (first remove) more (rest remove)]
    (if-not r
      selection
      (do
        (recur (vec (filter #(not= % r) selection))
          (first more) (rest more))))))

;; sel MIGHT be a vector
(defn set-selection [plid sel & [opts]]
  (when (and plid sel)
    ;; (println "DEBUG set-selection PLID" plid "SEL" sel)
    (let [selection (st/app-get-plan-value plid :selection)
          multiple? (vector? (first sel))
          new-selection (if (or multiple? (empty? sel)) sel [sel])]
      (when (not= new-selection selection)
        (when (and (vector? selection) (pos? (count selection)))
          (doseq [sel selection]
            (when-not (some #(keyword-identical? % sel) new-selection)
              (set-selected? false plid sel opts))))
        (doseq [sel new-selection]
          (when-not (some #(keyword-identical? % sel) selection)
            (set-selected? true plid sel opts))))
      (plan-selection plid new-selection opts))))

;; returns proposed HTN selection
(defn highlight-from-tpn [htn-plid tpn-plid csel]
  (loop [sel #{} c (first csel) more (rest csel)]
    (if-not c
      (vec sel)
      (let [[stype id] c
            ;; _ (println "Corresponding selection" c "in TPN" tpn-plid)
            s (case stype
                :edge
                (let [edge (st/plans-get-edge id)
                      {:keys [edge/htn-node]} edge
                      ;; _ (println "htn-node" htn-node)
                      hpt-hent (if htn-node (st/plans-get-node htn-node))
                      {:keys [node/type node/parent]} hpt-hent
                      ;; _ (println "hpt-hent type" type "parent" parent)
                      network-parent (if parent (st/get-network-parent parent))
                      hem (if (keyword-identical? type :htn-expanded-method)
                            hpt-hent
                            (if network-parent
                              (st/plans-get-node network-parent)))
                      hem-id (:node/id hem)
                      hem-id (if hem-id
                               [:node (composite-key htn-plid hem-id)])]
                  ;; (println "HEM" hem-id)
                  ;; (add-to-selection htn-plid [:node hem-id] {:node hem})
                  hem-id)
                :node
                (let [node (st/plans-get-node id)
                      {:keys [node/htn-node]} node
                      ;; _ (println "htn-node" htn-node)
                      hpt-hent (if htn-node (st/plans-get-node htn-node))
                      {:keys [node/type node/parent]} hpt-hent
                      ;; _ (println "hpt-hent type" type "parent" parent)
                      network-parent (if parent (st/get-network-parent parent))
                      hem (if (keyword-identical? type :htn-expanded-method)
                            hpt-hent
                            (if network-parent
                              (st/plans-get-node network-parent)))
                      hem-id (:node/id hem)
                      hem-id (if hem-id
                               [:node (composite-key htn-plid hem-id)])]
                  ;; (println "HEM" hem-id)
                  ;; (add-to-selection htn-plid [:node hem-id] {:node hem})
                  hem-id)
                (do
                  (println "highlight of TPN" stype "not supported")))]
        (recur (if s (conj sel s) sel) (first more) (rest more))))))

;; return the minimal representation of sel
;; first find any leader nodes among the nodes
;; then add in any non comprised edges
(defn minimal-selection [sel]
  ;; (println "MIN SEL" sel)
  (let [mnodes (vec (remove nil? (filter #(keyword-identical? :node (first %)) sel)))
        medges (remove nil? (filter #(keyword-identical? :edge (first %)) sel))
        msel (loop [msel [] n (first mnodes) more (rest mnodes)]
               ;; (println "MNTOP" msel "N" n)
               (if-not n
                 msel
                 (let [node-subset (selection-subset [n] msel)
                       subset-node (selection-subset msel [n])
                       msel (cond
                              (not (empty? node-subset))
                              (do
                                ;; (println "MN" n "is already in" msel)
                                msel ;; n is already in msel
                                )
                              (not (empty? subset-node))
                              ;; remove the subset-node in msel already, add n
                              (conj (remove-subset msel subset-node) n)
                              :else
                              (conj msel n))]
                   (recur msel (first more) (rest more)))))
        msel (loop [msel msel e (first medges) more (rest medges)]
               ;; (println "METOP" msel "E" e)
               (if-not e
                 msel
                 (let [edge-subset (selection-subset [e] msel)
                       msel (cond
                              (not (empty? edge-subset))
                              (do
                                ;; (println "ME" e "is already in" msel)
                                msel ;; e is already in msel
                                )
                              :else
                              (conj msel e))]
                   (recur msel (first more) (rest more)))))]
  ;; (println "MIN SEL FINAL" msel)
  msel))

;; returns proposed HTN selection
(defn highlight-from-htn [tpn-plid htn-plid csel]
  (loop [sel #{} c (first csel) more (rest csel)]
    ;; (println "HFH SEL" sel "C" c "MORE" more)
    (if-not c
      ;; already pre-computed to be minimal!
      ;; (minimal-selection (vec sel))
      (vec sel)
      (let [[stype id] c
            ;; _ (println "Corresponding selection" c "in HTN" htn-plid)
            sel (case stype
                  :node
                  (let [{:keys [node/tpn-selection]} (st/plans-get-node id)
                        tsel (atom sel)]
                    ;; (println "  TPN-SELECTION" tpn-selection)
                    (doseq [s tpn-selection]
                      (let [node-id (if (:node/id s) (node-key-fn s))
                            edge-id (if (:edge/id s) (edge-key-fn s))]
                        (if (keyword? node-id)
                          (swap! tsel conj
                            [:node (second (get-begin-node-ref node-id))])
                          (if (keyword? edge-id)
                            (swap! tsel conj [:edge edge-id])
                            (do
                              (println "ERROR, invalid tpn-selection!")
                              )))
                        ;; (println "  S" s "TSEL" @tsel)
                        ))
                    @tsel)
                  (do
                    (println "highlight of HTN" stype "not supported")
                    sel))]
        (recur sel (first more) (rest more))))))

;; if there is a relevant selection.. show it here!
;; if this is an htn with a corresponding tpn AND
;; the tpn has a selection... make the highlight now!
(defn highlight-relevant [&[remote]]
  (let [{:keys [ui/show-plan ui/show-network ui/network-type]} (st/get-ui-opts)
        {:keys [corresponding selection]} (st/app-get-plan show-plan)
        csel (if corresponding
               (st/app-get-plan-value corresponding :selection))
        auto-on? (auto?)]
    (println "HIGHLIGHT-RELEVANT" show-plan show-network network-type
      "CORRESPONDING" corresponding
      "SELECTION" selection "CSEL" csel)
    (when csel
      (case network-type
        :hem-network
        (if auto-on?
          (let [sel (if (and (vector? csel) (pos? (count csel)))
                      (highlight-from-tpn show-plan corresponding csel)
                      [])]
            ;; (println "TPN CSEL" csel "SEL" sel)
            (if remote
              (status-msg "Showing TPN correspondence from" remote)
              (status-msg "Showing TPN correspondence"))
            (set-selection show-plan sel {:auto true}))
          (status-msg "Not showing TPN correspondence in manual mode"))
        :tpn-network
        (if auto-on?
          (let [sel (if (and (vector? csel) (pos? (count csel)))
                      (highlight-from-htn show-plan corresponding csel)
                      [])]
            ;; (println "HTN CSEL" csel "SEL" sel)
            (if remote
              (status-msg "Showing HTN correspondence from" remote)
              (status-msg "Showing HTN correspondence"))
            (set-selection show-plan sel {:auto true}))
          (status-msg "Not showing HTN correspondence in manual mode"))
        ))
    true))

(declare load-plan)

(defn zoom-to-bounds [bounds not-auto?]
  ;; (println "ZOOM-TO-BOUNDS" bounds "not-auto?" (not not-auto?))
  (let [{:keys [xbounds ybounds]} bounds
        [xmin xmax] xbounds
        [ymin ymax] ybounds
        nodesep 50
        xmin (max (- xmin nodesep) 0)
        xmax (+ xmax nodesep)
        ymin (max (- ymin nodesep) 0)
        ymax (+ ymax nodesep nodesep) ;; extra padding at the bottom
        focus-width (- xmax xmin)
        focus-height (- ymax ymin)
        focus-ratio (/ focus-height focus-width)
        {:keys [pan-zoom/width pan-zoom/height
                pan-zoom/vp-width pan-zoom/vp-height]} (st/app-get :app/pan-zoom)
        vp-ratio (/ vp-height vp-width)
        ratio (/ height width)
        ;; _ (println "width" width "height" height "ratio" ratio)
        vertical? (> ratio vp-ratio) ;; constrained vertically?
        ;; _ (println "vp-width" vp-width "vp-height" vp-height "vp-ratio" vp-ratio
        ;;     "vertical?" vertical?)
        focus-vertical? (> focus-ratio vp-ratio)
        ;; _ (println "xmin" xmin "xmax" xmax "ymin" ymin "ymax" ymax
        ;;     "focus-width" focus-width "focus-height" focus-height
        ;;     "focus-ratio" focus-ratio "focus-vertical?" focus-vertical?)
        zoom (if focus-vertical?
               (/ height focus-height)
               (/ width focus-width))
        offset-x (if focus-vertical?
                   (* (- (/ focus-height vp-ratio) focus-width) 0.5)
                   0)
        ;; NOTE: don't push too far to to the right (as HTN's tend to have
        ;; extra left padding)
        offset-y (if focus-vertical?
                   0
                   (* (- (* focus-width vp-ratio) focus-height) 0.7))
        pan [(/ (- xmin offset-x) width) (/ (- ymin offset-y) height)]
        pz {:pan-zoom/pan pan :pan-zoom/zoom zoom}]
    (println "Z" zoom "P" pan)
    ;; we know that when this is called the plans have been mutated...
    (st/plans-merge-pan-zoom pz)
    (st/app-merge-pan-zoom pz true)
    (when not-auto?
      (my-user-action {:type :pan-zoom :pan pan :zoom zoom}))))

(defn htn-focus-tpn [tpn-id show-network htn-id node-id focus?]
  ;; (println "DEBUG htn-focus-tpn" tpn-id htn-id node-id focus?)
  (let [{:keys [node/tpn-selection]} (if node-id (st/plans-get-node node-id))
        {:keys [network/nodes network/edges]} (st/get-network show-network)
        ]
    ;; (println "DEBUG hft tpn-selection" tpn-selection)
    (doseq [node nodes]
      (st/plans-merge-node
        (assoc
          (dissoc node :node/ui-opts :node/tpn-selection)
          :node/hidden focus?)))
    (doseq [edge edges]
      (let [{:keys [edge/type edge/hidden]} edge]
        (if (and (not (keyword-identical? type :aggregation))
              (not= hidden focus?))
          (st/plans-merge-edge
            (assoc (dissoc edge :edge/from :edge/to :edge/ui-opts)
              :edge/hidden focus?)))))
    (if-not focus?
      (reset {:auto true})
      (let [bounds (atom {:xbounds [nil nil] :ybounds [nil nil]})]
        (doseq [s tpn-selection]
          (if (:node/id s)
            (let [node-id (composite-key tpn-id (:node/id s))
                  node (st/plans-get-node node-id)
                  {:keys [node/begin node/x node/y]} node]
              (println "NODE BOUND" x y)
              (swap! bounds (fn [{:keys [xbounds ybounds]}]
                              {:xbounds (min-max-fn xbounds [x x])
                               :ybounds (min-max-fn ybounds [y y])}))
              (st/plans-merge-node
                (assoc (dissoc node :node/ui-opts :node/tpn-selection)
                  :node/hidden false))
              (visit-nodes begin #{node-id}
                (fn [node]
                  (let [{:keys [node/hidden node/x node/y]} node]
                    ;; (println "NODE BOUND" x y) ;; DEBUG
                    (swap! bounds (fn [{:keys [xbounds ybounds]}]
                                    {:xbounds (min-max-fn xbounds [x x])
                                     :ybounds (min-max-fn ybounds [y y])}))
                    (if hidden
                      (st/plans-merge-node
                        (assoc
                          (dissoc node :plans/ui-opts :node/tpn-selection)
                          :node/hidden false)))
                    (map-outgoing node
                      (fn [edge]
                        (let [{:keys [edge/type edge/to edge/hidden]} edge]
                          (when (activity-type? type)
                            (if hidden
                              (st/plans-merge-edge
                                (assoc
                                  (dissoc edge
                                    :plans/ui-opts :edge/from :edge/to)
                                  :edge/hidden false)))
                            (node-key-fn to)))))))))
            (let [edge (st/plans-get-edge
                         (composite-key tpn-id (:edge/id s)))
                  {:keys [edge/hidden edge/from edge/to]} edge
                  from-x (:node/x from)
                  from-y (:node/y from)
                  to-x (:node/x to)
                  to-y (:node/y to)]
              ;; (println "EDGE BOUND FROM" from-x from-y "TO" to-x to-y) ;; DEBUG
              (swap! bounds (fn [{:keys [xbounds ybounds]}]
                              {:xbounds (min-max-fn xbounds
                                          [(min from-x to-x)
                                           (max from-x to-x)])
                               :ybounds (min-max-fn ybounds
                                          [(min from-y to-y)
                                           (max from-y to-y)])}))
              (if hidden
                (st/plans-merge-edge
                  (assoc (dissoc edge :edge/from :edge/to :edge/ui-opts)
                    :edge/hidden false)))
              (st/plans-merge-node
                (assoc
                  (dissoc from :plans/ui-opts :node/tpn-selection)
                  :node/hidden false))
              (st/plans-merge-node
                (assoc
                  (dissoc to :plans/ui-opts :node/tpn-selection)
                  :node/hidden false)))))
        (zoom-to-bounds @bounds false)
        ))))

(defn no-plan? [showing]
  (or (not showing) (#{:all :none :loading} showing)))

(defn display-plan
  "Display a plan"
  [plid &[opts]]
  (let [{:keys [load-delay]} opts
        plid (if (string? plid)
                  (keyword (string/replace-first plid  #"^:" ""))
                  plid)]
    (when-not (no-plan? plid)
      (let [{:keys [loaded? type corresponding n-keys]} (st/app-get-plan plid)
            load-delay (+ (or load-delay 0) (+ 100 (* 5 (log2 n-keys))))
            opts (assoc opts :load-delay load-delay)
            plid-type type
            plid-loaded? loaded?
            {:keys [loaded? focus]} (if corresponding
                                      (st/app-get-plan corresponding))
            load-corresponding? (and corresponding (not loaded?))
            [tpn-plan htn-plan] (if (keyword-identical? plid-type :htn-network)
                                  [corresponding plid]
                                  [plid corresponding])
            tooltips? (< n-keys 256)]
        (println "ACTIONS/DISPLAY-PLAN" plid "TYPE" type "OPTS" opts
          "plid-loaded?" plid-loaded?
          "load-corresponding?" load-corresponding?
          "focus" focus)
        (if (and plid-loaded? (not load-corresponding?))
          (do
            ;; (status-msg "display-plan" plid)
            (st/app-set :app/title (name plid))
            ;; (st/tooltips tooltips?)
            (st/app-merge-settings {:settings/tooltips tooltips?})
            (st/merge-ui-opts-settings {:settings/tooltips tooltips?})
            (st/show-plan plid)
            (reset opts)
            (if corresponding
              (highlight-relevant))
            (if (and focus (keyword-identical? type :tpn-network))
              (htn-focus-tpn plid (:ui/show-network (st/get-ui-opts))
                corresponding focus (as-boolean focus))))
          (if load-corresponding?
            (-> (load-plan tpn-plan) ;; LOAD TPN first!
              (sleep load-delay) ;; pace the browser
              (chain #(load-plan htn-plan opts))
              (sleep load-delay) ;; pace the browser, let TPN finish
              (chain #(display-plan plid opts))
              (tasks/catch #(do
                              (status-msg "unable to show" plid)
                              (st/loading false)
                              (st/app-set :app/loading nil))))
            (-> (load-plan plid opts)
              (sleep load-delay) ;; pace the browser
              (chain #(display-plan plid opts))
              (tasks/catch #(do
                              (status-msg "unable to show" plid)
                              (st/loading false)
                              (st/app-set :app/loading nil))))))
        true)))) ;; for deferreds

(defn show-plan-id
  "Display a plan"
  {:details {:plan-id "plan to display"}}
  [plan-id]
  (display-plan plan-id))

(defn load-plan [plan-id &[opts]]
  (let [plan-id (if (string? plan-id)
                  (keyword (string/replace-first plan-id  #"^:" ""))
                  plan-id)]
    (when-not (no-plan? plan-id)
      (let [d (tasks/deferred)
            secs (* 3 60 1000)
            dtimeout (tasks/timeout! d secs :timed-out)
            {:keys [loaded? n-keys parts n-parts]} (st/app-get-plan plan-id)
            have-parts (if parts (count parts) 0)
            reload? (:reload? opts)
            ]
        ;; (if (st/app-get :app/loading)
        ;;   (do
        ;;     (println "LOAD-PLAN will start in one second...")
        ;;     (tasks/timeout f 1000))
        ;;   (f))
        (println "LOAD-PLAN" plan-id "OPTS" opts)
        (cond
          (not n-keys)
          (do
            (println "cannot load unknown plan" plan-id)
            (tasks/timeout
              #(error! d (str "cannot load unknown plan " plan-id)))
            dtimeout)
          (and loaded? (not reload?))
          (do
            ;; (println "DEBUG: already loaded plan" plan-id)
            (tasks/timeout
              #(success! d (str "already loaded plan " plan-id)))
            dtimeout)
          (st/app-get :app/loading)
          (do
            (println "cannot load " plan-id
              " because a load is already in progress...")
            ;; (error! d (str "cannot load " plan-id
            ;;             " because a load is already in progress..."))
            (chain dtimeout #(load-plan plan-id opts)))
          :else
          (do
            ;; (status-msg "loading" plan-id)
            ;; (st/loading true)
            (st/app-set :app/loading d)
            (request-plan-part plan-id have-parts)
            dtimeout
            ))))))

(defn reload-plan [plan-id]
  (-> (load-plan plan-id {:reload? true})
    (sleep 5) ;; pace the loading
    (chain #(display-plan plan-id))))

(defn tpn-before-htn [plid]
  (if (not (nil? (clojure.string/index-of (name plid) "tpn")))
    0 1))

(defn load-all-plans [&[plan-ids]]
  ;; (println "LOAD-ALL-PLANS" plan-ids)
  (let [plan-ids (or plan-ids (vec (keys (st/app-get :app/plans))))
        plan-ids (sort tpn-before-htn plan-ids)
        start (tasks/deferred)
        finish
        (loop [prev start plan-id (first plan-ids) more (rest plan-ids)]
          (if-not plan-id
            prev
            (recur (sleep (chain prev #(load-plan plan-id)) 5)
              (first more) (rest more))))]
    (success! start true)
    finish))

(defn login []
  (let [{:keys [settings/settings-action]} (st/app-get-settings)
        dlogin (tasks/connect
                 (rmethod {} :login)
                 (tasks/deferred))
        finish (-> dlogin
                 (chain #(if (:error %) false
                             (let [settings
                                   (assoc (:login/settings %)
                                     :settings/shown? false
                                     :settings/settings-action settings-action)
                                   filename (:settings/filename settings)]
                               (if (:settings/auto settings)
                                 (auto))
                               (st/app-merge-input-box {:input-box/id :filename
                                                        :input-box/value filename})
                               (st/app-set-settings settings)
                               (st/set-ui-opts-settings settings)
                               (:login/remote %))))
                 (chain #(do (status-msg "logged in as" %)
                             (st/app-set :app/client %)
                             true))
                 (chain update-plan-list)
                 (chain update-url-config)
                 ;; (chain load-all-plans)
                 (chain #(do (status-msg "ready") true))
                 (tasks/catch #(do (st/app-set :app/client nil)
                                   (status-msg "login failed"))))]
    finish))

(defn recv-msg [line]
  (status-msg line))

(defn msg
  "Private message another user"
  {:details {:user "other PLANVIZ to message"
             :comments "contents of the message [optional]"}}
  [user & comments]
  (if-not user
    (status-msg "must specify user: /msg USER your message here")
    (rmethod {;; :return d-recv-msg
              :success-fn recv-msg :error-fn generic-reply}
      :msg user (apply str (interpose " " comments)))))

(defn stop-propagation [e]
  (when e
    (.preventDefault e)
    (.stopPropagation e)))

(defn menu-click-handled [e]
  (st/merge-ui-opts {:ui/menu nil})
  (stop-propagation e)
  false)

(defn info-menu-fn [node edge option e]
  (cond
    node
    (let [{:keys [tag]} option
          {:keys [plan/plid node/id node/type node/state]} node
          info (str "node " id ", type " type ", state " state)]
      (status-msg info)
      (my-user-action {:type :menu :tag tag :plid plid :node id :info info}))
    edge
    (let [;; _ (println "EDGE props:" (remove-fn edge)) ;; DEBUG
          {:keys [tag]} option
          {:keys [plan/plid edge/id edge/type edge/state edge/network-flows
                  edge/controllable]} edge
          ;; controllable (or controllable false)
          network-flows (if (keyword-identical? tag :tpn-network-flows) network-flows)
          info (str tag ", edge " id ", type " type ", state " state
                 ", controllable " controllable)]
      (status-msg info)
      (my-user-action (assoc-if {:type :menu :tag tag :plid plid :edge id}
                        :info (if-not network-flows info)
                        :network-flows network-flows)))
    :else
    (let [{:keys [tag]} option
          info "no node or edge selected"]
      (status-msg info)
      (my-user-action {:type :menu :tag tag :info info})))
  (menu-click-handled e))

;; https://developer.mozilla.org/en-US/docs/Web/API/Window/open#Position_and_size_features
(defn url-menu-fn [node edge option e]
  (let [{:keys [tag url tab]} option
        ;; features "location=0,toolbar=0,menubar=0,personalbar=0,titlebar=0,scrollbars=0,status=0"
        features "location=1,toolbar=1,menubar=1,personalbar=1,titlebar=1,scrollbars=1,status=1"]
    (status-msg "opening" url "in tab" tab)
    (.open js/window url tab features)
    (cond
      node
      (let [{:keys [plan/plid node/id node/type node/state]} node]
        (my-user-action {:type :menu :tag tag :plid plid :node id
                         :url url :tab tab}))
      edge
      (let [{:keys [plan/plid edge/id edge/type edge/state]} edge]
        (my-user-action {:type :menu :tag tag :plid plid :edge id
                         :url url :tab tab}))
      :else
      (my-user-action {:type :menu :tag tag :url url :tab tab}))
    (menu-click-handled e)))

;; returns rollup state for the aggregated edge
(defn set-aggregated? [begin-type begin end aggregated?]
  ;; (println "DEBUG set-aggregated?" begin end aggregated?)
  (let [network-updates? (and (keyword? aggregated?)
                           (keyword-identical? aggregated? :network-updates))
        aggregated? (boolean aggregated?)
        policy (if (keyword-identical? begin-type :c-begin)
                 :choice :parallel)
        rollup (atom 0)]
    (visit-nodes begin #{end}
      (fn [node]
        ;; (println "DEBUG set-aggregated? NODE" (:node/id node))
        (swap! rollup (partial max
                        (get-in node-states [(:node/state node) policy])))
        (if-not network-updates?
          (st/plans-merge-node
            ;; (if (and (begin? node) (not (keyword-identical? (node-key-fn node) begin)))
            (if (begin? node)
              (assoc (dissoc node :plans/ui-opts :node/tpn-selection)
                :node/hidden aggregated? :node/aggregated? false)
              (assoc (dissoc node :plans/ui-opts :node/tpn-selection)
                :node/hidden aggregated?))))
        (map-outgoing node
          (fn [edge]
            ;; (println "DEBUG set-aggregated? EDGE" (:edge/id edge))
            (let [{:keys [edge/type edge/to edge/state]} edge
                  aggregated-edge? (keyword-identical? :aggregation type)
                  hidden (or aggregated-edge? aggregated?)]
              (if-not aggregated-edge?
                (swap! rollup
                  (partial max
                    (get-in edge-states [state policy]))))
              (if-not network-updates?
                (st/plans-merge-edge
                  (assoc
                    (dissoc edge :plans/ui-opts :edge/from :edge/to)
                    :edge/hidden hidden)))
              (when (activity-type? type)
                (node-key-fn to)))))))
    (get (if (keyword-identical? policy :parallel)
           parallel-edge-states
           choice-edge-states) @rollup :normal)))

;; e nil if set via user-action
(defn aggregation-menu-fn [node _ option e]
  (when node
    (let [{:keys [tag]} option
          {:keys [plan/plid node/id node/end node/type]} node
          aggregated? (keyword-identical? tag :tpn-hide)
          node (assoc (dissoc node :plans/ui-opts :node/tpn-selection)
                 :node/aggregated? aggregated? :node/hidden false)
          begin-type type
          ;; set the interior
          rollup (set-aggregated? begin-type
                   (node-key-fn node) end aggregated?)]
      (status-msg tag id)
      ;; (println "rollup" rollup) ;; DEBUG
      ;; set the begin node
      (st/plans-merge-node node)
      ;; set the aggregation edge
      (map-outgoing node
        (fn [edge]
          (let [{:keys [edge/type edge/hidden]} edge]
            (when (keyword-identical? type :aggregation)
              (st/plans-merge-edge
                (assoc
                  (dissoc edge :plans/ui-opts :edge/from :edge/to)
                  :edge/state rollup
                  :edge/hidden (not aggregated?)))
              (if aggregated?
                (swap! aggregated-edges assoc (edge-key-fn edge)
                  {:begin-type begin-type
                   :begin (node-key-fn node)
                   :end end})
                (swap! aggregated-edges dissoc (edge-key-fn edge)))))))
      (when e
        (my-user-action {:type :menu :tag tag :plid plid :node id})
        (menu-click-handled e)))))

;; un hide all TPN nodes
(defn unhide-all []
  (let [{:keys [ui/show-plan ui/network-type]} (st/get-ui-opts)
        network (if (keyword-identical? network-type :tpn-network)
                  (st/get-network-basics show-plan))
        {:keys [network/begin network/end]} network]
    (when (and begin end)
      (println "UNHIDE-ALL" show-plan  "BEGIN" begin "END" end)
      (set-aggregated? :p-begin begin end false)
      (my-user-action {:type :menu :tag :tpn-show :plid show-plan :node begin}))))

;; e nil if set via user-action
(defn focus-menu-fn [node _ option e]
  (when node
    (let [{:keys [tag]} option
          {:keys [plan/plid node/id node/x node/y]} node
          node-id (composite-key plid id)
          focus? (keyword-identical? tag :htn-focus)
          visited (if focus? #{node-id} #{})
          begin (st/get-network-begin plid)
          bounds (atom {:xbounds [x x] :ybounds [y y]})]
      (status-msg tag id)
      ;; hide parents and siblings (or show all)
      (visit-nodes begin visited
        (fn [node]
          (st/plans-merge-node
            (assoc
              (dissoc node :plans/ui-opts :node/tpn-selection)
              :node/hidden focus?
              :node/aggregated? false))
          (map-outgoing node
            (fn [edge]
              (let [{:keys [edge/type edge/to edge/hidden]} edge]
                (when (#{:parallel-edge :choice-edge} type)
                  (if (not= focus? hidden)
                    (st/plans-merge-edge
                      (assoc
                        (dissoc edge :plans/ui-opts :edge/from :edge/to)
                        :edge/hidden focus?)))
                  (node-key-fn to)))))))
      ;; signal my action (before pan/zoom)
      (if e (my-user-action {:type :menu :tag tag :plid plid :node id}))
      ;; focus OUR sub network
      (when focus?
        (visit-nodes node-id #{}
          (fn [node]
            (st/plans-merge-node
              (assoc
                (dissoc node :plans/ui-opts :node/tpn-selection)
                :node/hidden false
                :node/aggregated? (keyword-identical? id (:node/id node))))
            (swap! bounds (fn [{:keys [xbounds ybounds]}]
                            (let [{:keys [node/x node/y]} node]
                              {:xbounds (min-max-fn xbounds [x x])
                               :ybounds (min-max-fn ybounds [y y])})))
            (map-outgoing node
              (fn [edge]
                (let [{:keys [edge/type edge/to edge/hidden]} edge]
                  (when (#{:parallel-edge :choice-edge} type)
                    (if hidden
                      (st/plans-merge-edge
                        (assoc
                          (dissoc edge :plans/ui-opts :edge/from :edge/to)
                          :edge/hidden false)))
                    (node-key-fn to)))))))
        (zoom-to-bounds @bounds e))
      (if-not focus?
        (reset)) ;; zoom out
      (st/app-set-plan-value plid :focus (if focus? node-id))
      (if e (menu-click-handled e)))))

(defn user-action [action]
  (let [client (st/app-get :app/client)
        ui-opts (st/get-ui-opts)
        showing (:ui/show-plan ui-opts)
        show-network (:ui/show-network ui-opts)
        auto-on? (auto?)
        blank-screen? (no-plan? showing)
        {:keys [planviz remote nick type plid selection pan zoom chat
                tag node edge url tab info network-flows]} action
        action-type type
        {:keys [loaded? type corresponding]} (st/app-get-plan plid)
        plid-type type
        plid-loaded? loaded?
        {:keys [loaded?]} (st/app-get-plan corresponding)
        load-corresponding? (and corresponding (not loaded?))
        [tpn-plan htn-plan] (if (keyword-identical? plid-type :htn-network)
                              [corresponding plid]
                              [plid corresponding])
        to-show (if (and auto-on? ;; assuming we want to show HTN or TPN
                      (not= remote client))
                  (or corresponding plid))
        ready? (or
                 (keyword-identical? :chat action-type)
                 (and plid-loaded? (not load-corresponding?))
                 ;; do NOT require showing this HTN or TPN
                 ;; (keyword-identical? showing to-show)
                 )
        opts {:auto true}]
    (when (not= remote client)
      (println "USER-ACTION" action "ready?" ready? "plid-loaded?" plid-loaded?
        "load-corresponding?" load-corresponding?))
    (if ready?
      (do
        (case action-type
          :selection
          (when (and (not= remote client) auto-on?)
            (println "REMOTE SET-SELECTION" action)
            (set-selection plid selection opts)
            (if (keyword-identical? corresponding showing)
              (highlight-relevant (or nick remote))))
          :pan-zoom
          (when (and (not= remote client) auto-on? (keyword-identical? plid showing))
            ;; (println "REMOTE PAN-ZOOM" pan zoom)
            (if pan (pan! pan))
            (if zoom (zoom! zoom))
            (status-msg "following view of" (or nick remote)))
          :chat
          (status-msg (str "<" (or nick remote) "> " chat))
          :menu
          (when (and (not= remote client) auto-on?)
            (cond
              (keyword-identical? tag :info)
              (println "USER-ACTION type" action-type "TAG" tag
                "NODE" node "EDGE" edge "INFO" info)
              (keyword-identical? tag :url)
              (println "USER-ACTION type" action-type "TAG" tag
                "URL" url "TAB" tab)
              (#{:htn-focus :htn-blur} tag)
              (let [focus? (keyword-identical? tag :htn-focus)
                    node-id (composite-key plid node)]
                (println "USER-ACTION type" action-type "TAG" tag "NODE" node)
                (if (keyword-identical? plid showing)
                  (focus-menu-fn (st/plans-get-node node-id) nil {:tag tag} nil)
                  (st/app-set-plan-value plid :focus (if focus? node-id)))
                (if (keyword-identical? corresponding showing)
                  (htn-focus-tpn showing show-network plid node-id focus?)))
              (#{:tpn-show :tpn-hide} tag)
              (let [{:keys [network/begin network/end]} (st/get-network-basics plid)]
                (println "USER-ACTION type" action-type "TAG" tag "NODE" node)
                (when (keyword-identical? plid showing)
                  (if (and (keyword-identical? tag :tpn-show)
                        (keyword-identical? node begin))
                    (set-aggregated? :p-begin begin end false) ;; unhide-all
                    (aggregation-menu-fn (st/plans-get-node (composite-key plid node))
                      nil {:tag tag} nil))))
              (keyword-identical? tag :tpn-network-flows)
              (println "USER-ACTION type" action-type "TAG" tag
                "NODE" node "NETWORK-FLOWS" network-flows)
              :else
              (println "USER-ACTION type" action-type "TAG" tag "not supported")))
          (println "USER-ACTION type" action-type "not supported"))
        true) ;; for deferred
      (if load-corresponding?
        (-> (load-plan tpn-plan opts) ;; LOAD TPN first!
          (sleep 30) ;; pace the browser
          (chain #(load-plan htn-plan opts))
          (sleep 30) ;; pace the browser
          ;; do NOT require showing this HTN or TPN
          ;; (chain #(display-plan to-show opts))
          (chain #(display-plan showing opts))
          (sleep 30) ;; pace the browser
          (chain #(user-action action)))
        (-> (load-plan plid opts) ;; loading on demand
          (sleep 30) ;; pace the browser
          ;; do NOT require showing this HTN or TPN
          ;; (chain #(display-plan to-show opts))
          (chain #(display-plan showing opts))
          (sleep 30) ;; pace the browser
          (chain #(user-action action)))))))

;; NOTE this is used ONLY for TPN's over RMQ --
;; Otherwise we would need to verify TPN was loaded before HTN
(defn add-plan [plid plan-details]
  (let [{:keys [ui/show-plan ui/show-network ui/network-type]} (st/get-ui-opts)
        {:keys [n-keys loaded? corresponding]} (st/app-get-plan plid)]
    ;; (println "ADD-PLAN" plid "DETAILS" plan-details
    ;;   "loaded?" loaded? "show-plan" show-plan)
    (if-not n-keys
      (st/app-set-plan plid plan-details))
    (status-msg "executing" plid)
    (if loaded?
      ;; only switch if not currently showing this plan or the corresponding one
      (when-not (or (keyword-identical? show-plan plid) (keyword-identical? show-plan corresponding))
        (display-plan plid))
      (-> (load-plan plid) ;; loading on demand
        (sleep 30) ;; pace the browser
        (chain #(display-plan plid))))))

;; set the state of all nodes and edges to normal in the
;; selected and corresponding plan(s)
(defn all-normal
  "Set the state of all nodes and edges to normal"
  []
  (let [{:keys [ui/show-plan ui/show-network ui/network-type]} (st/get-ui-opts)
        {:keys [loaded?]} (st/app-get-plan show-plan)]
    ;; (println "ALL NORMAL show-plan" show-plan "loaded?" loaded?)
    (if (and (not (#{:none :loading} show-plan)) loaded?)
      (st/all-normal show-plan))))

(defn another-plan [direction]
  (let [current-id (:ui/show-plan (st/get-ui-opts))
        plan-ids (vec (sort (keys (or (st/app-get :app/plans) {}))))
        n (count plan-ids)
        i (if (pos? n)
            (if current-id
              (mod (+ n (direction (vec-index-of plan-ids current-id))) n)
              0))
        next-id (if i (get plan-ids i))]
    (if next-id
      (display-plan next-id)
      (println "NO NEXT-PLAN"))))

(defn next-plan
  "Display the next plan in the known plan list"
  []
  (another-plan inc))

(defn prev-plan
  "Display the previous plan in the known plan list"
  []
  (another-plan dec))

(defn clear-plans
  "Discard the currently loaded plans"
  []
  (status-msg "clear-plans")
  (st/clear-plans)
  (reset))

(defn color-normal
  "End node/edge state color test"
  []
  (let [{:keys [ui/show-plan ui/show-network]} (st/get-ui-opts)]
    (if (or (not show-network) (keyword-identical? show-network :all))
      (status-msg "cannot run color-test for network:" show-network)
      (let [network (st/get-network show-network)
            {:keys [network/nodes network/edges]} network]
        (doseq [node nodes]
          (st/plans-merge-node
            (assoc (dissoc node :plans/ui-opts :node/tpn-selection)
              :node/state :normal :node/hidden false)))
        (doseq [edge edges]
          (if (activity-type? edge)
            (st/plans-merge-edge
              (assoc (dissoc edge :plan/ui-opts :edge/from :edge/to)
                :edge/state :normal
                :edge/hidden
                (keyword-identical? (:edge/type edge) :aggregation)
                ))))))))

(defn color-test
  "Start node/edge state color test"
  []
  (let [{:keys [ui/show-plan ui/show-network]} (st/get-ui-opts)]
    (if (or (not show-network) (keyword-identical? show-network :all))
      (status-msg "cannot run color-test for network:" show-network)
      (let [network (st/get-network show-network)
            {:keys [network/nodes network/edges]} network
            node-states-keys (vec (keys node-states))
            edge-states-keys (vec (keys edge-states))
            node-states-n (count node-states-keys)
            edge-states-n (count edge-states-keys)
            i (atom 0)]
        (doseq [node nodes]
          (st/plans-merge-node
            (assoc (dissoc node :plans/ui-opts :node/tpn-selection)
              :node/state
              (get node-states-keys (mod (swap! i inc) node-states-n)))))
        (doseq [edge edges]
          (if (keyword-identical? (:edge/type edge) :activity)
            (st/plans-merge-edge
              (assoc (dissoc edge :plan/ui-opts :edge/from :edge/to)
                :edge/state (get edge-states-keys
                              (mod (swap! i inc) edge-states-n))))))))))

(defn get-css [d]
  (let [css (st/app-get :app/css)]
    (if (string? css)
      true
      (let [dcss (tasks/deferred)]
        (xhr/send "/css/tplan.css"
          (fn [e]
            (let [xhr (.-target e)]
              (if (.isSuccess xhr)
                (do
                  (st/app-set :app/css (.getResponseText xhr))
                  (success! dcss true))
                (error! dcss false)))))
        dcss))))

(defn string->octet-array [s]
  ;; (into-array (doall (map #(.charCodeAt % 0) s))))
  (clj->js (mapv #(.charCodeAt % 0) s)))

(defn safe-octet-array [s]
  (loop [octets [] ch (first s) more (rest s)]
    (if-not ch
      (into-array octets)
      (if (= ch "▹")
        (recur (conj (conj octets 45) 62) (first more) (rest more)) ;; "->"
        (let [ascii (.charCodeAt ch 0)
              ascii (if (or (< ascii 0) (> ascii 255)) 32 ascii)] ;; wierd to space
          (recur (conj octets ascii) (first more) (rest more)))))))

(defn create-data-url [data-str mime-type]
  (let [data (safe-octet-array data-str)
        data-base64 (base64/encodeByteArray data)]
    (str "data:" mime-type ";base64," data-base64)))

(defn export-plan [d]
  (let [css (st/app-get :app/css)
        {:keys [ui/show-plan]} (st/get-ui-opts)
        filename (str (name show-plan) ".svg")
        application (gdom/getElement "application")
        plans (gdom/getElement "plans")
        svg (.-innerHTML plans)
        gt (inc (string/index-of svg ">"))
        svg-attrs (subs svg 4 gt)
        svg-body (-> svg
                   (subs gt)
                   (string/replace #"div>$" "svg>")
                   (string/replace #">" ">\n"))
        svg-header "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
        svg-begin "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"\n "
        css-begin "\n<style type=\"text/css\">\n<![CDATA[\n"
        css-end "\n]]>\n</style>\n"
        svg (str svg-header svg-begin svg-attrs
              css-begin css css-end
              svg-body)
        dataurl (create-data-url svg "image/svg+xml")
        a (.createElement js/document "a")]
    (println "exporting" show-plan "as" filename)
    (set! (.-id a) "download")
    (set! (.-download a) filename)
    (set! (.-innerText a) filename)
    (set! (.-href a) dataurl)
    (.appendChild application a)
    (.click a)
    (tasks/timeout #(.removeChild application a) 10)
    true))

(defn export
  "Download the current plan as an SVG file"
  []
  (let [{:keys [ui/show-plan]} (st/get-ui-opts)]
    (if (no-plan? show-plan)
      (status-msg "cannot export: no plan shown")
      (let [start (tasks/deferred)
            finish (-> start
                     (chain get-css)
                     (chain export-plan))]
        (success! start true)
        finish))))

(defn help-menu
  "Show the detailed help menu"
  []
  (let [shown? (st/app-get-help-shown)]
    (if shown?
      (status-msg "")
      (status-msg "help"))
    (st/app-set-help-shown (not shown?))))

(defn help-menu-hide
  "Hide the detailed help menu"
  []
  (if (st/app-get-help-shown)
    (help-menu)))

(defn help-click [e]
  (help-menu-hide)
  (stop-propagation e))

(defn settings-menu
  "Toggle showing the settings menu"
  []
  (let [{:keys [settings/xchar settings/ychar settings/auto
                settings/filename
                settings/shown?] :as settings} (st/app-get-settings)]
    (if shown?
      (let [settings (assoc settings
                       :settings/shown? false
                       :settings/auto (auto?))]
        (st/app-set-settings settings)
        (st/merge-ui-opts-settings settings)
        (status-msg ""))
      (let [start (tasks/deferred)
            finish (-> start
                     (chain-rmethod :list-settings-filenames)
                     (chain
                       (fn [filenames]
                         (if (vector? filenames)
                           (do
                             (status-msg "settings")
                             (st/app-merge-input-box {:input-box/id :xchar
                                                      :input-box/value xchar})
                             (st/app-merge-input-box {:input-box/id :ychar
                                                      :input-box/value ychar})
                             (st/app-merge-input-box {:input-box/id :filename
                                                      :input-box/value filename})
                             (st/app-merge-settings
                               {:settings/shown? true
                                :settings/auto (auto?)
                                :settings/filenames filenames}))
                           (status-msg "could not load settings filenames"))
                         true)))]
        (success! start true)
        finish))))

(defn settings-menu-hide
  "Hide the detailed settings menu"
  []
  (if (:settings/shown? (st/app-get-settings))
    (settings-menu)))

(declare execute-methods)

(defn help
  "Show brief command reminder"
  []
  (status-msg (apply str (interpose " /"
                           (cons "commands" (sort (keys execute-methods)))))))

(def execute-methods
  {"who" #'who
   "whoami" #'whoami
   "msg" #'msg
   "nick" #'nick
   "follow" #'follow
   "list" #'list-plans
   "show" #'show-plan-id
   "next" #'next-plan
   "prev" #'prev-plan
   "manual" #'manual
   "auto" #'auto
   "all-normal" #'all-normal
   "normal" #'color-normal
   "color" #'color-test
   "help" #'help-menu
   "settings" #'settings-menu
   "export" #'export
   "?" #'help})

(defn clear-cmd! []
  (st/app-merge-input-box {:input-box/id :cmd :input-box/value ""})
  nil)

;; returns [new-value new-start]
(defn execute [cmd]
  (if (string/starts-with? cmd "/")
    (let [args (string/split (subs cmd 1) #"\s")
          method-name (first args)
          args (doall (rest args))
          n (count args)
          method (get execute-methods method-name)
          method-fn (if method (deref method))
          arglist (if method (first (:arglists (meta method))))
          variadic (if arglist (vec-index-of arglist '&))
          arity (or variadic (and arglist (count arglist)) 0)]
      (if-not method
        (status-msg "Command \"" method-name "\" not found (try /?)")
        (if (or (and variadic (< n arity))
              (and (not variadic) (not= n arity)))
          (status-msg (str "Command \"" method-name "\" expects " arity " argument" (if (not= arity 1) "s")))
          (do
            ;; (println "EXECUTE" method-name "[" arity "]" args)
            (apply method-fn args)))))
    (my-user-action {:type :chat :chat cmd}))
  ["" 0])

;; return [new-value new-start]
(defn hide-menus
  "Hide menus"
  [& [value]]
  (println "HIDE-MENUS" value) ;; DEBUG
  (help-menu-hide) ;; remove help menu
  (settings-menu-hide)
  [(or value "") 0]) ;; remove settings menu

(defn input-box-key-fn [actions numeric?]
  (fn [& args]
    (let [[key id e] args
          ;; _ (println "IB KEY" key "ID" id) ;; DEBUG
          id-str (name id)
          k (keyword (str "settings/" id-str)) ;; key in settings
          elem (gdom/getElement id-str)
          start (.-selectionStart elem)
          end (.-selectionEnd elem)
          value (.-value elem)
          value-len (count value)
          ch? (= 1 (count key)) ;; regular character
          action (get actions key)
          [value new-start]
          (cond
            action
            (do
              (stop-propagation e)
              (action value))
            ch?
            (do
              (stop-propagation e)
              (if (>= start value-len)
                [(str value key) (inc start)]
                (if (zero? start)
                  [(str key value) (inc start)]
                  [(str (subs value 0 start) key (subs value start))
                   (inc start)])))
            (= key "Backspace")
            (do
              (stop-propagation e)
              (if (> end start) ;; delete selection
                (if (pos? start)
                  [(str (subs value 0 start) (subs value end))
                   start]
                  [(subs value end)
                   0])
                (if (pos? start)
                  (if (= start value-len)
                    [(subs value 0 (max (dec value-len) 0))
                     (dec start)]
                    [(str (subs value 0 (dec start)) (subs value start))
                     (dec start)])
                  [value start])))
            (and (= key "ArrowLeft") (pos? start))
            (do
              (stop-propagation e)
              [value (dec start)])
            (and (= key "ArrowRight") (< start value-len))
            (do
              (stop-propagation e)
              [value (inc start)])
            (and (= key "C-d") (< start value-len))
            (do
              (stop-propagation e)
              (if (zero? start)
                [(subs value 1) start]
                [(str (subs value 0 start) (subs value (inc start))) start]))
            (= key "C-a")
            (do
              (stop-propagation e)
              [value 0])
            (= key "C-e")
            (do
              (stop-propagation e)
              [value value-len])
            :else
            ;; NO (stop-propagation e)
            [value start])
          value (if numeric?
                      (if (and value (re-find #"^[\d.]+$" value))
                        (as-int value)
                        0)
                      value)]
      (st/app-merge-input-box {:input-box/id id :input-box/value value})
      ;; mutate settings key
      (st/app-merge-settings {k value})
      (when (<= new-start (inc value-len))
        (tasks/timeout #(.setSelectionRange elem new-start new-start) 10))
      nil)))

;; (defn input-box-enter-fn [id numeric?]
;;   (fn [value-str]
;;     (let [k (keyword (str "settings/" (name id)))
;;           value (if numeric?
;;                   (if (and value-str (re-find #"^[\d.]+$" value-str))
;;                     (as-int value-str)
;;                     10)
;;                   value-str)]
;;       (println "IB ENTER" id "VALUE" value)
;;       (st/merge-ui-opts-settings {k value})
;;       [value-str 0])))

(defn show-extra-keys [& args]
  (let [[key id e] args
        input-box (st/app-get-input-box id)
        {:keys [input-box/placeholder-orig]} input-box
        placeholder-orig (or placeholder-orig "")
        ektxt (keys/extra-keys-text)
        placeholder (str placeholder-orig (if (pos? (count ektxt)) "⎇ ") ektxt)]
    ;; (println "EXTRA KEY" key "ID" id "PLACEHOLDER" placeholder) ;; DEBUG
    (st/app-merge-input-box {:input-box/id id
                             :input-box/placeholder placeholder})))

(defn show-network [network]
  (st/show-network network)
  (reset))

(defn render-plans [rendering]
  (st/render-plans rendering)
  (status-msg "render-plans" rendering)
  (reset))

(defn vp-resize [[w h]]
  (println "vp" w "x" h) ;; DEBUG
  (st/app-merge-pan-zoom {:pan-zoom/vp-width w :pan-zoom/vp-height h})
  nil)

(defn vp-size []
  [(.-innerWidth js/window) (.-innerHeight js/window)])

(defn vp-listener [vp-timer]
  (when vp-timer
    ;; (println "Stopping previous vp listener:" vp-timer)
    (js/clearInterval vp-timer))
  (let [vp-timer
        (js/setInterval
          (fn []
            (let [{:keys [pan-zoom/vp-width pan-zoom/vp-height]}
                  (st/app-get :app/pan-zoom)
                  ;; [w h] (vp-size)
                  ;; for performance
                  w (.-innerWidth js/window)
                  h (.-innerHeight js/window)]
              (when (or (not= w vp-width) (not= h vp-height))
                (vp-resize [w h])))
            ) 1000)]
    ;; (println "Starting vp listener:" vp-timer) ;; DEBUG
    vp-timer))

(defn test-list-plans []
  (update-plan-list))

(defn edgeclick [edge]
  (let [{:keys [plan/plid edge/id edge/selected?]} edge
        sel [:edge (composite-key plid id)]]
    (if (keys/shift?)
      (if selected?
        (if (not (empty? (selection-subset [sel]
                           (vec (filter #(not= % sel)
                                  (st/app-get-plan-value plid :selection))))))
          ;; (println "NOT REMOVING" sel "is a subset of the current selection")
          (remove-from-selection plid sel {:edge edge}))
        (add-to-selection plid sel {:edge edge}))
      (set-selection plid sel {:edge edge}))))

(defn nodeclick [node]
  (let [{:keys [plan/plid node/id node/type
                node/htn-node node/outgoing node/selected?]} node
        sel [:node (composite-key plid id)]]
    ;; (println "ACTION NODECLICK" plid id "type" type "selected?" selected?)
    ;; "NODE" (remove-fn node))
    (cond
      (keyword-identical? type :state)
      (doseq [edge-id outgoing]
        (let [edge (st/plans-get-edge edge-id)]
          (if (keyword-identical? (:edge/type edge) :activity)
            (edgeclick edge))))
      (keyword-identical? type :htn-expanded-method)
      (do
        ;; (manual)
        (if (keys/shift?)
          (if selected?
            (remove-from-selection plid sel {:node node})
            (add-to-selection plid sel {:node node}))
          (set-selection plid sel {:node node}))
        )
      (begin? type)
      (if (keys/shift?)
        (let [selection (st/app-get-plan-value plid :selection)
              node-subset (selection-subset [sel]
                            (vec (filter #(not= % sel) selection)))
              subset-node (selection-subset selection [sel])]
          (if selected?
            (cond
              (not (empty? node-subset))
              (println "NOT REMOVING" sel
                "is a subset of the current selection")
              (not (empty? subset-node)) ;; remove subset
              (remove-from-selection plid (conj subset-node sel) {:node node})
              :else
              (remove-from-selection plid sel {:node node}))
            (cond
              (not (empty? node-subset))
              (println "NOT ADDING" sel "is a subset of the current selection")
              (not (empty? subset-node)) ;; remove subset
              (do
                (remove-from-selection plid subset-node {:node node})
                (add-to-selection plid sel {:node node})) ;; includes subset
              :else
              (add-to-selection plid sel {:node node}))))
        (set-selection plid sel {:node node}))
      :else
      (println "Clicking on" type "not yet supported!"))))

(defn show-tooltips
  "Show node/edge tooltips"
  []
  (status-msg "showing tooltips")
  (st/app-merge-settings {:settings/tooltips true})
  (st/merge-ui-opts-settings {:settings/tooltips true}))

(defn hide-tooltips
  "Hide node/edge tooltips"
  []
  (status-msg "hiding tooltips")
  (st/app-merge-settings {:settings/tooltips false})
  (st/merge-ui-opts-settings {:settings/tooltips false}))

(defn settings-action [id e]
  ;; (println "SETTINGS-ACTION ID" id) ;; DEBUG
  (cond
    (keyword-identical? id :settings-menu-close)
    (settings-menu) ;; was (settings-menu-hide)
    (#{:rewrap-htn-labels :auto :tooltips} id) ;; checkboxes
    (let [id-str (name id)
          q (str "#" id-str ":checked")
          qs (.querySelector js/document q)
          checked (boolean (and qs (.-value qs)))]
      ;; (println (str "  checked=" checked "=")) ;; DEBUG
      (cond
        (keyword-identical? id :rewrap-htn-labels)
        (st/app-merge-settings {:settings/rewrap-htn-labels checked})
        (keyword-identical? id :auto)
        (do
          (if (auto?)
            (if-not checked (manual))
            (if checked (auto)))
          (st/app-merge-settings {:settings/auto checked}))
        (keyword-identical? id :tooltips)
        (if checked (show-tooltips) (hide-tooltips))))
    (#{:filenames :load :save} id)
    (let [filenames (gdom/getElement "filenames")
          index (.-selectedIndex filenames)
          filename (.-value (aget (.-options filenames) index))
          settings (st/app-get-settings)]
      (if (not= filename (:settings/filename settings))
        (st/app-merge-settings {:settings/filename filename}))
      (when (keyword-identical? id :filenames)
        ;; (println "filenames" index "filename" filename) ;; DEBUG
        (st/app-merge-input-box {:input-box/id :filename
                                 :input-box/value filename}))
      (when (keyword-identical? id :load)
        (load-settings filename))
      (when (keyword-identical? id :save)
        (save-settings filename)))
    :else ;; font change
    (let [id-str (name id)
          value (.-value (gdom/getElement id-str))
          k (keyword (str "settings/" id-str))]
      ;; (println (str "  value=" value "=")) ;; DEBUG
      (st/app-merge-settings {k value})
      ))
  (stop-propagation e))

(defn add-url-options [options plid node-id edge-id]
  (let [url-config (st/app-get :app/url-config)
        planviz-url (make-url)]
    (loop [options options a (first url-config) more (rest url-config)]
      (if-not a
        (conj options
          (if node-id
            {:tag :info :text (str "✋ node information for " node-id)
             :fn info-menu-fn}
            {:tag :info :text (str "✋ edge information for " edge-id)
             :fn info-menu-fn}))
        (let [{:keys [plan node edge link url tab]} a
              link (str "⇨ " (or link url))
              tab (or tab "planviz")
              url (-> url
                    (string/replace "%planviz" planviz-url)
                    (string/replace "%plan" (name plid))
                    (string/replace "%node" (if node-id (name node-id) ""))
                    (string/replace "%edge" (if edge-id (name edge-id) "")))
              option (if (and (or (keyword-identical? plid plan) (keyword-identical? plan :|))
                           (or (and node-id (or (keyword-identical? node-id node) (keyword-identical? node :|)))
                             (and edge-id (or (keyword-identical? edge-id edge) (keyword-identical? edge :|)))))
                       {:tag :url :text link :url url :tab tab :fn url-menu-fn})
              options (if option (conj options option) options)]
          (recur options (first more) (rest more)))))))

(defn node-right-click [node]
  (let [{:keys [plan/plid node/id node/type node/aggregated? node/x node/y]} node
        ;; new-menu (if menu nil {:node node :x x :y y})
        options (cond
                  (begin? type)
                  (if aggregated?
                    [{:tag :tpn-show :text "⌬ TPN Show" :fn aggregation-menu-fn}]
                    [{:tag :tpn-hide :text "⌲ TPN Hide" :fn aggregation-menu-fn}])
                  (keyword-identical? type :htn-expanded-method)
                  (if aggregated?
                    [{:tag :htn-blur :text "⚄ HTN Show all" :fn focus-menu-fn}]
                    [{:tag :htn-focus :text "⚀ HTN Focus here" :fn focus-menu-fn}])
                  :else
                  [])
        menu {:node node :x x :y y
              :options (add-url-options options plid id nil)}]
    (println "ACTION NODE-RIGHT-CLICK" plid id "type" type "x" x "y" y)
    (st/merge-ui-opts {:ui/menu menu})))

(defn edge-right-click [edge]
  (let [{:keys [plan/plid edge/id edge/type edge/from edge/to edge/network-flows]} edge
        ;; new-menu (if menu nil {:node node :x x :y y})
        ;; menu {:node node :x x :y y}
        [x0 y0] [(:node/x from) (:node/y from)]
        [x1 y1] [(:node/x to) (:node/y to)]
        x (/ (+ x0 x1) 2)
        y (/ (+ y0 y1) 2)
        options (if (and network-flows (pos? (count network-flows)))
                  [{:tag :tpn-network-flows
                    :text "⅏ Show activity on network" :fn info-menu-fn}]
                  [])
        menu {:edge edge :x x :y y
              :options (add-url-options options plid nil id)}]
    (println "ACTION EDGE-RIGHT-CLICK" plid id "type" type)
      ;; "x0" x0 "y0" y0 "x1" x1 "y1" y1)
    (st/merge-ui-opts {:ui/menu menu})))

(defn graph-click [x e]
  (let [button (.-button e)
        browser-contextmenu? false
        menu (:ui/menu (st/get-ui-opts))
        target (.-target e)]
    (if menu
      (do
        (st/merge-ui-opts {:ui/menu nil})
        true) ;; allow real right click
      (do
        (case button
          0 (do
              (stop-propagation e)
              (cond
                (:edge/type x)
                (edgeclick x)
                (:node/type x)
                (nodeclick x)
                :else
                (clear-selection (:ui/show-plan (st/get-ui-opts)))))
          1 (println "middle button not used")
          2 (do
              (stop-propagation e)
              (cond
                (:edge/type x)
                (edge-right-click x)
                (:node/type x)
                (node-right-click x)
                :else
                (println "right click background")))
          true))))) ;; odd button case, let through

(def app-bindings
  {"x" #'zoom-out
   "z" #'zoom-in
   "ArrowRight" #'pan-right
   "ArrowUp" #'pan-up
   "ArrowLeft" #'pan-left
   "ArrowDown" #'pan-down
   "A-ArrowRight" #'next-plan
   "A-ArrowLeft" #'prev-plan
   "C-ArrowRight" #'next-plan
   "C-ArrowLeft" #'prev-plan
   "-" #'zoom-out
   "=" #'zoom-in
   "?" #'help-menu
   "g" #'clear-plans
   "p" #'list-plans
   "s" #'settings-menu
   "1" #'reset
   "6" #'update-plan-list
   "t" #'show-tooltips
   "T" #'hide-tooltips
   "Escape" #'hide-menus})

(defn app-key-fn [& args]
  (let [[key id e] args
        key-fn-var (get app-bindings key)
        key-fn (if key-fn-var (deref key-fn-var))]
    ;; (println "APP KEY" key "ID" id) ;; DEBUG
    (if key-fn
      (key-fn))))

(defn initialize-help []
  (let [help {:help/shown false :help/help-click help-click}
        cmds (sort (keys execute-methods))
        commands (concatv
                   [[:i "━━━ commands ━━━"][:br]]
                   (apply concat
                     (for [cmd cmds
                           :let [meta-data (meta (get execute-methods cmd))
                                 arglist (first (:arglists meta-data))
                                 syms (remove #{'&} arglist)
                                 details (:details meta-data)
                                 doc [:li (:doc meta-data)]
                                 args (for [sym syms]
                                        [:li [:i (name sym)] " "
                                         (get details (keyword sym))])]]
                       [[:b (str "/" cmd)] " "
                        (apply str (interpose " " arglist))
                        [:br]
                        (concatv [:ul doc] args)])))
        key-bindings (concatv
                       [[:i "━━━ key-bindings ━━━"][:br]]
                       (apply concat
                         (for [f (sort-by #(:name (meta %))
                                   (set (vals app-bindings)))
                               :let [meta-data (meta f)]]
                           [[:b (name (:name meta-data))] " " (:doc meta-data)
                            " ⇛ "
                            (apply concatv
                              [:span]
                              (interpose [", "]
                                (for [k (sort (mapv first
                                                (filter #(= (second %) f)
                                                  app-bindings)))]
                                  [[:i k]])))
                            [:br]])))
        content (concatv commands [[:br]] key-bindings)]
    (st/app-set-help (assoc help :help/content content))))

(defn initialize-settings []
  (let [settings (assoc ui/planviz-settings
                   :settings/shown? false
                   :settings/settings-action settings-action)]
    (st/app-set-settings settings)
    (st/set-ui-opts-settings settings)))

(defn vp-initialize []
  (let [initialized (st/app-get :app/initialized)
        vp-timer (st/app-get :app/vp-timer)
        initialized (inc initialized)
        vp-timer (vp-listener vp-timer)]
    (st/app-set :app/initialized initialized)
    (st/app-set :app/vp-timer vp-timer)
    (vp-resize (vp-size))
    initialized))

(defn initialize []
  (let [escape-action {"Escape" hide-menus}]
    (keys/register-key-fn :cmd
      (input-box-key-fn (assoc escape-action "Enter" execute) false))
    (keys/register-key-fn :xchar (input-box-key-fn escape-action true))
    (keys/register-key-fn :ychar (input-box-key-fn escape-action true))
    (keys/register-key-fn :filename (input-box-key-fn escape-action false))
    (keys/register-key-fn :extra show-extra-keys)
    (keys/register-key-fn :default app-key-fn)
    (set! (.-onkeydown js/window) keys/keydown)
    (set! (.-onkeyup js/window) keys/keyup)
    (initialize-help)
    (initialize-settings)
    (st/add-plans-root! graph-click)
    (st/add-app-root!)
    (vp-initialize))) ;; get and track viewport size
