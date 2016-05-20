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
            [webtasks.tasks :as tasks
             :refer [on-realized chain sleep success! error!]]
            [webtasks.ws :as ws]
            [webkeys.keys :as keys]
            [planviz.ui :as ui :refer [node-key-fn edge-key-fn network-key-fn
                                       activity? begin?]]
            [planviz.state :as st]
            [planviz.tplan :as tplan :refer [min-max-fn]]
            [plan-schema.core :as pschema :refer [composite-key]]
            [goog.dom :as gdom]
            [goog.crypt.base64 :as base64]
            [goog.net.XhrIo :as xhr]))

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

(def edge-states [:normal
                  :impossible ;; grey
                  :start ;; orange  (previously negotiation)
                  :negotiation ;; orange
                  :best ;; magenta
                  :active ;; blue
                  :started ;; blue (previously active)
                  :finished ;; green
                  :failed]) ;; red

(def node-states [:normal ;; white ????
                  :best ;; magenta
                  :impossible ;; grey
                  :reached ;; green
                  :failed ;; red
                  :started]) ;; blue (for HTN's)

(defn rmethod [{:keys [return success-fn error-fn] :as opts} method & args]
  (let [return (or return (tasks/deferred))
        success-fn (or success-fn
                     #(println "ANSWER" method "->"  %))
        error-fn (or error-fn
                   #(println "ERROR" method "->"  %))]
    ;; (println "RMETHOD" return method args)
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
              sel (if (= type :node)
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
  (let [action (if (or (:plid action) (= (:type action) :chat)) action
                   (assoc action
                     :plid (:ui/show-plan (st/get-ui-opts))))]
    ;; (println "MY USER-ACTION" action)
    ;; (rmethod {:success-fn generic-reply :error-fn generic-reply}
    (rmethod {}
      :user-action action)))

(defn update-tpn-end [plid]
  (let [end (if (= (st/app-get-plan-value plid :type) :tpn-network)
              (st/get-network-end plid))]
    (when end
      ;; (println "TPN-END" plid end)
      (st/app-set-plan-value plid :tpn-end end))
    true)) ;; for deferred

(defn new-plan [plan]
  (let [plan-id (first (keys (:plan/by-plid plan)))
        start (tasks/deferred)
        finish (-> start
                 (chain tplan/layout)
                 (chain st/new-plan)
                 (chain update-tpn-end))]
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

(defn network-update [update]
  (let [d (st/app-get :app/loading)
        defer (or (st/app-get :app/defer) [])]
    (if d
      (st/app-set :app/defer (conj defer update))
      (let [{:keys [plid update-uid state]} update
            {:keys [loaded? tpn-end corresponding]} (st/app-get-plan plid)]
        (if-not loaded?
          (println "ERROR network-update for not loaded plan:" plid)
          (do ;; if it has node in the uid then it's node, else edge
            (if (not (nil? (string/index-of (name update-uid) "node")))
              (let [node-id (composite-key plid update-uid)
                    node (st/plans-get-node node-id)
                    {:keys [node/htn-node]} node
                    hpt-hent (if (and corresponding htn-node)
                               (st/plans-get-node htn-node))
                    {:keys [node/type node/parent]} hpt-hent
                    network-parent (st/get-network-parent parent)
                    hem (if (= type :htn-expanded-method)
                          hpt-hent
                          (if network-parent (st/plans-get-node network-parent)))
                    hem-id (:node/id hem)]
                (when (not= (:node/state node) state)
                  (st/plans-merge-node {:plan/plid plid :node/id update-uid
                                        :node/state state}))
                (when hem-id
                  (if (not= (:node/state hem) state)
                    (st/plans-merge-node {:plan/plid corresponding
                                          :node/id hem-id
                                          :node/state state})))
                (when (and (= node-id tpn-end) (= state :reached))
                  (status-msg "finished" plid)))
              (let [edge (st/plans-get-edge (composite-key plid update-uid))
                    {:keys [edge/htn-node]} edge
                    hpt-hent (if (and corresponding htn-node)
                               (st/plans-get-node htn-node))
                    {:keys [node/parent]} hpt-hent
                    network-parent (st/get-network-parent parent)
                    hem-id network-parent
                    hem (st/plans-get-node hem-id)]
                (when (not= (:edge/state edge) state)
                  (st/plans-merge-edge {:plan/plid plid
                                        :edge/id update-uid
                                        :edge/state state}))
                (when hem-id
                  (if (not= (:node/state hem) state)
                    (st/plans-merge-node {:plan/plid corresponding
                                          :node/id hem-id
                                          :node/state state})))))))))))

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
        (when (and (= state :reached)
                (= (composite-key plid update-uid) tpn-end))
          (status-msg "finished" plid))))))

(defn apply-network-updates [success?]
  (let [defer (or (st/app-get :app/defer) [])]
    (when (and success? (pos? (count defer)))
      (println "APPLYING" (count defer) "deferred network updates...")
      (doseq [update defer]
        (network-update update)))
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
                     (status-msg "loaded" plan-id)
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
                         (sleep 5) ;; pace the loading
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

(defn list-plans []
  (let [plan-ids (vec (sort (keys (or (st/app-get :app/plans) {}))))]
    (apply status-msg "plans " plan-ids)))

;; may have value passed in from previous thing in chain
(defn update-plan-list [ & [v]]
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

(defn generic-reply [message]
  (if (:error message)
    (status-msg "ERROR" (:error message))
    (status-msg message)))

(defn whoami []
  (println "whoami")
  (rmethod {:success-fn generic-reply :error-fn generic-reply}
    :whoami))

(defn who []
  (println "who")
  (rmethod {:success-fn generic-reply :error-fn generic-reply}
    :who))

(defn nick [nickname]
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

(defn follow [nickname]
  (println "FOLLOW" nickname)
  (st/app-set :app/following (if (not= nickname "-") nickname))
  (rmethod {:success-fn follow-success :error-fn follow-error}
    :follow nickname))

(defn unfollow []
  (follow "-"))

(defn auto? []
  (= (or (st/app-get :app/mode) :manual) :auto))

(defn manual []
  (when (auto?)
    (st/app-set :app/mode :manual)
    (unfollow)
    (status-msg "switching to manual mode")))

(defn auto []
  (when-not (auto?)
    (st/app-set :app/mode :auto)
    (status-msg "switching to automatic mode")))

(defn zoom! [z]
  (st/app-merge-pan-zoom {:pan-zoom/zoom z})
  nil)

(def max-zoom 256.0)
(def min-zoom 0.5)

(defn zoom-in []
  (let [zoom (:pan-zoom/zoom (st/app-get :app/pan-zoom))
        zoom (* zoom 1.2)
        zoom (min (max zoom min-zoom) max-zoom)]
    (zoom! zoom)
    (my-user-action {:type :pan-zoom :zoom zoom})))

(defn zoom-out []
  (let [zoom (:pan-zoom/zoom (st/app-get :app/pan-zoom))
        zoom (* zoom 0.8)
        zoom (min (max zoom min-zoom) max-zoom)]
    (zoom! zoom)
    (my-user-action {:type :pan-zoom :zoom zoom})))

(defn pan! [p]
  (st/app-merge-pan-zoom {:pan-zoom/pan p})
  nil)

(defn pan-left []
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

(defn pan-right []
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

(defn pan-up []
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

(defn pan-down []
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

(defn reset [&[opts]]
  (let [from-auto? (:auto opts)]
    (pan! [0.0 0.0])
    (zoom! 1.0)
    (if-not from-auto?
      (my-user-action {:type :pan-zoom
                       :pan [0.0 0.0] :zoom 1.0}))
    true)) ;; for deferreds

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

(defn is-within? [sel id]
  (let [node (st/plans-get-node id)
        {:keys [node/end]} node
        sel-id (second sel)
        within (atom (= sel-id end))]
    (if-not @within
      (visit-nodes id #{end}
        (fn [node]
          (if (= sel-id (node-key-fn node))
            (do
              (reset! within true)
              nil)
            (map-outgoing node
              (fn [edge]
                (if (= sel-id (edge-key-fn edge))
                  (do
                    (reset! within true)
                    nil)
                  (let [{:keys [edge/type edge/to
                                edge/weight edge/hidden]} edge]
                    (if (activity? type)
                      (node-key-fn to))))))))))
    @within))

;; return the parts of sub in selection
(defn selection-subset [sub selection]
  (loop [a-subs [] a (first sub) a-more (rest sub)]
    (if-not a
      a-subs
      (let [within? (loop [b (first selection) b-more (rest selection)]
                      (if-not b
                        false
                        (if (or (= a b) (and (= (first b) :node)
                                          (is-within? a (second b))))
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
            edge (if (= id o-id)
                   edge (st/plans-get-edge id))
            {:keys [edge/selected? edge/from edge/to]} edge]
        (when (not= selected? new-selected?)
          (st/plans-merge-edge (assoc
                                 (dissoc edge :plans/ui-opts
                                   :edge/from :edge/to)
                                 :edge/selected? new-selected?))
          (st/plans-merge-node (assoc from :node/selected? new-selected?))
          (st/plans-merge-node (assoc to :node/selected? new-selected?))
          )
        )
      :node
      (let [o-id (node-key-fn node)
            node (if (= id o-id)
                   node (st/plans-get-node id))
            {:keys [node/end node/type node/selected?]} node]
        (when (not= selected? new-selected?)
          (st/plans-merge-node (assoc
                                 (dissoc node :plans/ui-opts
                                   :node/tpn-selection)
                                 :node/selected? new-selected?))
          (when (begin? type)
            (st/plans-merge-node (assoc
                                   (dissoc (st/plans-get-node end)
                                     :plans/ui-opts)
                                   :node/selected? new-selected?))
            (visit-nodes id #{end}
              (fn [node]
                (st/plans-merge-node (assoc
                                       (dissoc node :plans/ui-opts)
                                       :node/selected? new-selected?))
                (map-outgoing node
                  (fn [edge]
                    (let [{:keys [edge/type edge/to
                                  edge/weight edge/hidden]} edge]
                      (when (activity? type)
                        (st/plans-merge-edge (assoc
                                               (dissoc edge
                                                 :plans/ui-opts
                                                 :edge/from :edge/to)
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
    (when-not (some #(= % sel) selection)
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
    (let [selection (st/app-get-plan-value plid :selection)
          multiple? (vector? (first sel))
          new-selection (if (or multiple? (empty? sel)) sel [sel])]
      (when (not= new-selection selection)
        (when (and (vector? selection) (pos? (count selection)))
          (doseq [sel selection]
            (when-not (some #(= % sel) new-selection)
              (set-selected? false plid sel opts))))
        (doseq [sel new-selection]
          (when-not (some #(= % sel) selection)
            (set-selected? true plid sel opts))))
      (plan-selection plid new-selection opts))))

;; returns proposed HTN selection
(defn highlight-from-tpn [htn-plid tpn-plid csel]
  (loop [sel #{} c (first csel) more (rest csel)]
    (if-not c
      (vec sel)
      (let [[type id] c
            ;; _ (println "Corresponding selection" c "in TPN" tpn-plid)
            s (case type
                :edge
                (let [edge (st/plans-get-edge id)
                      {:keys [edge/htn-node]} edge
                      ;; _ (println "htn-node" htn-node)
                      hpt-hent (if htn-node (st/plans-get-node htn-node))
                      {:keys [node/parent]} hpt-hent
                      ;; _ (println "hpt-hent parent" parent)
                      network-parent (st/get-network-parent parent)
                      hem (if network-parent (st/plans-get-node network-parent))
                      hem-id (:node/id hem)]
                  ;; (println "HEM" hem-id)
                  ;; (add-to-selection htn-plid [:node hem-id] {:node hem})
                  (if hem-id [:node (composite-key htn-plid hem-id)]))
                :node
                (let [node (st/plans-get-node id)
                      {:keys [node/htn-node]} node
                      ;; _ (println "htn-node" htn-node)
                      hpt-hent (if htn-node (st/plans-get-node htn-node))
                      {:keys [node/type node/parent]} hpt-hent
                      network-parent (st/get-network-parent parent)
                      hem (if (= type :htn-expanded-method)
                            hpt-hent
                            (if network-parent
                              (st/plans-get-node network-parent)))
                      hem-id (:node/id hem)]
                  ;; (println "HEM" hem-id)
                  ;; (add-to-selection htn-plid [:node hem-id] {:node hem})
                  (if hem-id [:node (composite-key htn-plid hem-id)]))
                (do
                  (println "highlight of TPN" type "not supported")))]
        (recur (if s (conj sel s) sel) (first more) (rest more))))))

;; return the minimal representation of sel
;; first find any leader nodes among the nodes
;; then add in any non comprised edges
(defn minimal-selection [sel]
  ;; (println "MIN SEL" sel)
  (let [mnodes (vec (remove nil? (filter #(= :node (first %)) sel)))
        medges (remove nil? (filter #(= :edge (first %)) sel))
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
    (if-not c
      ;; already pre-computed to be minimal!
      ;; (minimal-selection (vec sel))
      (vec sel)
      (let [[type id] c
            _ (println "Corresponding selection" c "in HTN" htn-plid)
            sel (case type
                  :node
                  (let [node (st/plans-get-node id)
                        {:keys [;; node/tpn-edge node/tpn-node
                                node/tpn-selection]} node
                        tsel (atom sel)]
                    ;; (println "DEBUG tpn-selection" tpn-selection)
                    (doseq [s tpn-selection]
                      (if (:node/id s)
                        (swap! tsel conj [:node (second (get-begin-node-ref
                                                          (node-key-fn s)))])
                        (swap! tsel conj [:edge (edge-key-fn s)]))
                      )
                    @tsel
                   )
                  (do
                    (println "highlight of HTN" type "not supported")
                    sel))]
        (recur sel (first more) (rest more))))))

;; if there is a relevant selection.. show it here!
;; if this is an htn with a corresponding tpn AND
;; the tpn has a selection... make the highlight now!
(defn highlight-relevant [&[remote]]
  (let [{:keys [ui/show-plan ui/show-network ui/network-type]} (st/get-ui-opts)
        {:keys [corresponding selection]} (st/app-get-plan show-plan)
        csel (if corresponding (st/app-get-plan-value corresponding :selection))]
    (println "HIGHLIGHT-RELEVANT" show-plan show-network network-type)
    (case network-type
      :hem-network
      (if (auto?)
        (let [sel (if (and (vector? csel) (pos? (count csel)))
                    (highlight-from-tpn show-plan corresponding csel)
                    [])]
          (println "CSEL" csel "SEL" sel)
          (if remote
            (status-msg "Showing TPN correspondence from" remote)
            (status-msg "Showing TPN correspondence"))
          (set-selection show-plan sel {:auto true}))
        (status-msg "Not showing TPN correspondence in manual mode"))
      :tpn-network
      (if (auto?)
        (let [sel (if (and (vector? csel) (pos? (count csel)))
                    (highlight-from-htn show-plan corresponding csel)
                    [])]
          (println "CSEL" csel "SEL" sel)
          (if remote
            (status-msg "Showing HTN correspondence from" remote)
            (status-msg "Showing HTN correspondence"))
          (set-selection show-plan sel {:auto true}))
        (status-msg "Not showing HTN correspondence in manual mode"))
      true)))

(declare load-plan)

(defn display-plan [plan-id &[opts]]
  ;; (println "ACTIONS/DISPLAY-PLAN" plan-id "OPTS" opts)
  (let [plan-id (if (string? plan-id)
                  (keyword (string/replace-first plan-id  #"^:" ""))
                    plan-id)
        {:keys [loaded? type corresponding n-keys]} (st/app-get-plan plan-id)
        load-tpn? (and (keyword-identical? type :htn-network) corresponding
                    (not (st/app-get-plan-value corresponding :loaded?)))
        tooltips? (< n-keys 256)]
    (if loaded?
      (do
        (status-msg "display-plan" plan-id)
        (st/app-set :app/title (name plan-id))
        (st/tooltips tooltips?)
        (st/show-plan plan-id)
        (reset opts)
        (highlight-relevant))
      (if load-tpn?
        (-> (load-plan corresponding) ;; LOAD TPN first!
          (sleep 5) ;; pace the browser
          (chain #(load-plan plan-id opts))
          (sleep 5) ;; pace the browser
          (chain #(display-plan plan-id opts))
          (tasks/catch #(do
                          (status-msg "unable to show" plan-id)
                          (st/loading false)
                          (st/app-set :app/loading nil))))
        (-> (load-plan plan-id opts)
          (sleep 5) ;; pace the browser
          (chain #(display-plan plan-id opts))
          (tasks/catch #(do
                          (status-msg "unable to show" plan-id)
                          (st/loading false)
                          (st/app-set :app/loading nil))))))
    true)) ;; for deferreds

(defn load-plan [plan-id &[opts]]
  (let [d (tasks/deferred)
        dtimeout (tasks/timeout! d 60000 :timed-out)
        {:keys [loaded? n-keys parts n-parts]} (st/app-get-plan plan-id)
        have-parts (if parts (count parts) 0)
        reload? (:reload? opts)]
    ;; (println "LOAD-PLAN" plan-id "OPTS" opts)
    (cond
      (not n-keys)
      (do
        (println "cannot load unknown plan" plan-id)
        (error! d (str "cannot load unknown plan " plan-id)))
      (and loaded? (not reload?))
      (do
        (success! d (str "already loaded plan " plan-id)))
      (st/app-get :app/loading)
      (do
        (println "cannot load " plan-id
          " because a load is already in progress...")
        (error! d (str "cannot load " plan-id
                    " because a load is already in progress...")))
      :else
      (do
        (status-msg "loading" plan-id)
        (st/loading true)
        (st/app-set :app/loading d)
        (request-plan-part plan-id have-parts)))
    dtimeout))

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
  (let [dlogin (tasks/connect
                 (rmethod {} :login)
                 (tasks/deferred))
        finish (-> dlogin
                 (chain #(if (:error %) false (:login/remote %)))
                 (chain #(do (status-msg "logged in as" %)
                             (st/app-set :app/client %)
                             true))
                 (chain update-plan-list)
                 ;; (chain load-all-plans)
                 (chain #(do (status-msg "ready") true))
                 (tasks/catch #(do (st/app-set :app/client nil)
                                   (status-msg "login failed"))))]
    finish))

(defn recv-msg [line]
  (status-msg line))

(defn msg [& args]
  (let [user (first args)
        comment (apply str (interpose " " (rest args)))]
    (if-not user
      (status-msg "must specify user: /msg USER your message here")
      (rmethod {;; :return d-recv-msg
                :success-fn recv-msg :error-fn generic-reply}
        :msg user comment))))

(defn no-plan? [showing]
  (or (not showing) (#{:all :none :loading} showing)))

(defn user-action [action]
  (let [client (st/app-get :app/client)
        {:keys [planviz remote nick type plid selection pan zoom chat
                tag node edge url tab info network-flows]} action
        {:keys [loaded?]} (st/app-get-plan plid)
        showing (:ui/show-plan (st/get-ui-opts))
        ready? (or (not (auto?)) (not plid) loaded?)
        to-show (if (no-plan? showing) plid showing)
        auto-on? (auto?)]
    (if ready?
      (do
        (case type
          :selection
          (when (and (not= remote client) auto-on?)
            (println "REMOTE SET-SELECTION" action)
            (set-selection plid selection {:auto true})
            (when (not= showing plid)
              (highlight-relevant (or nick remote))))
          :pan-zoom
          (when (and (not= remote client) auto-on? (= plid showing))
            ;; (println "REMOTE PAN-ZOOM" pan zoom)
            (if pan (pan! pan))
            (if zoom (zoom! zoom))
            (status-msg "following view of" (or nick remote))
            )
          :chat
          (status-msg (str "<" (or nick remote) "> " chat))
          :menu
          (when (and (not= remote client) auto-on?)
            (cond
              (keyword-identical? tag :info)
              (println "USER-ACTION type" type "TAG" tag
                "NODE" node "EDGE" edge "INFO" info)
              (keyword-identical? tag :url)
              (println "USER-ACTION type" type "TAG" tag
                "URL" url "TAB" tab)
              (#{:htn-focus :htn-blur} tag)
              (do
                (println "USER-ACTION type" type "TAG" tag
                  "NODE" node)
                (st/app-set-plan-value plid :focus
                  (if (keyword-identical? tag :htn-focus) node)))
              (#{:tpn-show :tpn-hide} tag)
              (println "USER-ACTION type" type "TAG" tag
                "NODE" node)
              (keyword-identical? tag :tpn-network-flows)
              (println "USER-ACTION type" type "TAG" tag
                "NODE" node "NETWORK-FLOWS" network-flows)
              :else
              (println "USER-ACTION type" type "TAG" tag "not supported")))
          (println "USER-ACTION type" type "not supported"))
        true) ;; for deferred
      (-> (load-plan plid) ;; loading on demand
        (sleep 5) ;; pace the browser
        (chain #(display-plan to-show))))))

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
      (when-not (or (= show-plan plid) (= show-plan corresponding))
        (display-plan plid))
      (-> (load-plan plid) ;; loading on demand
        (sleep 5) ;; pace the browser
        (chain #(display-plan plid))))))

;; set the state of all nodes and edges to normal in the
;; selected and corresponding plan(s)
(defn all-normal []
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

(defn next-plan []
  (another-plan inc))

(defn prev-plan []
  (another-plan dec))

(defn clear-plans []
  (status-msg "clear-plans")
  (st/clear-plans)
  (reset))

(defn color-normal []
  (let [{:keys [ui/show-plan ui/show-network]} (st/get-ui-opts)]
    (if (or (not show-network) (= show-network :all))
      (status-msg "cannot run color-test for network:" show-network)
      (let [network (st/get-network show-network)
            {:keys [network/nodes network/edges]} network]
        (doseq [node nodes]
          (st/plans-merge-node
            (assoc node :node/state :normal)))
        (doseq [edge edges]
          (if (keyword-identical? (:edge/type edge) :activity)
            (st/plans-merge-edge
              (assoc (dissoc edge :edge/from :edge/to)
                :edge/state :normal))))))))

(defn color-test []
  (let [{:keys [ui/show-plan ui/show-network]} (st/get-ui-opts)]
    (if (or (not show-network) (= show-network :all))
      (status-msg "cannot run color-test for network:" show-network)
      (let [network (st/get-network show-network)
            {:keys [network/nodes network/edges]} network
            node-states-n (count node-states)
            edge-states-n (count edge-states)
            i (atom 0)]
        (doseq [node nodes]
          (st/plans-merge-node
            (assoc node :node/state
              (get node-states (mod (swap! i inc) node-states-n)))))
        (doseq [edge edges]
          (if (keyword-identical? (:edge/type edge) :activity)
            (st/plans-merge-edge
              (assoc (dissoc edge :edge/from :edge/to)
                :edge/state (get edge-states
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
  (into-array (map #(.charCodeAt % 0) s)))

(defn create-data-url [data-str mime-type]
  (let [data (string->octet-array data-str)
        data-base64 (base64/encodeByteArray data)]
    (str "data:" mime-type ";base64," data-base64)))

(defn export-plan [d]
  (let [css (st/app-get :app/css)
        {:keys [ui/show-plan]} (st/get-ui-opts)
        filename (str (name show-plan) ".svg")
        application (gdom/getElement "application")
        plans (gdom/getElement "plans")
        svg (.-innerHTML plans)
        lt (inc (string/index-of svg ">"))
        svg-header "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
        svg-begin "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"\n "
        css-begin "\n<style type=\"text/css\">\n<![CDATA[\n"
        css-end "\n]]>\n</style>\n"
        svg (str svg-header svg-begin (subs svg 4 lt)
              css-begin css css-end
              (string/replace (subs svg lt) #">" ">\n"))
        dataurl (create-data-url svg "image/svg+xml")
        a (.createElement js/document "a")]
    (println "exporting" show-plan "as" filename)
    (set! (.-id a) "download")
    (set! (.-download a) filename)
    (set! (.-href a) dataurl)
    (.appendChild application a)
    (.click a)
    (tasks/timeout #(.removeChild application a) 5)
    true))

(defn export []
  (let [{:keys [ui/show-plan]} (st/get-ui-opts)]
    (if (no-plan? show-plan)
      (status-msg "cannot export: no plan shown")
      (let [start (tasks/deferred)
            finish (-> start
                     (chain get-css)
                     (chain export-plan))]
        (success! start true)
        finish))))

(declare help)

(def execute-methods
  {"who" #'who
   "whoami" #'whoami
   "msg" #'msg
   "nick" #'nick
   "follow" #'follow
   "list" #'list-plans
   "show" #'display-plan
   "next" #'next-plan
   "prev" #'prev-plan
   "manual" #'manual
   "auto" #'auto
   "all-normal" #'all-normal
   "normal" #'color-normal
   "color" #'color-test
   "help" #'help
   "export" #'export
   "?" #'help})

(defn help []
  (status-msg (apply str (interpose " /"
                           (cons "commands" (sort (keys execute-methods)))))))

(defn execute [cmd]
  (if (string/starts-with? cmd "/")
    (let [args (string/split (subs cmd 1) #"\s")
          method-name (first args)
          args (rest args)
          method (get execute-methods method-name)
          method-fn (if method (deref method))
          arity (if method (first (:arglists (meta method))))
          arity (if arity
                  (if-let [a (vec-index-of arity '&)]
                    (if (zero? a) :var a)
                    (count arity)))]
      (if-not method
        (status-msg "Command \"" method-name "\" not found (try /?)")
        (if (and (not= :var arity) (not= (count args) arity))
          (status-msg "Command \"" method-name "\" expects" arity "arguments")
          (do
            ;; (println "EXECUTE" method-name "[" arity "]" args)
            (apply method-fn args)))))
    (my-user-action {:type :chat :chat cmd})))

(defn cmd-key-fn [key]
  (let [cmde (gdom/getElement "cmd")
        start (.-selectionStart cmde)
        cmd (.-value cmde)
        cmd-len (count cmd)
        ch? (= 1 (count key)) ;; regular character
        [cmd new-start];
        (if ch?
          (if (>= start cmd-len)
            [(str cmd key) (inc start)]
            (if (zero? start)
              [(str key cmd) (inc start)]
              [(str (subs cmd 0 start) key (subs cmd start)) (inc start)]))
          (cond
            (= key "Enter")
            (do
              (execute cmd)
              ["" 0])
            (and (= key "Backspace") (pos? start))
            (if (= start cmd-len)
              [(subs cmd 0 (max (dec cmd-len) 0)) (dec start)]
              [(str (subs cmd 0 (dec start)) (subs cmd start)) (dec start)])
            (and (= key "ArrowLeft") (pos? start))
            [cmd (dec start)]
            (and (= key "ArrowRight") (< start cmd-len))
            [cmd (inc start)]
            (and (= key "C-d") (< start cmd-len))
            (if (zero? start)
              [(subs cmd 1) start]
              [(str (subs cmd 0 start) (subs cmd (inc start))) start])
            (= key "C-a")
            [cmd 0]
            (= key "C-e")
            [cmd cmd-len]
            :else
            [cmd start]))]
    (st/app-merge-input-box {:input-box/value cmd :input-box/start new-start})
    (when (<= new-start (inc cmd-len))
      (tasks/timeout #(.setSelectionRange cmde new-start new-start) 10))
    nil))

(defn show-extra-keys []
  (let [ektxt (keys/extra-keys-text)
        placeholder (str st/input-box-placeholder
                      (if (pos? (count ektxt)) "⎇ ")
                      ektxt)]
    (st/app-merge-input-box {:input-box/placeholder placeholder})))

(defn clear-cmd! []
  (st/app-merge-input-box {:input-box/value "" :input-box/start 0})
  nil)

(defn show-network [network]
  (st/show-network network)
  (reset))

(defn show-all []
  (st/show-network :all)
  (st/show-plan :all)
  (status-msg "show-plan" :all)
  (reset))

(defn render-plans [rendering]
  (st/render-plans rendering)
  (status-msg "render-plans" rendering)
  (reset))

(defn vp-resize [[w h]]
  (println "vp" w "x" h)
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
    ;; (println "Starting vp listener:" vp-timer)
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
      (= type :state)
      (doseq [edge-id outgoing]
        (let [edge (st/plans-get-edge edge-id)]
          (if (keyword-identical? (:edge/type edge) :activity)
            (edgeclick edge))))
      (= type :htn-expanded-method)
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

(defn show-tooltips []
  (status-msg "showing tooltips")
  (st/tooltips true))

(defn hide-tooltips []
  (status-msg "hiding tooltips")
  (st/tooltips false))

(defn menu-click-handled [e]
  (st/merge-ui-opts {:ui/menu nil})
  (.preventDefault e)
  (.stopPropagation e)
  ;; (my-user-action action)
  false)

(defn default-menu-fn [node edge option e]
  (cond
    node
    (let [{:keys [tag]} option
          {:keys [plan/plid node/id node/type node/state]} node
          info (str tag ", node " id ", type " type ", state " state)]
      (status-msg info)
      (my-user-action {:type :menu :tag tag :plid plid :node id :info info}))
    edge
    (let [{:keys [tag]} option
          {:keys [plan/plid edge/id edge/type edge/state edge/network-flows]} edge
          network-flows (if (keyword-identical? tag :tpn-network-flows) network-flows)
          info (str tag ", edge " id ", type " type ", state " state)]
      (status-msg info)
      (my-user-action (assoc-if {:type :menu :tag tag :plid plid :edge id}
                        :info (if-not network-flows info)
                        :network-flows network-flows)))
    :else
    (let [{:keys [tag]} option
          info (str tag ", no node or edge selected")]
      (status-msg info)
      (my-user-action {:type :menu :tag tag :info info})))
  (menu-click-handled e))

(defn info-menu-fn [node edge option e]
  (cond
    node
    (let [{:keys [tag]} option
          {:keys [plan/plid node/id node/type node/state]} node
          info (str "node " id ", type " type ", state " state)]
      (status-msg info)
      (my-user-action {:type :menu :tag tag :plid plid :node id :info info}))
    edge
    (let [{:keys [tag]} option
          {:keys [plan/plid edge/id edge/type edge/state]} edge
          info (str "edge " id ", type " type ", state " state)]
      (status-msg info)
      (my-user-action {:type :menu :tag tag :plid plid :edge id :info info}))
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

(defn set-aggregated? [begin end aggregated]
  ;; (println "DEBUG set-aggregated?" begin end aggregated)
  (visit-nodes begin #{end}
    (fn [node]
      ;; (println "DEBUG set-aggregated? NODE" (:node/id node))
      (st/plans-merge-node
        ;; (if (and (begin? node) (not (keyword-identical? (node-key-fn node) begin)))
        (if (begin? node)
          (assoc node :node/hidden aggregated :node/aggregated? false)
          (assoc node :node/hidden aggregated)))
      (map-outgoing node
        (fn [edge]
          ;; (println "DEBUG set-aggregated? EDGE" (:edge/id edge))
          (let [{:keys [edge/type edge/to]} edge
                hidden (if (keyword-identical? :aggregation type)
                         true
                         aggregated)]
            (st/plans-merge-edge (assoc
                                   (dissoc edge
                                     :plans/ui-opts
                                     :edge/from :edge/to)
                                   :edge/hidden hidden))
            (when (activity? type)
              (node-key-fn to))))))))

(defn aggregation-menu-fn [node _ option e]
  (when node
    (let [{:keys [tag]} option
          {:keys [plan/plid node/id node/end]} node
          aggregated (keyword-identical? tag :tpn-hide)
          node (assoc node :node/aggregated? aggregated :node/hidden false)]
      (status-msg tag id)
      ;; set the interior
      (set-aggregated? (node-key-fn node) end aggregated)
      ;; set the begin node
      (st/plans-merge-node node)
      ;; set the aggregation edge
      (map-outgoing node
        (fn [edge]
          (let [{:keys [edge/type edge/hidden]} edge]
            (if (and (keyword-identical? type :aggregation)
                  (= hidden aggregated))
              (st/plans-merge-edge (assoc edge :edge/hidden (not aggregated)))))))
      (my-user-action {:type :menu :tag tag :plid plid :node id})
      (menu-click-handled e))))

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
          (st/plans-merge-node (assoc
                                 (dissoc node :plans/ui-opts)
                                 :node/hidden focus?
                                 :node/aggregated? false))
          (map-outgoing node
            (fn [edge]
              (let [{:keys [edge/type edge/to edge/hidden]} edge]
                (when (#{:parallel-edge :choice-edge} type)
                  (if (not= focus? hidden)
                    (st/plans-merge-edge (assoc
                                           (dissoc edge
                                             :plans/ui-opts
                                             :edge/from :edge/to)
                                           :edge/hidden focus?)))
                  (node-key-fn to)))))))
      ;; signal my action (before pan/zoom)
      (my-user-action {:type :menu :tag tag :plid plid :node id})
      ;; focus OUR sub network
      (when focus?
        (visit-nodes node-id #{}
          (fn [node]
            (st/plans-merge-node (assoc
                                   (dissoc node :plans/ui-opts)
                                   :node/hidden false
                                   :node/aggregated?
                                   (keyword-identical? id (:node/id node))))
            (swap! bounds (fn [{:keys [xbounds ybounds]}]
                            (let [{:keys [node/x node/y]} node]
                              {:xbounds (min-max-fn xbounds [x x])
                               :ybounds (min-max-fn ybounds [y y])})))
            (map-outgoing node
              (fn [edge]
                (let [{:keys [edge/type edge/to edge/hidden]} edge]
                  (when (#{:parallel-edge :choice-edge} type)
                    (if hidden
                      (st/plans-merge-edge (assoc
                                             (dissoc edge
                                               :plans/ui-opts
                                               :edge/from :edge/to)
                                             :edge/hidden false)))
                    (node-key-fn to)))))))
        (let [{:keys [xbounds ybounds]} @bounds
              [xmin xmax] xbounds
              [ymin ymax] ybounds
              nodesep 50
              xmin (- xmin nodesep)
              xmax (+ xmax nodesep)
              ymin (- ymin nodesep)
              ymax (+ ymax (* 2 nodesep))
              {:keys [pan-zoom/width pan-zoom/height
                      pan-zoom/vp-width pan-zoom/vp-height]} (st/app-get :app/pan-zoom)
              vp-ratio (/ vp-height vp-width)
              focus-width (- xmax xmin)
              focus-height (- ymax ymin)
              ratio (/ focus-height focus-width)
              vertical? (> ratio vp-ratio) ;; constraint
              zoom-x (/ (if vertical? (/ height ratio) width) focus-width)
              zoom-y (/ (if vertical? height (* width ratio)) focus-height)
              zoom (min zoom-x zoom-y)
              pan [(/ xmin width) (/ ymin height)]]
          (zoom! zoom)
          (pan! pan)
          (my-user-action {:type :pan-zoom :pan pan :zoom zoom})))
      (if-not focus?
        (reset)) ;; zoom out
      (st/app-set-plan-value plid :focus (if focus? node-id))
      (menu-click-handled e))))

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
        options (concatv options
                  [{:tag :url :text "⇨ Open planviz" :url (make-url)
                    :tab "planviz" :fn url-menu-fn}
                   {:tag :url :text "⇨ Open dollabs" :url "http://dollabs.com"
                    :tab "planviz" :fn url-menu-fn}
                   {:tag :info :text (str "✋ node information for " id)
                    :fn info-menu-fn}])
        menu {:node node :x x :y y :options options}]
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
        options (if network-flows
                  [{:tag :tpn-network-flows
                    :text "⅏ Show activity on network" :fn default-menu-fn}]
                  [])
        options (concatv options
                  [{:tag :url :text "⇨ Open planviz" :url (make-url)
                    :tab "planviz" :fn url-menu-fn}
                   {:tag :url :text "⇨ Open dollabs" :url "http://dollabs.com"
                    :tab "planviz" :fn url-menu-fn}
                   {:tag :info :text (str "✋ edge information for " id)
                    :fn info-menu-fn}])
        menu {:edge edge :x x :y y :options options}]
    (println "ACTION EDGE-RIGHT-CLICK" plid id "type" type)
      ;; "x0" x0 "y0" y0 "x1" x1 "y1" y1)
    (st/merge-ui-opts {:ui/menu menu})
    ))

(defn graph-click [x e]
  (let [button (.-button e)
        browser-contextmenu? false
        menu (:ui/menu (st/get-ui-opts))
        target (.-target e)
        ]
    (println "CLICK" button "@" (.-clientX e) "," (.-clientY e)
      "b?" browser-contextmenu?)
    ;; (reset! debug-target target)
    ;; (println "bubbles" (.-bubbles e)
    ;;   "cancelable" (.-cancelable e))
    ;; (if menu ;; any click will remove menu
    ;;   (st/merge-ui-opts {:ui/menu nil}))
    (if menu
      (do
        (st/merge-ui-opts {:ui/menu nil})
        true) ;; allow real right click
      (do
        (case button
          0 (do
              (.preventDefault e)
              (.stopPropagation e)
              (cond
                (:edge/type x)
                (edgeclick x)
                (:node/type x)
                (nodeclick x)
                :else
                (clear-selection (:ui/show-plan (st/get-ui-opts)))))
          1 (println "middle button not used")
          2 (do
              (.preventDefault e)
              (.stopPropagation e)
              (cond
                (:edge/type x)
                (edge-right-click x)
                (:node/type x)
                (node-right-click x)
                :else
                (println "right click background")))
          true))))) ;; odd button case, let through

(defn app-key-fn [key]
  ;; (println "app-key-fn" key)
  (case key
    "x" (zoom-out) ;; 88
    "z" (zoom-in) ;; 90
    "ArrowRight" (pan-right) ;; 39
    "ArrowUp" (pan-up) ;; 38
    "ArrowLeft" (pan-left) ;; 37
    "ArrowDown" (pan-down) ;; 40
    "A-ArrowRight" (next-plan) ;; 39
    "A-ArrowLeft" (prev-plan) ;; 37
    "C-ArrowRight" (next-plan) ;; 39
    "C-ArrowLeft" (prev-plan) ;; 37
    "-" (zoom-out) ;; 173
    "=" (zoom-in) ;; 61
    "a" (show-all)
    "g" (clear-plans)
    "p" (list-plans) ;; 82
    "y" (render-plans :graphic)
    "1" (reset)
    "6" (update-plan-list)
    "t" (show-tooltips)
    "T" (hide-tooltips)
    true)) ;; always end a case with true

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
  (keys/register-key-fn "cmd" cmd-key-fn)
  (keys/register-key-fn :default app-key-fn)
  (keys/register-key-fn :extra show-extra-keys)
  (set! (.-onkeydown js/window) keys/keydown)
  (set! (.-onkeyup js/window) keys/keyup)
  (st/add-plans-root! graph-click)
  (st/add-app-root!)
  (vp-initialize)) ;; get and track viewport size
