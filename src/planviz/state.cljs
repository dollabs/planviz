;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns planviz.state
    "planviz state management"
    (:require [clojure.set :as set]
              [clojure.string :as string]
              [cljs.pprint :refer [pprint]]
              [om.next :as om]
              [goog.dom :as gdom]
              [avenir.utils :as au :refer [assoc-if remove-fn
                                           as-boolean as-int as-float]]
              [planviz.components :as comp]
              [planviz.ui :as ui
               :refer [message-box-key-fn input-box-key-fn
                       pan-zoom-key-fn calc-bigplan
                       ui-opts-key-fn node-key-fn edge-key-fn
                       label-key-fn network-key-fn
                       composite-key]]))

;; DEBUG
(defn explain-exception [e]
  (str "Exception: " (.-message e) " at " (.-fileName e) ":" (.-lineNumber e)))

(defn replace-kv [replace-k replace-v & [ensure-k]]
  (letfn [(rkv
            ([v]
             (cond
               (vector? v)
               (mapv rkv v)
               (map? v)
               (reduce-kv rkv {}
                 (if (and ensure-k (get v ensure-k) (not (get v replace-k)))
                   (assoc v replace-k nil)
                   v))
               :else v))
            ([m k v]
             (assoc m k
               (cond
                 (vector? v) (mapv rkv v)
                 (map? v)
                 (if (keyword-identical? k replace-k)
                   replace-v
                   (reduce-kv rkv {}
                     (if (and ensure-k (get v ensure-k)
                           (not (get v replace-k)))
                       (assoc v replace-k nil)
                       v)))
                 :else (if (keyword-identical? k replace-k)
                         replace-v
                         v)))))]
    rkv))

;; helper functions ------------------------------

(defn denormalize [env k]
  (let [{:keys [state query]} env
        st @state]
    {:value (om/db->tree query (get st k) st)}))

(defn denormalize-by-key-fn [env]
  (let [{:keys [state query query-root]} env]
    {:value (om/db->tree query query-root @state)}))

;; plans -------------------------------------------------------------

(def pan-zoom-plans-id :pz-plans)

(def pan-zoom-plans
  {:pan-zoom/id pan-zoom-plans-id
   :pan-zoom/width 800
   :pan-zoom/height 800
   :pan-zoom/vp-width 800
   :pan-zoom/vp-height 800
   :pan-zoom/pan [0 0]
   :pan-zoom/zoom 1.0
   })

(def rendering-initial :graphic)

(def ui-opts-initial
  {:ui/show-plan :none
   :ui/show-network :none
   :ui/network-type :none
   :ui/rendering rendering-initial
   :ui/show-virtual? false;; nodes and edges
   :ui/node-ids? false ;; show node ids
   :ui/edge-ids? false ;; show edge -ids
   ;; :ui/tooltips? true ;; show tooltips
   :ui/graph-click nil
   :ui/settings {}})

(def plans-state-initial-empty
  (let [tables (set (vals comp/plans-refs))]
    (apply merge
      {:plans/pan-zoom [:pan-zoom/pan-zoom-by-id (pan-zoom-key-fn pan-zoom-plans)]
       :plans/ui-opts [:ui/singleton :singleton]
       :plans/plans []
       :om.next/tables tables}
      (for [table tables]
        (cond
          (keyword-identical? table :pan-zoom/pan-zoom-by-id)
          {table {(pan-zoom-key-fn pan-zoom-plans) pan-zoom-plans}}
          (keyword-identical? table :ui/singleton)
          {table {:singleton ui-opts-initial}}
          :else
          {table {}})))))

(defonce plans-state (atom plans-state-initial-empty))

(defn pp-plans []
  (pprint (remove-fn @plans-state)))

(defmulti plans-read om/dispatch)

(defmethod plans-read :default
  [{:keys [state] :as env} k _]
  {:value (get @state k)})

(defmethod plans-read :plans/pan-zoom
  [env k _]
  (denormalize env k))

(defmethod plans-read :pan-zoom/pan-zoom-by-id
  [env _ _]
  (denormalize-by-key-fn env))

(defmethod plans-read :plans/ui-opts
  [env k _]
  (denormalize env k))

(defmethod plans-read :ui/singleton
  [env _ _]
  (denormalize-by-key-fn env))

(defmethod plans-read :plans/plans
  [env k _]
  (denormalize env k))

(defmethod plans-read :plan/by-plid
  [env _ _]
  (denormalize-by-key-fn env))

(defmethod plans-read :plan/networks
  [env k _]
  (denormalize env k))

(defmethod plans-read :network/network-by-plid-id
  [env _ _]
  (denormalize-by-key-fn env))

(defmethod plans-read :network/nodes
  [env k _]
  (denormalize env k))

(defmethod plans-read :node/node-by-plid-id
  [env _ _]
  (denormalize-by-key-fn env))

(defmethod plans-read :node/tpn-selection
  [env k _]
  (denormalize env k))

(defmethod plans-read :network/edges
  [env k _]
  (denormalize env k))

(defmethod plans-read :edge/edge-by-plid-id
  [env _ _]
  (denormalize-by-key-fn env))

(defmethod plans-read :edge/from
  [env k _]
  (denormalize env k))

(defmethod plans-read :edge/to
  [env k _]
  (denormalize env k))

;; ------------

(defmulti plans-mutate om/dispatch)

(defmethod plans-mutate :default [a b c]
  (println "WARN plans-mutate default")
  ;(println "a keys" (keys a))
  ;(pprint a)
  (println "b" b)
  ;(println "c" c)
  )

(defmethod plans-mutate 'plans/pan-zoom
  [{:keys [state] :as env} k {:keys [pan-zoom] :as params}]
  {:value {:keys comp/pan-zoom-query}
   :action
   (let [id (pan-zoom-key-fn pan-zoom)
         ref [:pan-zoom/pan-zoom-by-id id]]
     #(swap! state update-in ref merge pan-zoom))})

(defmethod plans-mutate 'plans/ui-opts
  [{:keys [state] :as env} k {:keys [ui-opts] :as params}]
  {:value {:keys comp/plans-query} ;; provoke a redraw
   :action
   #(swap! state update-in [:ui/singleton :singleton] merge ui-opts)})

(defmethod plans-mutate 'plans/clear
  [{:keys [state] :as env} _ _]
  {:value {:keys comp/plans-query}
   :action
   #(reset! state plans-state-initial-empty)})

(defmethod plans-mutate 'plans/new
  [{:keys [state] :as env} k {:keys [plan] :as params}]
  {:value {:keys comp/plans-query}
   :action
   #(swap! state
      (fn [st]
        (let [{:keys [plans/ui-opts plans/plans
                      plan/by-plid network/network-by-plid-id
                      node/node-by-plid-id edge/edge-by-plid-id]} st
              st-by-plid (or by-plid {})
              st-network-by-plid-id (or network-by-plid-id {})
              st-node-by-plid-id (or node-by-plid-id {})
              st-edge-by-plid-id (or edge-by-plid-id)
              plan (comp/plan-refs-fix plan)
              {:keys [plan/by-plid network/network-by-plid-id
                      node/node-by-plid-id edge/edge-by-plid-id]} plan
              show-plan (first (keys by-plid))
              show-network (get-in by-plid [show-plan :plan/begin])
              network0 (get network-by-plid-id show-network)
              {:keys [network/width network/height network/type]} network0
              by-plid (merge st-by-plid by-plid)
              network-by-plid-id (merge st-network-by-plid-id network-by-plid-id)
              edge-by-plid-id (merge st-edge-by-plid-id edge-by-plid-id)
              node-by-plid-id (merge st-node-by-plid-id node-by-plid-id)]
          ((replace-kv :plans/ui-opts [:ui/singleton :singleton] :plan/plid)
           (assoc st
             :plans/plans (vec (set (conj plans [:plan/by-plid show-plan])))
             :plan/by-plid by-plid
             :network/network-by-plid-id network-by-plid-id
             :edge/edge-by-plid-id edge-by-plid-id
             :node/node-by-plid-id node-by-plid-id)))))})

(defmethod plans-mutate 'plans/all-normal
  [{:keys [state] :as env} k {:keys [plid] :as params}]
  {:value {:keys comp/plans-query}
   :action
   #(swap! state
      (fn [st]
        (let [{:keys [plan/by-plid network/network-by-plid-id
                      node/node-by-plid-id edge/edge-by-plid-id]} st
              node-normal-fn (fn [m k v]
                               (assoc m k
                                 (if (keyword-identical? (:plan/plid v) plid)
                                   (dissoc (assoc v :node/state :normal)
                                     :node/begin-state)
                                     v)))
              node-by-plid-id (reduce-kv node-normal-fn {} node-by-plid-id)
              edge-normal-fn (fn [m k v]
                               (assoc m k
                                 (if (keyword-identical? (:plan/plid v) plid)
                                   (assoc v :edge/state :normal :edge/marker-mid nil) v)))
              edge-by-plid-id (reduce-kv edge-normal-fn {} edge-by-plid-id)]
          (assoc st
            :edge/edge-by-plid-id edge-by-plid-id
            :node/node-by-plid-id node-by-plid-id))))})

(defn node-id? [id]
  (not (nil? (string/index-of (name id) "node"))))

;; *-begin node? duplicate state in the *-end node :node/begin-state
(defn node-updates [node-by-plid-id updates]
  (loop [node-by-plid-id node-by-plid-id
         u (first updates) more (rest updates)
         begin-states []]
    (if-not u
      (if (empty? begin-states)
        node-by-plid-id
        (let [[end state] (first begin-states)
              begin-states (rest begin-states)
              node (get node-by-plid-id end)
              node-by-plid-id (assoc node-by-plid-id end
                                (assoc node :node/begin-state state))]
          (recur node-by-plid-id nil nil begin-states)))
      (let [{:keys [plid update-uid state]} u
            node-id (if (node-id? update-uid)
                      (composite-key plid update-uid))
            node (if node-id (get node-by-plid-id node-id))
            node-by-plid-id (if node
                              (assoc node-by-plid-id node-id
                                (assoc node :node/state state))
                              node-by-plid-id)
            {:keys [node/type node/end]} node
            begin-states (if (or (keyword-identical? type :c-begin) (keyword-identical? type :p-begin))
                           (conj begin-states [end state])
                           begin-states)]
        (recur node-by-plid-id (first more) (rest more) begin-states)))))

(defn edge-updates [edge-by-plid-id updates]
  (loop [edge-by-plid-id edge-by-plid-id u (first updates) more (rest updates)]
    (if-not u
      edge-by-plid-id
      (let [{:keys [plid update-uid state display-name]} u
            edge-id (if (not (node-id? update-uid))
                      (composite-key plid update-uid))
            edge (if edge-id (get edge-by-plid-id edge-id))
            prev-state (:edge/state edge)
            ;; For cancel event, we want to preserve previous state
            ;; and add a marker
            cancel-event? (= :cancel state)
            cancelled-event? (= :cancelled state)
            new-state (if cancel-event?
                        prev-state
                        (or state prev-state))
            edge (assoc edge :edge/marker-mid state)
            edge-by-plid-id (if edge
                              (assoc edge-by-plid-id edge-id
                                (assoc edge :edge/state new-state :edge/display-name (or display-name (:edge/display-name edge))))
                              edge-by-plid-id)]
        (recur edge-by-plid-id (first more) (rest more))))))

(defmethod plans-mutate 'plans/network-updates
  [{:keys [state] :as env} k {:keys [updates] :as params}]
  ;; {:value {:keys comp/plans-query}
  {:value
   {:keys [{:plans/plans
            [{:plan/networks
              [{:network/nodes
                [:node/state :node/begin-state]}
               {:network/edges
                [:edge/state]}]}]}]}
   :action
   #(swap! state
      (fn [st]
        (let [{:keys [node/node-by-plid-id edge/edge-by-plid-id]} st
              node-by-plid-id (node-updates node-by-plid-id updates)
              edge-by-plid-id (edge-updates edge-by-plid-id updates)]
          (assoc st
            :edge/edge-by-plid-id edge-by-plid-id
            :node/node-by-plid-id node-by-plid-id))))})

(defmethod plans-mutate 'plans/node
  [{:keys [state] :as env} k {:keys [node] :as params}]
  {:value {:keys comp/node-query}
   :action
   (let [node-id (node-key-fn node)
         ref [:node/node-by-plid-id node-id]]
     #(swap! state update-in ref merge node))})

(defmethod plans-mutate 'plans/edge
  [{:keys [state] :as env} k {:keys [edge] :as params}]
  {:value {:keys comp/edge-query}
   :action
   (let [edge-id (edge-key-fn edge)
         ref [:edge/edge-by-plid-id edge-id]]
     #(swap! state update-in ref merge edge))})

(defmethod plans-mutate 'plans/edge-marker-mid
  [{:keys [state]} _ {:keys [edge]}]
  {:value {:keys comp/edge-query}
   :action #(swap! state update-in [:edge/edge-by-plid-id (edge-key-fn edge) :edge/marker-mid]
                   (fn [_]
                     (:edge/marker-mid edge)))})

(defonce plans-parser (om/parser {:read plans-read :mutate plans-mutate}))

(defonce plans-reconciler
  (om/reconciler {:state plans-state :parser plans-parser}))

(defn plans-query [q]
  (plans-parser {:state plans-state} q))

(defn merge-ui-opts [ui-opts]
  (plans-query `[(plans/ui-opts {:ui-opts ~ui-opts})])
  nil)

;; (defn add-plans-root! [execute-fn]
(defn add-plans-root! [graph-click]
  (merge-ui-opts {:ui/graph-click graph-click})
  (om/add-root! plans-reconciler comp/Plans (gdom/getElement "plans")))

;; app -------------------------------------------------------------

(def pan-zoom-app-id :pz-app)

(def pan-zoom-app (assoc pan-zoom-plans :pan-zoom/id pan-zoom-app-id))

(def message-box-id :mb-1)

(def message-box-initial
  {:message-box/id message-box-id
   :message-box/value "planviz"})

(def cmd-box-placeholder "/command, or /? for help ")

(def cmd-box-initial
  {:input-box/id :cmd
   :input-box/value ""
   :input-box/placeholder-orig cmd-box-placeholder
   :input-box/placeholder cmd-box-placeholder
   :input-box/size 80})

(def xchar-box-initial
  {:input-box/id :xchar
   :input-box/numeric? true
   :input-box/value ""
   :input-box/size 5})

(def ychar-box-initial
  {:input-box/id :ychar
   :input-box/numeric? true
   :input-box/value ""
   :input-box/size 5})

(def filename-box-initial
  {:input-box/id :filename
   :input-box/value "default"
   :input-box/size 20})

(def app-state-initial
  (let [tables (set (vals comp/app-refs))]
    (apply merge
      {:app/title "planviz"
       :app/initialized 0
       :app/vp-timer nil
       :app/mode :manual
       :app/plans {}
       :app/settings {}
       :app/message-box
       [:message-box/message-box-by-id
        (message-box-key-fn message-box-initial)]
       :app/pan-zoom
       [:pan-zoom/pan-zoom-by-id (pan-zoom-key-fn pan-zoom-app)]
       :app/cmd-box [:input-box/input-box-by-id (input-box-key-fn cmd-box-initial)]
       :app/xchar-box [:input-box/input-box-by-id
                       (input-box-key-fn xchar-box-initial)]
       :app/ychar-box [:input-box/input-box-by-id
                       (input-box-key-fn ychar-box-initial)]
       :app/filename-box [:input-box/input-box-by-id
                            (input-box-key-fn filename-box-initial)]
       :om.next/tables tables}
      (for [table tables]
        (cond
          (keyword-identical? table :message-box/message-box-by-id)
          {table {(message-box-key-fn message-box-initial)
                  message-box-initial}}
          (keyword-identical? table :input-box/input-box-by-id)
          {table {(input-box-key-fn cmd-box-initial) cmd-box-initial
                  (input-box-key-fn xchar-box-initial) xchar-box-initial
                  (input-box-key-fn ychar-box-initial) ychar-box-initial
                  (input-box-key-fn filename-box-initial) filename-box-initial}}
          (keyword-identical? table :pan-zoom/pan-zoom-by-id)
          {table {(pan-zoom-key-fn pan-zoom-app) pan-zoom-app}}
          :else
          {table {}})))))

(defonce app-state (atom app-state-initial))

(defn pp-app []
  (pprint (remove-fn @app-state)))

(defmulti app-read om/dispatch)

(defmethod app-read :default
  [{:keys [state] :as env} k _]
  {:value (get @state k)})

(defmethod app-read :app/message-box
  [env k _]
  (denormalize env k))

(defmethod app-read :message-box/message-box-by-id
  [env _ _]
  (denormalize-by-key-fn env))

(defmethod app-read :app/cmd-box
  [env k _]
  (denormalize env k))

(defmethod app-read :app/xchar-box
  [env k _]
  (denormalize env k))

(defmethod app-read :app/ychar-box
  [env k _]
  (denormalize env k))

(defmethod app-read :app/filename-box
  [env k _]
  (denormalize env k))

(defmethod app-read :input-box/input-box-by-id
  [env _ _]
  (denormalize-by-key-fn env))

(defmethod app-read :app/pan-zoom
  [env k _]
  (denormalize env k))

(defmethod app-read :pan-zoom/pan-zoom-by-id
  [env _ _]
  (denormalize-by-key-fn env))

(defmulti app-mutate om/dispatch)

(defmethod app-mutate 'app/set
  [{:keys [state] :as env} _ {:keys [k v] :as params}]
  {:value {:keys comp/app-query}
   :action
   #(swap! state assoc k v)})

(defmethod app-mutate 'app/message-box
  [{:keys [state] :as env} k {:keys [message-box] :as params}]
  {:value {:keys comp/message-box-query}
   :action
   (let [id (message-box-key-fn message-box)
         ref [:message-box/message-box-by-id id]]
     #(swap! state update-in ref merge message-box))})

(defmethod app-mutate 'app/input-box
  [{:keys [state] :as env} k {:keys [input-box] :as params}]
  {:value {:keys comp/input-box-query}
   :action
   (let [id (input-box-key-fn input-box)
         ref [:input-box/input-box-by-id id]]
     #(swap! state update-in ref merge input-box))})

(defmethod app-mutate 'app/pan-zoom
  [{:keys [state] :as env} k {:keys [pan-zoom] :as params}]
  {:value {:keys comp/pan-zoom-query}
   :action
   (let [id (pan-zoom-key-fn pan-zoom)
         ref [:pan-zoom/pan-zoom-by-id id]]
     #(swap! state update-in ref merge pan-zoom))})

(defonce app-parser (om/parser {:read app-read :mutate app-mutate}))

(defonce app-reconciler (om/reconciler {:state app-state :parser app-parser}))

(defn app-query [q]
  (app-parser {:state app-state} q))

(defn add-app-root! []
  (om/add-root! app-reconciler comp/Application (gdom/getElement "app")))

;; -------------------------------------------------------------

(defn plans-merge-node [node]
  (plans-query `[(plans/node {:node ~node})])
  nil)

(defn plans-merge-edge [edge]
  (plans-query `[(plans/edge {:edge ~edge})])
  nil)

(defn plans-get-node [node-id]
  (let [ref [:node/node-by-plid-id node-id]
        q [{ref comp/node-query}]]
    (get (plans-query q) ref)))

(defn plans-get-edge [edge-id]
  (let [ref [:edge/edge-by-plid-id edge-id]
        q [{ref comp/edge-query}]]
    (get (plans-query q) ref)))

(defn app-get [k]
  (let [q (cond
            (keyword-identical? k :app/message-box)
            [{:app/message-box comp/message-box-query}]
            (keyword-identical? k :app/pan-zoom)
            [{:app/pan-zoom comp/pan-zoom-query}]
            (#{:app/cmd-box :app/xchar-box :app/ychar-box :app/filename-box} k)
            [{k comp/input-box-query}]
            :else
            [k])]
    (get (app-query q) k)))

(defn app-set [k v]
  (app-query `[(app/set {:k ~k :v ~v})])
  nil)

(defn app-set-message [msg]
  (let [mb {:message-box/id message-box-id :message-box/value msg}]
    (app-query `[(app/message-box {:message-box ~mb})])
    nil))

(defn app-get-input-box [input-box]
  (let [input-box-id (or (and (map? input-box) (:input-box/id input-box))
                       input-box)
        ref [:input-box/input-box-by-id input-box-id]
        q [{ref comp/input-box-query}]]
    (get (app-query q) ref)))

(defn app-merge-input-box [input-box]
  (app-query `[(app/input-box {:input-box ~input-box})])
  nil)

(defn app-get-plan [plid]
  (get (app-get :app/plans) plid))

(defn app-set-plan [plid plan-data]
  (app-set :app/plans (assoc (app-get :app/plans) plid plan-data)))

(defn app-get-plan-value [plid k]
  (get-in (app-get :app/plans) [plid k]))

(defn app-set-plan-value [plid k v]
  (let [plans (app-get :app/plans)
        plan-data (get plans plid)]
    (app-set :app/plans (assoc plans plid (assoc plan-data k v)))))

(defn app-get-help []
  (app-get :app/help))

(defn app-set-help [help]
  (app-set :app/help help))

(defn app-get-help-shown []
  (get (app-get-help) :help/shown))

(defn app-set-help-shown [shown?]
  (let [help (app-get-help)]
    (app-set-help (assoc help :help/shown shown?))))

(defn app-get-settings []
  (app-get :app/settings))

(defn app-set-settings [settings]
  (app-set :app/settings settings))

(defn app-merge-settings [settings]
  (app-set :app/settings
    (merge (app-get-settings) settings)))

(defn get-bigplan []
  (let [bigplan (gdom/getElement "bigplan")]
    (if bigplan
      (let [width (as-float (.getAttribute bigplan "width"))
            height (as-float (.getAttribute bigplan "height"))
            style (string/split (.getAttribute bigplan "style") #"[:p]")
            top (as-float (get style 2))
            left (as-float (get style 4))]
        [left top width height])
      [0 0 0 0])))

(defn set-bigplan [[left top width height]]
  (let [bigplan (gdom/getElement "bigplan")
        style (str "top:" top "px;left:" left "px;")]
    (when bigplan
      (.setAttribute bigplan "width", (str width "px"))
      (.setAttribute bigplan "height", (str height "px"))
      (.setAttribute bigplan "style", style))))

(defn app-merge-pan-zoom [pz & [both?]]
  ;; (println "app-merge-pan-zoom" pz "both?" both?) ;; DEBUG
  (let [pz (assoc pz :pan-zoom/id pan-zoom-app-id)]
    (app-query `[(app/pan-zoom {:pan-zoom ~pz})])
    (if-not both? ;; cheat!
      (let [{:keys [pan-zoom/width pan-zoom/height
                    pan-zoom/vp-width pan-zoom/vp-height
                    pan-zoom/pan pan-zoom/zoom]}
            (app-get :app/pan-zoom)]
        (if (and width height)
          (let [bp (get-bigplan)
                ;; _ (println "app-merge-pan-zoom")
                bp-new (calc-bigplan width height
                         vp-width vp-height zoom pan)]
            (when (not= bp bp-new)
              ;; (println "FORCE" bp-new)
              (set-bigplan bp-new))))))))

(defn plans-merge-pan-zoom [pz]
  ;; (println "plans-merge-pan-zoom" pz) ;; DEBUG
  (let [pz (assoc pz :pan-zoom/id pan-zoom-plans-id)]
    (plans-query `[(plans/pan-zoom {:pan-zoom ~pz})])
    nil))

(defn get-ui-opts []
  (let [k :plans/ui-opts
        query [{k comp/ui-opts-query}]]
    (get (plans-query query) k)))

(defn get-ui-opts-settings []
  (:ui/settings (get-ui-opts)))

(defn set-ui-opts-settings [settings]
  (merge-ui-opts {:ui/settings settings}))

(defn merge-ui-opts-settings [settings]
  (set-ui-opts-settings
    (merge (get-ui-opts-settings) settings)))

(defn get-plan [plid]
  (let [ref [:plan/by-plid plid]
        q [{ref comp/plan-query}]]
    (get (plans-query q) ref)))

(defn get-network [network-plid-id]
  (let [ref [:network/network-by-plid-id network-plid-id]
        q [{ref comp/network-query}]]
    (get (plans-query q) ref)))

(defn get-network-end [plid]
  (let [ref [:plan/by-plid plid]
        k :plan/begin
        q [{ref [k]}]
        begin (get-in (plans-query q) [ref k])
        nref [:network/network-by-plid-id begin]
        nk :network/end
        nq [{nref [nk]}]]
    (get-in (plans-query nq) [nref nk])))

(defn get-network-begin [plid]
  (let [ref [:plan/by-plid plid]
        k :plan/begin
        q [{ref [k]}]
        begin (get-in (plans-query q) [ref k])
        nref [:network/network-by-plid-id begin]
        nk :network/begin
        nq [{nref [nk]}]]
    (get-in (plans-query nq) [nref nk])))

(defn get-network-basics [plid]
  (let [ref [:plan/by-plid plid]
        k :plan/begin
        q [{ref [k]}]
        begin (get-in (plans-query q) [ref k])
        nref [:network/network-by-plid-id begin]
        nks [:network/id :network/width :network/height :network/type
             :network/begin :network/end]
        nq [{nref nks}]]
    (get (plans-query nq) nref)))

(defn get-network-parent [network-plid-id]
  (let [nref [:network/network-by-plid-id network-plid-id]
        nk :network/parent
        nq [{nref [nk]}]]
    (get-in (plans-query nq) [nref nk])))

(defn get-plans-state [k]
  (get (plans-query [k]) k))

(defn clear-plans []
  (plans-query '[(plans/clear)])
  nil)

(defn set-plans-size [w h]
  (let [pz (assoc (app-get :app/pan-zoom)
             :pan-zoom/width w :pan-zoom/height h)]
    (plans-merge-pan-zoom pz)
    (app-merge-pan-zoom pz true)))

(defn show-plan [plid]
  (let [{:keys [network/id network/width network/height network/type]}
        (if (not (#{:none :loading} plid)) (get-network-basics plid))
        begin (if id (composite-key plid id))
        [width height] (if (keyword-identical? plid :none) [800 800] [width height])]
    (merge-ui-opts {:ui/show-plan plid
                    :ui/show-network begin
                    :ui/network-type type})
    (set-plans-size width height)))

(defn new-plan [plan]
  (let [plid (first (keys (:plan/by-plid plan)))]
    (show-plan :loading)
    (plans-query `[(plans/new {:plan ~plan})])
    (show-plan :none)
    plid)) ;; for deferred

(defn loading [loading?]
  (if loading?
    (set-plans-size nil nil)
    (set-plans-size 800 800)))

(defn show-network [network]
  (merge-ui-opts {:ui/show-network network}))

(defn render-plans [rendering]
  (merge-ui-opts {:ui/rendering rendering}))

(defn show-virtual [show-virtual?]
  (merge-ui-opts {:ui/show-virtual? show-virtual?}))

(defn node-ids [node-ids?]
  (merge-ui-opts {:ui/node-ids? node-ids?}))

(defn edge-ids [edge-ids?]
  (merge-ui-opts {:ui/edge-ids? edge-ids?}))

(defn all-normal [plid]
  (plans-query `[(plans/all-normal {:plid ~plid})])
  true) ;; for deferred

(defn network-updates [updates]
  (plans-query `[(plans/network-updates {:updates ~updates})])
  true) ;; for deferred