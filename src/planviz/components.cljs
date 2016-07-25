;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns planviz.components
  "planviz data model"
  (:require [om.next :as om :refer-macros [defui]]
            [planviz.ui :as ui
             :refer [message-box-key-fn input-box-key-fn
                     pan-zoom-key-fn
                     ui-opts-key-fn node-key-fn edge-key-fn
                     label-key-fn network-key-fn]]))

(defn ref-by [by]
  (fn [id]
    [by id]))

(defn fix-refs [refs]
  (let [ref-fn (fn [m k v] (assoc m k (ref-by v)))
        refs-by (reduce-kv ref-fn {} refs)]
    (fn fix-refs-fn
      ([v]
       (cond
         (map? v) (reduce-kv fix-refs-fn {} v)
         :else v))
      ([m k v]
       (let [ref-by-fn (get refs-by k)]
         (assoc m k
           (if ref-by-fn
             (cond
               (map? v) (reduce-kv fix-refs-fn {} v)
               (vector? v) (mapv ref-by-fn v)
               :else (ref-by-fn v))
             (if (map? v)
               (reduce-kv fix-refs-fn {} v)
               v))))))))

;; MessageBox ------------------------------------------------------------

(defui MessageBox
  static om/Ident
  (ident [_ props]
    [:message-box/message-box-by-id (message-box-key-fn props)])
  static om/IQuery
  (query [_]
    '[:message-box/id
      :message-box/value
      ])
  Object
  (render [this]
    (let [props (om/props this)]
      (ui/message-box props))))

(def message-box-factory (om/factory MessageBox {:keyfn message-box-key-fn}))
(def message-box-query (om/get-query MessageBox))

;; InputBox ------------------------------------------------------------

(defui InputBox
  static om/Ident
  (ident [_ props]
    [:input-box/input-box-by-id (input-box-key-fn props)])
  static om/IQuery
  (query [_]
    '[:input-box/id
      :input-box/value
      :input-box/start
      :input-box/end
      :input-box/placeholder
      :input-box/size])
  Object
  (render [this]
    (let [props (om/props this)]
      (ui/input-box props))))

(def input-box-factory (om/factory InputBox {:keyfn input-box-key-fn}))
(def input-box-query (om/get-query InputBox))

;; PanZoom ------------------------------------------------------------

(defui PanZoom
  static om/Ident
  (ident [_ props]
    [:pan-zoom/pan-zoom-by-id (pan-zoom-key-fn props)])
  static om/IQuery
  (query [_]
    '[:pan-zoom/id
      :pan-zoom/width ;; 800
      :pan-zoom/height ;; 800
      :pan-zoom/vp-width ;; 800
      :pan-zoom/vp-height ;; 800
      :pan-zoom/pan ;; [0 0]
      :pan-zoom/zoom]) ;; 1.0
  Object
  (render [this]
    (let [props (om/props this)]
      (ui/pan-zoom props))))

(def pan-zoom-factory (om/factory PanZoom {:keyfn pan-zoom-key-fn}))
(def pan-zoom-query (om/get-query PanZoom))

;; UIOpts ------------------------------------------------------------

(defui UIOpts
  static om/Ident
  (ident [_ _]
    [:ui/singleton (ui-opts-key-fn)])
  static om/IQuery
  (query [_]
    '[:ui/show-plan ;; :all
      :ui/show-network ;; :all
      :ui/network-type
      :ui/rendering ;; rendering-initial
      :ui/show-virtual? ;; false;; nodes and edges
      :ui/node-ids? ;; false ;; show node ids
      :ui/edge-ids? ;; false ;; show edge -ids
      :ui/tooltips? ;; show tooltips
      :ui/graph-click ;; fn when a graph item is clicked
      :ui/menu ;; popup menu
      ])
  Object
  (render [this]
    (let [props (om/props this)]
      )))

(def ui-opts-query (om/get-query UIOpts))

;; GraphSelectionItem ----------------------------

(declare Node)
(declare Edge)

(defn gsi-ident-fn [props]
  (if (:node/id props)
    [:node/node-by-plid-id (node-key-fn props)]
    [:edge/edge-by-plid-id (edge-key-fn props)]))

(defui GraphSelectionItem
  static om/Ident
  (ident [_ props]
    (gsi-ident-fn props))
  static om/IQueryParams
  (params [_]
    {:node (with-meta [:plan/plid :node/id :node/state :node/begin-state]
             {:component Node})
     :edge (with-meta [:plan/plid :edge/id :edge/state]
             {:component Edge})})
  static om/IQuery
  (query [_]
    '{:node/node-by-plid-id ?node
      :edge/edge-by-plid-id ?edge})
  Object
  (render [this]
    (let [props (om/props this)]
      )))

(def gsi-query (om/get-query GraphSelectionItem))
(def gsi-refs {})

;; Node ------------------------------------------------------------

(defui Node
  static om/Ident
  (ident [_ props]
    [:node/node-by-plid-id (node-key-fn props)])
  static om/IQueryParams
  (params [_]
    {:ui-opts [:ui/network-type :ui/node-ids? :ui/tooltips? :ui/graph-click]
     :selection gsi-query})
  static om/IQuery
  (query [_]
    '[{:plans/ui-opts ?ui-opts}
      :plan/plid
      :node/id
      :node/type
      :node/state
      :node/probability ;; for uncontrolled c-begin c-end
      ;;TPN :state :c-begin :p-begin
      ;;HTN :htn-primitive-task :htn-expanded-nonprimitive-task
      :node/parent
      ;;HEM :htn-expanded-method
      ;; For ALL
      :node/label
      :node/sequence-label
      :node/sequence-end
      ;; FOR c-begin p-begin state
      :node/cost<=
      :node/reward<=
      ;; FOR c-begin p-begin
      :node/end
      ;; FOR c-end p-end
      :node/begin
      :node/begin-state ;; duplicate for performance of begin state
      :node/htn-node ; points to htn-primitive-task or HENT
      :node/htn-network
      :node/tpn-node
      :node/tpn-edge
      {:node/tpn-selection ?selection}
      ;; FOR :htn-primitive-task :htn-expanded-nonprimitive-task
      ;; parent-net points to the parent htn-network
      ;; tpn-node points to state, c-begin, or p-begin
      ;; FOR :htn-expanded-method
      ;; network (id for a :htn-network)
      ;; :node/layout ;; layout information for the node
      ;; incoming edges
      ;; outgoing edges
      :node/x
      :node/y
      ;; for TPLAN
      :node/hidden
      :node/incoming
      :node/outgoing
      :node/rank
      :node/p
      :node/up-priority
      :node/down-priority
      ;; for display
      :node/selected?
      :node/aggregated?
      :node/number
      ])
  Object
  (render [this]
    (let [props (om/props this)]
      (ui/node-memo props))))

(def node-factory (om/factory Node {:keyfn node-key-fn}))
(def node-query (om/get-query Node))
(def node-refs (assoc gsi-refs
                 :plans/ui-opts :ui/singleton))

;; Edge ------------------------------------------------------------

(defui Edge
  static om/Ident
  (ident [_ props]
    [:edge/edge-by-plid-id (edge-key-fn props)])
  static om/IQueryParams
  (params [_]
    {:node [:plan/plid :node/id :node/x :node/y]
     :ui-opts [:ui/network-type :ui/tooltips? :ui/graph-click]
     })
  static om/IQuery
  (query [_] ;; NORMAL QUOTE below
    '[{:plans/ui-opts ?ui-opts}
      :plan/plid
      :edge/id
      :edge/type
      ;; TPN :temporal-constraint :activity :null-activity
      ;; HTN :edge (in :htn-network) :hedge (in :hem-network)
      :edge/order
      ;; FOR ALL
      :edge/name
      :edge/state
      :edge/label
      :edge/value ;; bounds for temporal constraint
      :edge/htn-node ; points to htn-primitive-task or HENT
      ;; FOR :temporal-constraint
      :edge/between
      :edge/between-ends
      :edge/between-starts
      :edge/sequence-label
      :edge/sequence-end
      ;; FOR :activity
      :edge/plant
      :edge/plantid
      :edge/command
      :edge/cost
      :edge/reward
      :edge/controllable
      :edge/network-flows
      :edge/non-primitive ;; false or TPN network id of sub network
      ;; FOR :null-activity
      :edge/probability
      :edge/guard
      ;; for TPLAN
      :edge/weight
      :edge/hidden
      ;; for display
      :edge/selected?
      :edge/number
      {:edge/from ?node} {:edge/to ?node}])
  Object
  (render [this]
    (let [props (om/props this)]
      (ui/edge-memo props node-factory))))

(def edge-factory (om/factory Edge {:keyfn edge-key-fn}))
(def edge-query (om/get-query Edge))
(def edge-refs (assoc node-refs
                 :edge/from :node/node-by-plid-id
                 :edge/to :node/node-by-plid-id))

;; Label ------------------------------------------------------------

(defui Label
  static om/Ident
  (ident [_ props]
    [:edge/edge-by-plid-id (edge-key-fn props)])
  static om/IQueryParams
  (params [_]
    {:node [:node/x :node/y]
     :ui-opts [:ui/show-virtual? :ui/edge-ids?]})
  static om/IQuery
  (query [_] ;; NORMAL QUOTE below
    '[{:plans/ui-opts ?ui-opts}
      :plan/plid
      :edge/id
      :edge/type
      ;; TPN :temporal-constraint :activity :null-activity
      ;; HTN :edge (in :htn-network) :hedge (in :hem-network)
      :edge/order
      ;; FOR ALL
      :edge/name
      :edge/state
      :edge/value ;; bounds for temporal constraint
      :edge/htn-node ; points to htn-primitive-task or HENT
      ;; FOR :temporal-constraint
      :edge/label
      :edge/between
      :edge/between-ends
      :edge/between-starts
      :edge/sequence-label
      :edge/sequence-end
      ;; FOR :activity
      :edge/plant
      :edge/plantid
      :edge/command
      :edge/cost
      :edge/reward
      :edge/controllable
      :edge/network-flows
      :edge/non-primitive ;; false or TPN network id of sub network
      ;; FOR :null-activity
      :edge/probability
      :edge/guard
      ;; for TPLAN
      :edge/weight
      :edge/hidden
      ;; for display
      :edge/selected?
      :edge/number
      {:edge/from ?node} {:edge/to ?node}])
  Object
  (render [this]
    (let [props (om/props this)]
      (ui/label-memo props))))

(def label-factory (om/factory Label {:keyfn label-key-fn}))
(def label-query (om/get-query Label))
(def label-refs (assoc node-refs
                  :edge/from :node/node-by-plid-id
                  :edge/to :node/node-by-plid-id))

;; Network ------------------------------------------------------------

(defui Network
  static om/Ident
  (ident [_ props]
    [:network/network-by-plid-id (network-key-fn props)])
  static om/IQueryParams
  (params [_]
    {:nodes node-query
     :edges edge-query
     :ui-opts [:ui/show-virtual?]})
  static om/IQuery
  (query [_]
    '[{:plans/ui-opts ?ui-opts}
      :plan/plid
      :network/id
      :network/type ;; :tpn-network :htn-network :hem-network
      :network/begin ;; (htn optional) begin node
      :network/end  ;; optional end node
      :network/label
      :network/rootnodes  ;; for :hem-network
      :network/parent
      :network/width
      :network/height
      {:network/nodes ?nodes}
      {:network/edges ?edges}])
  Object
  (render [this]
    (let [props (om/props this)]
      (ui/network props node-factory edge-factory label-factory))))


(def network-factory (om/factory Network {:keyfn network-key-fn}))
(def network-query (om/get-query Network))
(def network-refs (assoc edge-refs
                    :network/nodes :node/node-by-plid-id
                    :network/edges :edge/edge-by-plid-id))

;; Plan ------------------------------------------------------------

(defui Plan
  static om/Ident
  (ident [_ {:keys [plan/plid]}]
    [:plan/by-plid plid])
  static om/IQueryParams
  (params [_]
    {:networks network-query
     :ui-opts [:ui/show-network]})
  static om/IQuery
  (query [_]
    '[{:plans/ui-opts ?ui-opts}
      :plan/plid
      :plan/name
      :plan/type ;; :tpn-network :htn-network
      :plan/begin ;; network-id
      :plan/corresponding
      {:plan/networks ?networks}])
  Object
  (render [this]
    (let [props (om/props this)]
      (ui/plan props network-factory))))

(def plan-factory (om/factory Plan {:keyfn :plan/plid}))
(def plan-query (om/get-query Plan))
(def plan-refs (assoc network-refs
                 :plan/networks :network/network-by-plid-id))

;; Plans ------------------------------------------------------------

(defui Plans
  static om/IQueryParams
  (params [_]
    {:pan-zoom pan-zoom-query
     :ui-opts [:ui/show-plan :ui/graph-click :ui/menu]
     :plans plan-query})
  static om/IQuery
  (query [_]
    '[{:plans/pan-zoom ?pan-zoom}
      {:plans/ui-opts ?ui-opts}
      {:plans/plans ?plans}]) ;; all plans
  Object
  (render [this]
    (let [props (om/props this)]
      (ui/plans props plan-factory))))

(def plans-query (om/get-query Plans))
(def plans-refs (assoc plan-refs
                  :plans/pan-zoom :pan-zoom/pan-zoom-by-id
                  :plan/plans :plan/by-plid))
(def plan-refs-fix (fix-refs plan-refs))

;; Application --------------------------

(defui Application
  static om/IQueryParams
  (params [_]
    {:message-box message-box-query
     :input-box input-box-query
     :pan-zoom pan-zoom-query})
  static om/IQuery
  (query [_]
    '[:app/title ;; "planviz"
      :app/initialized ;; 0
      :app/vp-timer ;; nil
      ;; :app/following
      :app/client
      :app/mode
      :app/loading ;; set to deferred when loading
      :app/defer ;; network updates to defer until after loading
      :app/plans
      :app/css ;; source for css
      :app/url-config ;; url configuration for right click menus
      :app/help ;; help configuration
      {:app/message-box ?message-box}
      {:app/input-box ?input-box}
      {:app/pan-zoom ?pan-zoom}])
  Object
  (render [this]
    (let [props (om/props this)]
      (ui/application props
        message-box-factory input-box-factory pan-zoom-factory))))

(def app-query (om/get-query Application))
(def app-refs {:app/message-box :message-box/message-box-by-id
               :app/input-box :input-box/input-box-by-id
               :app/pan-zoom :pan-zoom/pan-zoom-by-id})
