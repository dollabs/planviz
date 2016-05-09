;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns planviz.ui
  "UI helper functions"
  (:require [clojure.string :as string]
            [goog.dom :as gdom]
            [cljs.pprint :refer [pprint]]
            [cljsjs.react] ;; for om and sablono
            [cljsjs.react.dom] ;; for om and sablono
            [cljsjs.react.dom.server] ;; for sablono
            [sablono.core :as html :refer-macros [html]]
            [avenir.utils :as au :refer [assoc-if remove-fn concatv]]
            [avenir.math :as math :refer [sqrt PI_2 sin cos atan]]
            [plan-schema.core :as pschema
             :refer [composite-key composite-key-fn]]))

;; generic utilities
(defn non-zero? [x]
  (and x (not (zero? x))))

(defonce SQRT3 (sqrt 3))

;; key-fn's

(defn message-box-key-fn [props]
  (:message-box/id props))

(defn input-box-key-fn [props]
  (:input-box/id props))

(defn pan-zoom-key-fn [props]
  (:pan-zoom/id props))

(def ui-opts-key-fn (constantly :singleton))

(def node-key-fn (composite-key-fn :plan/plid :node/id))

(def edge-key-fn (composite-key-fn :plan/plid :edge/id))

(defn label-key-fn [props]
  (keyword (subs (str (get props :plan/plid)
                   (get props :edge/id) "-label") 1)))

(def network-key-fn (composite-key-fn :plan/plid :network/id))

;; for react
(defn safe-symbol [s]
  (let [n (if (keyword? s) (name s) s)]
    (string/replace n "." "-")))

(def xchar 4)
(def ychar 10)

(defn translate [x y]
  (str "translate(" x "," y ")"))

(def data-all #{:data})
(def data-nodes-edges (conj data-all :data-nodes-edges))
(def data-network (conj data-nodes-edges :data-network))
(def data-plan (conj data-network :data-plan))
(def data-overview (conj data-plan :data-overview))

(defn target-class [selected?]
  (if selected?
    "target-selected" "target-unselected"))

(defn tooltip [tag x y tip type]
  (let [tip-len (count tip)
        width (* (+ tip-len 2) xchar)
        height (* 1.5 ychar)
        x-tip (- x (/ width 2))
        y-tip (case type
                :activity
                (+ y ychar -3)
                :htn-expanded-method
                (- y (* 4 ychar))
                (- y ychar height))
        ]
    [tag
     [:rect {:x x-tip :y y-tip :rx 3 :ry 3
             :width width :height height}]
     [:text {:x (+ x-tip xchar) :y (+ y-tip ychar)} tip]]))

;; 0 for normal
;; 0.5 for started
;; 1 for finished/reached
(defn selection-score [sel]
  (let [{:keys [node/id node/begin-state node/state]} sel
        node? (keyword? id)
        edge-state (if-not state (:edge/state sel))]
    (cond
      (or (and (not node?) (= edge-state :finished))
        (and node? (= state :reached)))
      1
      (or
        (and (not node?)
          (or (nil? edge-state) (= edge-state :normal)))
        (and node?
          (or (nil? begin-state) (= begin-state :normal))))
      0
      :else
      0.5)))

(defn hem-node-state [tpn-selection]
  (let [total (count tpn-selection)
        scores (map selection-score tpn-selection)
        score (reduce + 0 scores)
        done (if (pos? total) (/ score total) 0)]
    (if (zero? done)
      :normal
      (if (= 1 done)
        :reached
        :started))))

(defn node [{:keys[plans/ui-opts plan/plid node/id
                   node/type node/state node/x node/y
                   node/label node/sequence-label
                   node/probability node/selected?
                   node/tpn-selection] :as props}]
  (let [{:keys [ui/network-type ui/node-ids? ui/tooltips?
                ui/graph-click]} ui-opts
        state (if tpn-selection
                (hem-node-state tpn-selection)
                state)]
    (html
      ;; (if (= rendering :graphic)
        (let [xlink (cond
                      (#{:p-begin :p-end} type)
                      "parallel"
                      (#{:c-begin :c-end} type)
                      (if probability "unchoice" "choice")
                      (= :htn-expanded-method type)
                      "hem"
                      :else
                      "state")
              css (str xlink "-"
                    (if (= type :virtual) "virtual-")
                    (name state))
              use [:use {:class css :x x :y y :xlinkHref (str "#" xlink)
                         ;; :on-click #(nodeclick id)
                         }]
              top (- y 10)
              ychar 8
              ;; U+25B9 	WHITE RIGHT-POINTING SMALL TRIANGLE
              label (if sequence-label
                      (str label "\n▹ " sequence-label)
                      label)
              lines (if label (string/split label #"\n"))
              labels (if lines
                       (for [i (range (count lines))
                             :let [lab (get lines i)]]
                         [:text {:class "node-label"
                                 :textAnchor "middle"
                                 :x x :y (+ top (* i ychar))} lab]
                         )
                       )
              y-node-id (+ y (if (= network-type :hem-network) 30 20))
              tip (str (name id) " " (name state))]
          (concatv
            [:g.node
             (if (fn? graph-click)
               {:on-click (partial graph-click props)})
             ]
            [(if (= type :htn-expanded-method)
               (let [r 12
                     hem-size (* 4 r)
                     hem-offset (- (/ hem-size 2))]
                 [:rect {:class (target-class selected?)
                         :x (+ (* 2 hem-offset) x) :y (+ hem-offset y)
                         :rx 3 :ry 3
                         :width (* 2 hem-size) :height hem-size}])
               [:circle {:class (target-class selected?)
                         :cx x :cy y :r 16}])]
            [use]
            labels
            (if node-ids?
              [[:text {:textAnchor "middle" :x x :y y-node-id} (name id)]])
            (if tooltips?
              [(tooltip :g.node-tooltip.hide x y tip type)]))))))

(def node-memo (memoize node))

;; returns x y d (where x y are the label position)
(defn link-arc [type x0 y0 x1 y1]
  (let [x (/ (+ x0 x1) 2)
        y (/ (+ y0 y1) 2)]
    (case type
      :temporal-constraint
      (let [dx (- x1 x0)
            dy (- y1 y0)
            dh (sqrt (+ (* dx dx) (* dy dy)))
            ranksep 70 ;; grab from state?
            ratio (/ dh ranksep)
            a (+ 0.75 (* 0.85 (max (- (min (/ dh ranksep) 16) 2) 0)))
            r (* a dh)
            z (- r (/ (sqrt (- (* 4 r r) (* dh dh))) 2))
            [x y] (if (zero? dy)
                    (if (pos? dx)
                      [x (- y z)]
                      [x (+ y z)])
                    (if (zero? dx)
                      (if (pos? dy)
                        [(+ x z) y]
                        [(- x z) y])
                      (if (pos? dx)
                        (if (pos? dy)
                          (let [theta (- PI_2 (atan (/ dx dy)))]
                            [(+ x (* z (sin theta))) (- y (* z (cos theta)))])
                          (let [theta (- PI_2 (atan (- (/ dx dy))))]
                            [(- x (* z (sin theta))) (- y (* z (cos theta)))]))
                        (if (pos? dy)
                          (let [theta (- PI_2 (atan (- (/ dx dy))))]
                            [(+ x (* z (sin theta))) (+ y (* z (cos theta)))])
                          (let [theta (- PI_2 (atan (/ dx dy)))]
                            [(- x (* z (sin theta))) (+ y (* z (cos theta)))])))))]
        [x (- y 2)
         (str "M" x0 " " y0 "A" r " " r " 0 0,1 " x1 " " y1)
         ratio])
      ;; default ----------------------------------------------
      [x (- y 2)
       (str "M" x0 " " y0 "L" x1 " " y1)
       0])))

(def link-arc-memo (memoize link-arc))

(defn construct-extra [cost reward probability guard]
  (let [extra (str
                (if (non-zero? cost) "cost: ")
                (if (non-zero? cost) cost)
                (if (non-zero? reward) " reward: ")
                (if (non-zero? reward) reward)
                (if (non-zero? probability) " probability: ")
                (if (non-zero? probability) probability)
                (if guard " guard: ")
                guard)]
    extra))

(defn edge [{:keys[plans/ui-opts plan/plid edge/id
                   edge/type edge/state edge/from edge/to
                   edge/cost edge/reward edge/probability edge/guard
                   edge/selected?]
             :as props}
            node-factory]
  (let [{:keys [ui/network-type ui/tooltips?
                ui/graph-click]} ui-opts]
    (html
      ;; (if (= rendering :graphic)
        (let [[x0 y0] [(:node/x from) (:node/y from)]
              [x1 y1] [(:node/x to) (:node/y to)]
              [x y d ratio] (link-arc-memo type x0 y0 x1 y1)
              virtual? (= type :virtual)
              constraint? (= type :temporal-constraint)
              length-class (if (and constraint? (> ratio 3))
                             (if (< ratio 10) "-long" "-very-long"))
              marker-end (if constraint? (str "url(#arrow" length-class ")")
                             (if virtual? "url(#arrowlight)"
                                 (if (= network-type :hem-network)
                                   "url(#arrowhem)"
                                   "url(#arrowhead)")))
              marker-start nil ;; (if (= type :choice-edge) "url(#choicehem)")
              class (str (name type) "-" (name state) length-class)
              attrs (assoc-if {:class class :marker-end marker-end :d d}
                      :marker-start marker-start)
              target-attrs (if (#{:activity :choice-edge :parallel-edge} type)
                             {:class (target-class selected?) :d d})
              extra (construct-extra cost reward probability guard)
              tip (if (empty? extra) (str (name id) " " (name state)) extra)]
          [:g.edge
             (if (and (= type :activity) (fn? graph-click))
               {:on-click (partial graph-click props)})
           (if target-attrs
             [:path target-attrs])
           [:path attrs]
           (if tooltips?
             [(tooltip :g.edge-tooltip.hide x y tip type)])]))))

(def edge-memo (memoize edge))

(defn construct-label [name label sequence-label plant plantid command bounds]
  (let [full (str plant
               (if-not (empty? plantid) ".")
               plantid
               (if-not (empty? command) "$")
               command)
        label (str
                (if-not (empty? full) full name)
                (if label " (") label
                ;; U+25B9 	WHITE RIGHT-POINTING SMALL TRIANGLE
                (if sequence-label " ▹ ")
                sequence-label
                (if label ")"))
        label (if (vector? bounds)
                (str label (if label " ") bounds)
                label)]
    label))

(defn label [{:keys[plans/ui-opts edge/id edge/type edge/name edge/label
                    edge/sequence-label
                    edge/plant edge/plantid edge/command
                    edge/from edge/to edge/bounds
                    edge/cost edge/reward edge/probability edge/guard
                    edge/order] :as props}]
  (let [{:keys [ui/show-virtual? ui/edge-ids?]} ui-opts
        virtual? (= type :virtual)
        label? (or show-virtual? (not virtual?))
        label (if label? (construct-label name label sequence-label
                           plant plantid command bounds))
        label (if edge-ids?
                (str label " = "
                  (clojure.core/name id)
                  (if order " {")
                  order
                  (if order "}"))
                label)
        extra nil ;; (construct-extra cost reward probability guard)
        [x0 y0] [(:node/x from) (:node/y from)]
        [x1 y1] [(:node/x to) (:node/y to)]
        [x y d ratio] (if label?
                        (link-arc-memo type x0 y0 x1 y1)
                        [x0 y0 0 0])
        order (if edge-ids? order)
        above [:text {:textAnchor "middle"
                      :x (- x 5) :y (+ y -3 (if order (* 7 order) 0))} label]
        below (if (not (empty? extra))
                [:text {:textAnchor "middle" :x x :y (+ y 12)} extra])]
    (html
      (if below
        [:g above below]
        above))))

(def label-memo (memoize label))

(defn network [{:keys [plans/ui-opts network/id network/type
                       network/nodes network/edges
                       network/width network/height] :as props}
               node-factory edge-factory label-factory]
  (let [{:keys [ui/show-virtual?]} ui-opts
        edges-to-show (if show-virtual? edges
                          (remove #(= :virtual (:edge/type %)) edges))
        edges? (pos? (count edges-to-show))
        nodes-to-show (if show-virtual? nodes
                          (remove #(= :virtual (:node/type %)) nodes))
        nodes? (pos? (count nodes-to-show))]
    (html
      (concatv
        [:g]
        ;; (if (= rendering :graphic)
          (concatv
            (if edges? (map edge-factory edges-to-show))
            (if nodes? (map node-factory nodes-to-show))
            (if edges? (map label-factory edges-to-show)))))))

(defn plan [{:keys [plans/ui-opts plan/plid plan/name plan/type plan/networks]
             :as props}
            network-factory]
  (let [{:keys [ui/show-network]} ui-opts
        networks-to-show (if (or (nil? show-network) (= :all show-network))
                           networks
                           (filter #(= (network-key-fn %) show-network) networks))
        networks? (pos? (count networks-to-show))]
    (html
      (concatv
        [:g]
        (map network-factory networks-to-show)))))

(def markers
  (str
    "<marker id=\"arrowhead\" orient=\"auto\" markerHeight=\"4\" markerWidth=\"4\" refY=\"0\" refX=\"17\" viewBox=\"0 -5 10 10\"><path d=\"M0,-5L10,0L0,5\"></path></marker>\n"
    "<marker id=\"choicehem\" orient=\"auto\" markerHeight=\"5\" markerWidth=\"5\" refY=\"0\" refX=\"-70\" viewBox=\"-5 -5 10 10\"><circle r=\"5\"></circle></marker>\n"
    "<marker id=\"arrowhem\" orient=\"auto\" markerHeight=\"5\" markerWidth=\"5\" refY=\"0\" refX=\"5\" viewBox=\"0 -5 10 10\"><path d=\"M0,-5L10,0L0,5\"></path></marker>\n"
    "<marker id=\"arrowlight\" orient=\"auto\" markerHeight=\"4\" markerWidth=\"4\" refY=\"0\" refX=\"19\" viewBox=\"0 -5 10 10\"><path d=\"M0,-5L10,0L0,5\"></path></marker>\n"
    "<marker id=\"arrow\" orient=\"auto\" markerHeight=\"3\" markerWidth=\"3\" refY=\"-2\" refX=\"22\" viewBox=\"0 -5 10 10\"><path d=\"M0,-5L10,-1L1,5\"></path></marker>\n"
    "<marker id=\"arrow-long\" orient=\"auto\" markerHeight=\"3.5\" markerWidth=\"3.5\" refY=\"-0.5\" refX=\"20\" viewBox=\"0 -5 10 10\"><path d=\"M0,-5L10,0L0,5\"></path></marker>\n"
    "<marker id=\"arrow-very-long\" orient=\"auto\" markerHeight=\"8\" markerWidth=\"8\" refY=\"0\" refX=\"24\" viewBox=\"0 -5 10 10\"><path d=\"M0,-5L10,0L0,5\"></path></marker>\n"
    ))

(defn svg-defs []
  (let [r 10
        hem-size (* 4 r)
        hem-offset (- (/ hem-size 2))
        h (/ r 3.67) ;; half radius of hexagon circle
        dh (* SQRT3 h)] ;; shortest radius
    [:defs
     [:g#state
      [:circle {:class "node" :r r}]]
     [:g#parallel
      [:circle {:class "node" :r r}]
      [:path {:class "parallel"
              :d (let [dx (/ r 2)
                       dy (- r 3)]
                   (str "M" (- dx) " " (- dy) "L" (- dx) " " dy
                     "M" dx " " dy "L" dx " " (- dy)))}]]
     [:g#choice
      [:circle {:class "node" :r r}]
      [:circle {:class "choice" :r (/ r 2)}]]
     [:g#unchoice ;; uncontrolled choice
      [:circle {:class "node" :r r}]
      [:path {:class "unchoice"
              :d (str "M" (- dh) " " (+ h)
                   "L" (- dh) " " (- h)
                   "L" 0 " " (- (* 2 h))
                   "L" (+ dh) " " (- h)
                   "L" (+ dh) " " (+ h)
                   "L" 0 " " (+ (* 2 h))
                   "Z" ;; "L" (- dh) " " (+ h)
                   )}]]
     [:g#hem
      [:rect {:class "hem"
              :x (* 2 hem-offset) :y hem-offset
              :rx 3 :ry 3
              :width (* 2 hem-size) :height hem-size}]]
     [:g#markers {:dangerouslySetInnerHTML {:__html markers}}]]))

(defn plans [{:keys [plans/pan-zoom plans/ui-opts plans/plans] :as props}
             plan-factory]
  (let [{:keys [pan-zoom/width pan-zoom/height
                pan-zoom/vp-width pan-zoom/vp-height
                pan-zoom/pan pan-zoom/zoom]} pan-zoom
        {:keys [ui/show-plan ui/graph-click]} ui-opts
        plans-to-show (if (or (nil? show-plan) (= :all show-plan))
                        plans
                        (filter #(= (:plan/plid %) show-plan) plans))
        plans? (pos? (count plans-to-show))
        ;; rendering (or rendering :graphic)
        loading? (not (and width height))
        [width height] [(or width 800) (or height 800)]
        viewbox  (str "0 0 " width " " height)
        vp-ratio (/ vp-height vp-width)
        ratio (/ height width)
        vertical? (> ratio vp-ratio) ;; constraint
        [big-w big-h] (mapv #(* % zoom)
                        (if vertical?
                          [(/ vp-height ratio) vp-height]
                          [vp-width (* vp-width ratio)]))
        [pan-x pan-y] pan
        [big-left big-top] [(- (* pan-x big-w)) (- (* pan-y big-h))]]
    (html
      (if loading?
        [:div#plans [:div.load-container.load5 [:div.loader "Loading..."]]]
        [:svg#bigplan {:viewBox viewbox
                       :style {:top (str big-top)
                               :left (str big-left)}
                       :width big-w :height big-h
                       :on-click (partial graph-click nil)}
         (svg-defs)
         (concatv
           [:g#planviz
            ;; blue rect for entire graph
            ;; [:rect.plans {:x 0 :y 0
            ;;               :width (- width 2) :height (- height 2)}]
            ]
           (map plan-factory plans-to-show))]))))

(defn message-box [{:keys [message-box/id message-box/value] :as props}]
  (let [id (or id :mb-wat)
        tag (keyword (str "div#mb-" (name id) ".message-box"))]
    (html
      [tag value])))

(defn input-box [{:keys [input-box/id input-box/value
                         ;; input-box/start input-box/end
                         input-box/placeholder input-box/size] :as props}]
  (let [id (or id :ib-wat)
        id-str (name id)
        div-tag (keyword (str "div#ib-" id-str ".input-box"))
        input-tag (keyword (str "input#" id-str))
        attrs (assoc-if {:type "text" :value (or value "")}
                :placeholder placeholder
                :size size)]
    (html
       [div-tag
        [input-tag attrs]])))

(defn pan-zoom [{:keys [pan-zoom/id pan-zoom/width pan-zoom/height
                        pan-zoom/vp-width pan-zoom/vp-height
                        pan-zoom/pan pan-zoom/zoom] :as props}]
  (let [id (or id :pz-wat)
        tag (keyword (str "div#pz-" (name id) ".pan-zoom"))
        loading? (not (and width height))
        [width height] [(or width 800) (or height 800)]
        viewbox  (str "0 0 " width " " height)
        vp-ratio (/ vp-height vp-width)
        ratio (/ height width)
        vertical? (> ratio vp-ratio) ;; constraint
        [pan-x pan-y] pan
        [view-x view-y] [(* pan-x width) (* pan-y height)]
        [view-w view-h] (mapv #(/ % zoom)
                          (if vertical?
                            [(/ height vp-ratio) height]
                            [width (* vp-ratio width)]))]
    (html
      (if loading?
        [:br]
        [tag
         [:svg#miniplan {:viewBox viewbox
                         :preserveAspectRatio "xMidYMid meet"}
          [:use {:xlinkHref "#planviz"}]
          [:rect.planview {:x view-x :y view-y
                           :width view-w :height view-h}]]]))))

(defn application [{:keys [app/message-box app/input-box app/pan-zoom
                           app/title app/mode] :as props}
                   message-box-factory input-box-factory pan-zoom-factory]
  (html
    [:div#application
     [:div#title (str title "  " (name (or mode :manual)))]
     [:div#logo]
     (message-box-factory message-box)
     (input-box-factory input-box)
     (pan-zoom-factory pan-zoom)]))
