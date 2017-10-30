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
            [avenir.math :as math :refer [sqrt PI_2 sin cos atan]]))

;; from plan-schema.core
(defn composite-key [k1 k2]
  (keyword (subs (str k1 k2) 1)))

(defn composite-key-fn [k1 k2]
  (fn [props]
    (keyword (subs (str (get props k1) (get props k2)) 1))))

;; generic utilities
(defn non-zero? [x]
  (and x (not (zero? x))))

(defonce SQRT3 (sqrt 3))

(defn digit? [s]
  ;; (and (string? s) (boolean (#{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"} s)))
  (and (string? s) (= 1 (count s))
    (let [a (.charCodeAt s 0)]
      (and (>= a 48) (<= a 57)))))

(defn lower-case-letter? [s]
  (and (string? s) (= 1 (count s))
    (let [a (.charCodeAt s 0)]
      (and (>= a 97) (<= a 122)))))

(defn upper-case-letter? [s]
  (and (string? s) (= 1 (count s))
    (let [a (.charCodeAt s 0)]
      (and (>= a 65) (<= a 90)))))

(defn lower-case? [s]
  (and (string? s) (not (digit? s)) (= s (string/lower-case s))))

(defn upper-case? [s]
  (and (string? s) (not (digit? s))  (= s (string/upper-case s))))

;; fonts
(def font-families {"font-family-helvetica" "Helvetica"
                    "font-family-pt-sans-narrow" "PT Sans Narrow"
                    "font-family-roboto" "Roboto"
                    "font-family-open-sans-condensed" "Open Sans Condensed"
                    "font-family-nunito" "Nunito"})

(def font-weights {"font-weight-light" "Light"
                   ;; "font-weight-regular" "Regular"
                   "font-weight-bold" "Bold"})

(def font-sizes {"font-size-7" "7 px"
                 "font-size-8" "8 px"
                 "font-size-9" "9 px"
                 "font-size-10" "10 px"
                 "font-size-11" "11 px"
                 "font-size-12" "12 px"
                 "font-size-13" "13 px"})


(def planviz-settings {:settings/auto false
                       :settings/tooltips true
                       :settings/font-family "font-family-helvetica"
                       :settings/font-size "font-size-9"
                       :settings/font-weight "font-weight-light"
                       :settings/rewrap-htn-labels true
                       :settings/xchar 5
                       :settings/ychar 10
                       :settings/filename "default"
                       :settings/filenames ["default"]})

;; ignore these keys when saving settings
(def planviz-settings-ignore [:settings/shown? :settings/settings-action
                              :settings/filenames])

;; for TPN graphs

(def edge-states {:normal      {:parallel 0 :choice 2 :node-state :normal}
                  :best        {:parallel 1 :choice 3 :node-state :best}
                  :finished    {:parallel 2 :choice 6 :node-state :reached}
                  :cancelled    {:parallel 2 :choice 6 :node-state :reached}
                  :started     {:parallel 3 :choice 5 :node-state :started}
                  :active      {:parallel 3 :choice 5 :node-state :started}
                  :start       {:parallel 4 :choice 4 :node-state :started}
                  :negotiation {:parallel 4 :choice 4 :node-state :started}
                  :impossible  {:parallel 5 :choice 0 :node-state :impossible}
                  :failed      {:parallel 6 :choice 1 :node-state :failed}})

(def node-states {:normal      {:parallel 0 :choice 2 :edge-state :normal}
                  :best        {:parallel 1 :choice 3 :edge-state :best}
                  :reached     {:parallel 2 :choice 6 :edge-state :finished}
                  :started     {:parallel 3 :choice 5 :edge-state :started}
                  :impossible  {:parallel 5 :choice 0 :edge-state :impossible}
                  :failed      {:parallel 6 :choice 1 :edge-state :failed}})

(def parallel-edge-states {0 :normal
                           1 :best
                           2 :finished
                           3 :started
                           4 :start
                           5 :impossible
                           6 :failed})

(def choice-edge-states {0 :impossible
                         1 :failed
                         2 :normal
                         3 :best
                         4 :start
                         5 :started
                         6 :finished})

(def constraint-types #{:temporal-constraint
                        :cost<=-constraint
                        :reward>=-constraint})

(defn constraint? [edge-or-type]
  (constraint-types (if (map? edge-or-type)
                      (:edge/type edge-or-type)
                      edge-or-type)))

(def activity-types #{:activity :null-activity :delay-activity})

(defn activity-type? [edge-or-type]
  (activity-types (if (map? edge-or-type)
                      (:edge/type edge-or-type)
                      edge-or-type)))

(def begin-types #{:c-begin :p-begin})

(defn begin? [node-or-type]
  (begin-types (if (map? node-or-type)
                 (:node/type node-or-type)
                 node-or-type)))

;; key-fn's

(def message-box-key-fn :message-box/id)

(def input-box-key-fn :input-box/id)

(def pan-zoom-key-fn :pan-zoom/id)

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

(defn tooltip [settings tag x y tip type]
  (let [{:keys [settings/xchar settings/ychar]} settings
        tip-len (count tip)
        width (* (+ tip-len 2) xchar)
        height (* 1.5 ychar)
        x-tip (- x (/ width 2))
        y-tip (case type
                :activity
                (+ y ychar -3)
                :delay-activity
                (+ y ychar -3)
                :htn-expanded-method
                (- y (* 4 ychar))
                (- y ychar height))]
    [tag
     [:rect {:x x-tip :y y-tip :rx 3 :ry 3
             :width width :height height}]
     [:text {:x (+ x-tip xchar) :y (+ y-tip ychar)} tip]]))

(defn rewrap-label [settings label]
  (when label
    (let [{:keys [settings/xchar]} settings
          ;; _ (println "DEBUG label is a"
          ;;     (if (string? label) "String" (type label))
          ;;     "=" label)
          label (if (string? label) label (str label))
          raw (string/replace label "\n" "")
          len (count raw) ;; avoid fencepost
          hem-width 89
          ;; desired number of lines
          n (inc (quot (* (dec len) xchar) hem-width))
          ;; naive partition based on length
          m (quot len n) ;; break every mth char
          ;; parts (if (= n 1)
          ;;         (list raw)
          ;;         (partition m m "" raw))
          ;; label (apply str (map #(apply str (conj (vec %) "\n")) parts))
          ;; keeps a trailing newline
          ;; --------------------
          ;; break space, -
          ]
      ;; i is the number chars since a newline
      (loop [label "" i 0 ch (first raw) more (rest raw)]
        (if-not ch
          label
          (let [next-ch (first more)
                lower-to-upper? (and
                                  (or (digit? ch) (lower-case-letter? ch))
                                  (upper-case-letter? next-ch))
                break? (and (>= i (quot (* 0.65 m) 1))
                         (or (= ch " ") (= ch "-") lower-to-upper?)
                         (> (- len (count label)) 5))
                [newline i] (if break?
                              ["\n" 0]
                              [(if lower-to-upper? " " "")
                               (inc i)])
                label (str label ch newline)]
            (recur label i next-ch (rest more)))))
      )))

;; 0 for normal
;; 0.5 for started
;; 1 for finished/reached
(defn selection-score [sel]
  (let [{:keys [node/id node/begin-state node/state]} sel
        node? (keyword? id)
        edge-state (if-not state (:edge/state sel))]
    (cond
      (or (and (not node?) (keyword-identical? edge-state :finished))
        (and node? (keyword-identical? state :reached)))
      1
      (or
        (and (not node?)
          (or (nil? edge-state) (keyword-identical? edge-state :normal)))
        (and node?
          (or (nil? begin-state) (keyword-identical? begin-state :normal))))
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
                   node/type node/state node/x node/y node/hidden
                   node/label node/display-name node/args node/sequence-label
                   ;; node/cost<= node/reward=>
                   node/probability node/selected?
                   node/tpn-selection node/number] :as props}]
  (let [{:keys [ui/network-type ui/node-ids?
                ui/graph-click ui/settings]} ui-opts
        {:keys [settings/font-family settings/font-size settings/font-weight
                settings/rewrap-htn-labels settings/tooltips
                settings/xchar settings/ychar]} settings
        state (if tpn-selection
                (hem-node-state tpn-selection)
                state)]
    (html
      ;; (if (= rendering :graphic)
      (let [xlink (str
                    (cond
                      (#{:p-begin :p-end} type)
                      "parallel"
                      (#{:c-begin :c-end} type)
                      (if probability "unchoice" "choice")
                      (keyword-identical? :htn-expanded-method type)
                      "hem"
                      :else
                      "state")
                    (if hidden "-hidden"))
            css (str xlink "-"
                  (if (keyword-identical? type :virtual) "virtual-")
                  (name state))
            use [:use {:class css :x x :y y :xlinkHref (str "#" xlink)
                       ;; :on-click #(nodeclick id)
                       }]
            top (- y 11)
            ;; U+25B9 	WHITE RIGHT-POINTING SMALL TRIANGLE
            ;; and construct-label should be construct-display-name
            show-args-pref? true ;; future user choice (default true)
            show-args? (and show-args-pref? (not (nil? args)))
            show-fn? (and show-args? (not (nil? display-name)))
            display-name (or display-name label) ;; FIXME backward compat
            display-name (str
                           display-name
                           (if show-fn? "(")
                           (if show-args? (apply str (interpose ", " args)))
                           (if show-fn? ")"))
            display-name (if rewrap-htn-labels
                    (rewrap-label settings display-name)
                    display-name)
            display-name (str display-name
                           (if label "\n") label
                           (if sequence-label "▹ ") sequence-label)
            _ (println "DISPLAY-NAME" id (str "=>" display-name "<="))
            lines (if (not (empty? display-name))
                    (string/split display-name #"\n"))
            display-names (if (and lines (not hidden))
                            (for [i (range (count lines))
                                  :let [lab (get lines i)]]
                              [:text {:class (str "node-label "
                                               font-family " "
                                               font-size " "
                                               font-weight)
                                      :textAnchor "middle"
                                      :x x :y (+ top (* i ychar))} lab]))
            y-node-id (+ y (if (keyword-identical? network-type :hem-network) 30 20))
            ;; tip (str (name id) " " (name state))
            tip (str (name id) " " (name state) " "
                  ;; number ;; the node number is for debugging
                  )
            ]
        (concatv
          [:g.node
           (if (fn? graph-click)
             {:on-click (partial graph-click props)
              :on-context-menu (partial graph-click props)
              })
           ]
          [(if (keyword-identical? type :htn-expanded-method)
             (let [r 12
                   hem-size (* 4 r)
                   hem-offset (- (/ hem-size 2))]
               [:rect {:class (target-class (and selected? (not hidden)))
                       :x (+ (* 2 hem-offset) x) :y (+ hem-offset y)
                       :rx 3 :ry 3
                       :width (* 2 hem-size) :height hem-size}])
             [:circle {:class (target-class (and selected? (not hidden)))
                       :cx x :cy y :r 16}])]
          [use]
          display-names
          (if node-ids?
            [[:text {:textAnchor "middle" :x x :y y-node-id} (name id)]])
          (if tooltips
            [(tooltip settings :g.node-tooltip.hide x y tip type)]))))))

(def node-memo (memoize node))

;; returns x y d (where x y are the label position)
(defn link-arc [type x0 y0 x1 y1]
  (let [x (/ (+ x0 x1) 2)
        y (/ (+ y0 y1) 2)]
    (if (constraint? type)
      (let [dx (- x1 x0)
            dy (- y1 y0)
            dh (sqrt (+ (* dx dx) (* dy dy)))
            ranksep 70 ;; grab from state?
            ratio (/ dh ranksep)
            offset (case type ;; FIXME differentiate constraints
                     :reward>=-constraint 0.8
                     :cost<=-constraint 0.9
                     ;; 1.0) ;; ORIGINAL
                     1.5)
            factor (case type ;; FIXME differentiate constraints
                     :reward>=-constraint 0.75
                     :cost<=-constraint 0.9
                     ;; 1.1) ;; ORIGINAL
                     0.02)
            a (+ offset
                (* factor (max (- (min (/ dh ranksep) 16) 2) 0)))
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
      ;; Create a middle point for the path element to support cancel marker
      ;; https://www.w3.org/TR/SVG/paths.html#PathData
      [x (- y 2)
       (str "M" x0 " " y0 "L" x " " y " " x1 " " y1)
       0])))

(def link-arc-memo (memoize link-arc))

(defn construct-edge-tip [id state number cost reward probability
                          controllable plant plantid argsmap guard]
  (let [tip (str
              (name id)
              " "
              (name state)
              ;; " " number
              " : "
              plant
              (if plantid "[")
              plantid
              (if plantid "]")
              " "
              argsmap
              " controllable: "
              controllable
              (if (non-zero? cost) " cost: ")
                (if (non-zero? cost) cost)
                (if (non-zero? reward) " reward: ")
                (if (non-zero? reward) reward)
                (if (non-zero? probability) " probability: ")
                (if (non-zero? probability) probability)
                (if guard " guard: ")
                guard)]
    tip))

(defn safe-type-name [type]
  (case type
    :cost<=-constraint "costle-constraint"
    :reward>=-constraint "rewardge-constraint"
    (name type)))

(defn edge [{:keys[plans/ui-opts plan/plid edge/id
                   edge/type edge/state edge/from edge/to
                   edge/cost edge/reward edge/controllable
                   edge/probability edge/guard
                   edge/selected? edge/hidden
                   edge/plant edge/plantid edge/argsmap
                   edge/number
                   edge/marker-mid]
             :as props}
            node-factory]
  (let [{:keys [ui/network-type
                ui/graph-click ui/settings]} ui-opts
        {:keys [settings/tooltips]} settings]
    (html
      ;; (if (= rendering :graphic)
        (let [[x0 y0] [(:node/x from) (:node/y from)]
              [x1 y1] [(:node/x to) (:node/y to)]
              [x y d ratio] (link-arc-memo type x0 y0 x1 y1)
              virtual? (keyword-identical? type :virtual)
              cnstr? (constraint? type)
              length-class (if (and cnstr? (> ratio 3))
                             (if (< ratio 10) "-long" "-very-long"))
              marker-end (if cnstr?
                           (if hidden
                             nil
                             (str "url(#arrow"
                               "-" (name state)
                               length-class
                               ")"))
                           (if virtual? "url(#arrowlight)"
                               (if (keyword-identical? network-type :hem-network)
                                 "url(#arrowhem)"
                                 (if hidden
                                   nil ;; "url(#arrowlight)"
                                   ;; "url(#arrowhead)"
                                   (str "url(#arrowhead-" (name state) ")")))))
              marker-start nil ;; (if (keyword-identical? type :choice-edge) "url(#choicehem)")
              marker-mid (cond (= :cancel marker-mid)
                               (str "url(#cancel)")
                               (= :cancelled marker-mid)
                               (str "url(#cancelled)")
                               (= :check-mark marker-mid)
                               (str "url(#check_mark)")
                               :else nil)
              class (str (safe-type-name type) "-"
                      (if hidden "hidden" (name state)) length-class
                      ;; consider adding just state to the class list
                      ;; " "
                      ;; (name state)
                      )
              attrs (assoc-if {:class class :d d}
                              :marker-start marker-start
                              :marker-mid marker-mid
                              :marker-end marker-end)
              target-attrs (if (#{:activity :delay-activity :aggregation
                                  :choice-edge :parallel-edge} type)
                             {:class (target-class
                                       (and selected? (not hidden))) :d d})
              tip (construct-edge-tip id state number cost reward probability
                    controllable plant plantid argsmap guard)
              ]
          (if (and hidden (keyword-identical? type :aggregation))
            [:desc "hidden"]
            [:g.edge
             (if (and (#{:activity :delay-activity :aggregation
                         :parallel-edge :choice-edge} type) (fn? graph-click))
               {:on-click (partial graph-click props)
                :on-context-menu (partial graph-click props)})
             (if target-attrs
               [:path target-attrs])
             [:path attrs]
             (if tooltips
               [(tooltip settings :g.edge-tooltip.hide x y tip type)])])))))

(def edge-memo (memoize edge))

(defn construct-label-classic [name label sequence-label plant plantid command
                               args type value]
  (let [argstr (apply str (interpose ", " args))
        full (str plant
               (if-not (empty? plantid) "[")
               plantid
               (if-not (empty? plantid) "]")
               (if (and (not (empty? command)) (not= command "delay")) ".")
               command
               (if (not (empty? argstr)) " ")
               argstr)
        full (if (not (empty? full)) (str full "()"))
        label (str
                (if-not (empty? full) full name)
                (if label " (") label
                ;; U+25B9 	WHITE RIGHT-POINTING SMALL TRIANGLE
                (if sequence-label " ▹ ")
                sequence-label
                (if label ")"))
        label (if value
                (str label
                  (case type
                    :cost<=-constraint "cost<= "
                    :reward>=-constraint "reward>= "
                    " ")
                  value)
                label)]
    label))

(defn construct-label [name label display-name sequence-label plant plantid
                       command args type value]
  (if (constraint? type)
    (case type
      :cost<=-constraint (str "cost<= " value)
      :reward>=-constraint (str "reward>= " value)
      (str value))
    (let [show-args-pref? true ;; future user choice (default true)
          show-args? (and show-args-pref? (not (nil? args)))
          show-fn? (and show-args? (not (nil? display-name)))]
      (str
        display-name
        (if show-fn? "(")
        (if show-args? (apply str (interpose ", " args)))
        (if show-fn? ")")
        (if label " ")
        label
        (if sequence-label " ▹ ")
        sequence-label))))

;; older approach
    ;; (if name
    ;;   ;; WAS just name
    ;;   (if label
    ;;     (str
    ;;       (or display-name name)
    ;;       " " (str label)) ;; show label for debugging!
    ;;     (or display-name name))
    ;;   (let [argstr (apply str (interpose ", " args))]
    ;;     (str
    ;;       command
    ;;       (if command "(")
    ;;       argstr
    ;;       (if command ")")
    ;;       (if label " ")
    ;;       label
    ;;       (if sequence-label " ▹ ")
    ;;       sequence-label)))))

(defn label [{:keys[plans/ui-opts edge/id edge/type edge/name
                    edge/label edge/display-name
                    edge/sequence-label edge/hidden
                    edge/plant edge/plantid edge/command edge/args
                    edge/from edge/to edge/value
                    edge/cost edge/reward
                    ;; edge/cost<= edge/reward=>
                    edge/probability edge/guard
                    edge/order] :as props}]
  (let [{:keys [ui/show-virtual? ui/edge-ids? ui/settings]} ui-opts
        {:keys [settings/ychar]} settings
        virtual? (keyword-identical? type :virtual)
        label? (or show-virtual? (not virtual?))
        display-name (if label? (construct-label name label display-name
                                  sequence-label
                                  plant plantid command args type value))
        display-name (if edge-ids?
                (str display-name " = "
                  (clojure.core/name id)
                  (if order " {")
                  order
                  (if order "}"))
                display-name)
        extra nil ;; (construct-extra cost reward probability guard)
        [x0 y0] [(:node/x from) (:node/y from)]
        [x1 y1] [(:node/x to) (:node/y to)]
        [x y d ratio] (if label?
                        (link-arc-memo type x0 y0 x1 y1)
                        [x0 y0 0 0])
        y (case type
            :reward>=-constraint (+ (- y ychar ychar) 6)
            :cost<=-constraint (+ (- y ychar) 4)
            (+ y 2);; y
            )
        order (if edge-ids? order)
        above [:text {:textAnchor "middle"
                      :x (- x 5) :y (+ y -3 (if order (* 7 order) 0))} display-name]
        below (if (not (empty? extra))
                [:text {:textAnchor "middle" :x x :y (+ y 12)} extra])]
    (html
      (if hidden
        [:desc "hidden"]
        (if below
          [:g above below]
          above)))))

(def label-memo (memoize label))

(defn network [{:keys [plans/ui-opts network/id network/type
                       network/nodes network/edges
                       network/width network/height] :as props}
               node-factory edge-factory label-factory]
  (let [{:keys [ui/show-virtual?]} ui-opts
        edges-to-show (if show-virtual? edges
                          (remove #(keyword-identical? :virtual (:edge/type %)) edges))
        edges? (pos? (count edges-to-show))
        nodes-to-show (if show-virtual? nodes
                          (remove #(keyword-identical? :virtual (:node/type %)) nodes))
        nodes? (pos? (count nodes-to-show))]
    (html
      (concatv
        [:g]
        ;; (if (keyword-identical? rendering :graphic)
          (concatv
            (if edges? (map edge-factory edges-to-show))
            (if nodes? (map node-factory nodes-to-show))
            (if edges? (map label-factory edges-to-show)))))))

(defn plan [{:keys [plans/ui-opts plan/plid plan/name plan/type plan/networks]
             :as props}
            network-factory]
  (let [{:keys [ui/show-network]} ui-opts
        networks-to-show (if (or (nil? show-network) (keyword-identical? :all show-network))
                           networks
                           (filter #(keyword-identical? (network-key-fn %) show-network) networks))
        networks? (pos? (count networks-to-show))]
    (html
      (concatv
        [:g]
        (map network-factory networks-to-show)))))

(defn markers []
  (let [choicehem "<marker id=\"choicehem\" orient=\"auto\" markerHeight=\"5\" markerWidth=\"5\" refY=\"0\" refX=\"-70\" viewBox=\"-5 -5 10 10\"><circle r=\"5\"></circle></marker>\n"
        arrowhem "<marker id=\"arrowhem\" orient=\"auto\" markerHeight=\"5\" markerWidth=\"5\" refY=\"0\" refX=\"5\" viewBox=\"0 -5 10 10\"><path d=\"M0,-5L10,0L0,5\"></path></marker>\n"
        arrowlight "<marker id=\"arrowlight\" orient=\"auto\" markerHeight=\"4\" markerWidth=\"4\" refY=\"0\" refX=\"19\" viewBox=\"0 -5 10 10\"><path d=\"M0,-5L10,0L0,5\"></path></marker>\n"
        arrowhead "<marker id=\"arrowhead@STATE@\" orient=\"auto\" markerHeight=\"4\" markerWidth=\"4\" refY=\"0\" refX=\"17\" viewBox=\"0 -5 10 10\"><path d=\"M0,-5L10,0L0,5\"></path></marker>\n"
        arrowhead-style (apply str
                          (interpose "\n"
                            (for [state (map name (keys edge-states))]
                              (string/replace arrowhead
                                "@STATE@" (str "-" state)))))
        arrow (str
                "<marker id=\"arrow@STATE@\" orient=\"auto\" markerHeight=\"3\" markerWidth=\"3\" refY=\"-2\" refX=\"22\" viewBox=\"0 -5 10 10\"><path d=\"M0,-5L10,-1L1,5\"></path></marker>\n"
                "<marker id=\"arrow@STATE@-long\" orient=\"auto\" markerHeight=\"3.5\" markerWidth=\"3.5\" refY=\"-0.5\" refX=\"19\" viewBox=\"0 -5 10 10\"><path d=\"M0,-5L10,0L0,5\"></path></marker>\n"
                "<marker id=\"arrow@STATE@-very-long\" orient=\"auto\" markerHeight=\"8\" markerWidth=\"8\" refY=\"0\" refX=\"15\" viewBox=\"0 -5 10 10\"><path d=\"M0,-5L10,0L0,5\"></path></marker>\n")
        arrow-style (apply str
                      (interpose "\n"
                        (for [state (map name (keys edge-states))]
                          (string/replace arrow
                            "@STATE@" (str "-" state)))))
        cancel "<marker id=\"cancel\" markerWidth=\"10\" markerHeight=\"10\" refX=\"0\" refY=\"0\"\n            viewBox=\"-5 -5 10 10\" markerUnits=\"userSpaceOnUse\"\n            fill=\"none\">\n      <path d=\"M -4,-4 L 4,4 M -4,4 L 4,-4\" stroke=\"white\" stroke-width=\"4\"/>\n      <path d=\"M -4,-4 L 4,4 M -4,4 L 4,-4\" stroke=\"gray\" stroke-width=\"2\"/>\n    </marker>\n"
        cancelled "<marker id=\"cancelled\" markerWidth=\"10\" markerHeight=\"10\" refX=\"0\" refY=\"0\"\n            viewBox=\"-5 -5 10 10\" markerUnits=\"userSpaceOnUse\"\n            fill=\"none\">\n      <path d=\"M -4,-4 L 4,4 M -4,4 L 4,-4\" stroke=\"white\" stroke-width=\"4\"/>\n      <path d=\"M -4,-4 L 4,4 M -4,4 L 4,-4\" stroke=\"black\" stroke-width=\"2\"/>\n    </marker>\n"
        check_mark "<marker id=\"check_mark\" markerWidth=\"10\" markerHeight=\"10\" refX=\"5\" refY=\"5\"\n            viewBox=\"0 0 10 10\" markerUnits=\"userSpaceOnUse\"\n            fill=\"none\">\n      <path d=\"M0,7.5 L3.33,10 L10,0\" stroke=\"#009900\" stroke-width=\"1.25\"/>\n</marker>\n"
        ]
    (str
      choicehem
      arrowhem
      arrowlight
      arrowhead-style
      arrow-style
      cancel
      cancelled
      check_mark)))

(defn svg-defs []
  (let [r 10
        hem-size (* 4 r)
        hem-offset (- (/ hem-size 2))
        h (/ r 3.67) ;; half radius of hexagon circle
        dh (* SQRT3 h)
        r_2 (/ r 2)
        dx r_2
        dy (- r 3)
        parallel-path (str "M" (- dx) " " (- dy) "L" (- dx) " " dy
                        "M" dx " " dy "L" dx " " (- dy))
        unchoice-path (str "M" (- dh) " " (+ h)
                        "L" (- dh) " " (- h)
                        "L" 0 " " (- (* 2 h))
                        "L" (+ dh) " " (- h)
                        "L" (+ dh) " " (+ h)
                        "L" 0 " " (+ (* 2 h))
                        "Z") ;; "L" (- dh) " " (+ h)
        ] ;; shortest radius
    [:defs
     [:g#state
      [:circle {:class "node" :r r}]]
     [:g#state-hidden
      [:circle {:class "node" :r r}]]
     [:g#parallel
      [:circle {:class "node" :r r}]
      [:path {:class "parallel-path" :d parallel-path}]]
     [:g#parallel-hidden
      [:circle {:class "node" :r r}]
      [:path {:class "parallel-path-hidden" :d parallel-path}]]
     [:g#choice
      [:circle {:class "node" :r r}]
      [:circle {:class "choice-circle" :r r_2}]]
     [:g#choice-hidden
      [:circle {:class "node" :r r}]
      [:circle {:class "choice-circle-hidden" :r r_2}]]
     [:g#unchoice ;; uncontrolled choice
      [:circle {:class "node" :r r}]
      [:path {:class "unchoice-path" :d unchoice-path}]]
     [:g#unchoice-hidden ;; uncontrolled choice
      [:circle {:class "node" :r r}]
      [:path {:class "unchoice-path-hidden" :d unchoice-path}]]
     [:g#hem
      [:rect {:class "hem"
              :x (* 2 hem-offset) :y hem-offset
              :rx 3 :ry 3
              :width (* 2 hem-size) :height hem-size}]]
     [:g#hem-hidden
      [:rect {:class "hem-hidden"
              :x (* 2 hem-offset) :y hem-offset
              :rx 3 :ry 3
              :width (* 2 hem-size) :height hem-size}]]
     [:g#markers {:dangerouslySetInnerHTML {:__html (markers)}}]
     [:linearGradient#menu-gradient {:x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"}
      [:stop.menu-stop1 {:offset "0%"}]
      [:stop.menu-stop2 {:offset "50%"}]
      [:stop.menu-stop3 {:offset "100%"}]]
     ]))

(defn show-menu [settings menu]
  (let [{:keys [settings/xchar settings/ychar]} settings
        {:keys [node edge x y options]} menu
        n (count options)
        max-text (reduce #(max %1 (count (:text %2))) 0 options)
        w (* (+ max-text 2) xchar)
        xo (+ x 2)
        yo (+ y 2)
        wo (- w 4)
        ho (inc ychar)
        yt (+ y ychar)
        h (+ (* n ho) 4)]
     (apply concatv
       [:g.menu
        [:rect.menu-box {:x x :y y :width w :height h}]]
       (for [i (range n) :let [option (get options i)]]
         [[:rect.menu-option {:x xo :y (+ yo (* i ho))
                              :width wo :height ho
                              :on-click (partial (:fn option) node edge option)}]
          [:text {:x (+ x xchar) :y (+ yt (* i ho))} (:text option)]]))))

(defn calc-bigplan [width height vp-width vp-height zoom pan]
  (let [vp-ratio (/ vp-height vp-width)
        ratio (/ height width)
        vertical? (> ratio vp-ratio) ;; constrained vertically?
        ;; _ (println "CALC-BIGPLAN height" height "width" width "ratio" ratio)
        ;; _ (println "vp-height" vp-height "vp-width" vp-width "vp-ratio" vp-ratio
        ;;     "vertical?" vertical?) ;; DEBUG
        [pan-x pan-y] pan
        [big-w big-h] (mapv #(* % zoom)
                        (if vertical?
                          [(/ vp-height ratio) vp-height]
                          [vp-width (* vp-width ratio)]))
        [offset-x offset-y] (if vertical?
                              [(if (zero? pan-x)
                                 (/ (max (- vp-width big-w) 0) 2) 0) 0]
                              [0 (if (zero? pan-y)
                                   (/ (max (- vp-height big-h) 0) 2) 0)])
        ;; DEBUG
        _ (println "zoom" zoom "pan" pan "offset-x" offset-x "offset-y" offset-y)
        [big-left big-top] [(- offset-x (* pan-x big-w))
                            (- offset-y (* pan-y big-h))]
        bp [big-left big-top big-w big-h]]
    bp))

(defn plans [{:keys [plans/pan-zoom plans/ui-opts plans/plans] :as props}
             plan-factory]
  (let [{:keys [pan-zoom/width pan-zoom/height
                pan-zoom/vp-width pan-zoom/vp-height
                pan-zoom/pan pan-zoom/zoom]} pan-zoom
        {:keys [ui/show-plan ui/graph-click ui/menu ui/settings]} ui-opts
        plans-to-show (if (or (nil? show-plan) (keyword-identical? :all show-plan))
                        plans
                        (filter #(keyword-identical? (:plan/plid %) show-plan) plans))
        plans? (pos? (count plans-to-show))
        ;; rendering (or rendering :graphic)
        loading? (not (and width height))
        [width height] [(or width 800) (or height 800)]
        viewbox  (str "0 0 " width " " height)
        [big-left big-top big-w big-h] (calc-bigplan width height
                                         vp-width vp-height zoom pan)]
    (html
      [:div#plans
       (if loading?
         [:div#load-container.load-container.load5
          [:div#loader.loader "Loading..."]]
         [:svg#bigplan {:viewBox viewbox
                        :style {:top (str big-top "px")
                                :left (str big-left "px")}
                        :width (str big-w "px") :height (str big-h "px")
                        :on-click (partial graph-click nil)}
          (svg-defs)
          (concatv
            [:g#planviz
             ;; blue rect for entire graph
             ;; [:rect.plans {:x 0 :y 0
             ;;               :width (- width 2) :height (- height 2)}]
             ]
            (map plan-factory plans-to-show)
            (if menu [(show-menu settings menu)]))])
       ])))

(defn message-box [{:keys [message-box/id message-box/value] :as props}]
  (let [id (or id :mb-wat)
        tag (keyword (str "div#mb-" (name id) ".message-box"))]
    (html
      [tag value])))

(defn input-box [{:keys [input-box/id input-box/numeric? input-box/value
                         input-box/placeholder input-box/size] :as props}]
  (let [id-str (name id)
        ;; div-tag (keyword (str "div#ib-" id-str ".input-box"))
        input-tag (keyword (str "input#" id-str))
        attrs (assoc-if
                {:type "text"
                 :value (or value "")}
                :placeholder placeholder
                :size size)]
    (html [input-tag attrs])))

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

(defn show-help [help]
  (let [{:keys [help/shown help/help-click help/content]} help]
    (if shown
      [:div#help-menu.help-shown
       [:div#help-menu-close
        (if (fn? help-click)
          {:on-click (partial help-click)}) "✕"]
       [:b "Help Menu"] " " [:i "(press ESC to exit)"]
       [:br]
       content]
      [:div#help-menu.help-hidden])))


(defn show-settings [settings input-box-factory xchar-box ychar-box filename-box]
  (let [{:keys [settings/shown? settings/settings-action
                settings/font-family settings/font-size settings/font-weight
                settings/rewrap-htn-labels settings/auto
                settings/tooltips settings/filename settings/filenames
                settings/xchar settings/ychar]} settings
        label "Example Node Label\nWith some very long lines of details"
        ;; label "GetDataAndInterpretGetDataAndInterpret(A B)"
        label (if rewrap-htn-labels
                (rewrap-label settings label)
                label)
        filenames (set filenames)
        filenames (vec (if (filenames filename) filenames
                           (conj filenames filename)))]
    (if shown?
      [:div#settings-menu.settings-shown
       [:div#settings-menu-close
        {:on-click (partial settings-action :settings-menu-close)}
        "✕"]
       [:b "Settings Menu"] " " [:i "(press ESC to exit)"]
       [:br]
       [:br]
       "Node font: "
       (vec
         (cons
           :select#font-family
           (cons
             {:value font-family
              :on-change (partial settings-action :font-family)}
             (for [k (sort (keys font-families))]
               [:option {:value k} (get font-families k)]))))
       " "
       (vec
         (cons
           :select#font-size
           (cons
             {:value font-size
              :on-change (partial settings-action :font-size)}
             (for [k (keys font-sizes)]
               [:option {:value k} (get font-sizes k)]))))
       " "
       (vec
         (cons
           :select#font-weight
           (cons
             {:value font-weight
              :on-change (partial settings-action :font-weight)}
             (for [k (keys font-weights)]
               [:option {:value k} (get font-weights k)]))))
       " "
       (vec
         (cons
           (keyword (str "div#examplenode."
                      font-family "." font-size "." font-weight))
           (interpose [:br] (string/split label "\n"))))
       [:br]
       [:br]
       "Rewrap node labels?: "
       [:input#rewrap-htn-labels
        (assoc-if {:type "checkbox" :value true
                   :on-change (partial settings-action :rewrap-htn-labels)}
          :checked rewrap-htn-labels)]
       " Character width (px): "
       (input-box-factory xchar-box)
       " Character height (px): "
       (input-box-factory ychar-box)
       [:br]
       [:br]
       "Auto mode: "
       [:input#auto
        (assoc-if {:type "checkbox" :value true
                   :on-change (partial settings-action :auto)}
          :checked auto)]
       " Show tooltips?: "
       [:input#tooltips
        (assoc-if {:type "checkbox" :value true
                   :on-change (partial settings-action :tooltips)}
          :checked tooltips)]
       [:br]
       [:br]
       "Settings filename: "
       [:br]
       (vec
         (cons
           :select#filenames
           (cons
             {:value filename
              :selected true
              :size 5
              :on-change (partial settings-action :filenames)}
             (for [f (sort filenames)]
               [:option {:value f} f]))))
       [:br]
       [:button#load {:on-click (partial settings-action :load)}
        "Load"]
       " "
       [:button#save {:on-click (partial settings-action :save)}
        "Save"]
       [:i " Save As... "]
       (input-box-factory filename-box)]
      [:div#settings-menu.settings-hidden])))

(defn application [{:keys [app/message-box app/cmd-box app/pan-zoom
                           app/title app/mode app/help app/settings
                           app/xchar-box app/ychar-box app/filename-box] :as props}
                   message-box-factory input-box-factory pan-zoom-factory]
  (html
    [:div#application
     [:div#title (str title "  " (name (or mode :manual)))]
     [:div#logo]
     (show-help help)
     (message-box-factory message-box)
     [:div#ib-cmd.input-box
      (input-box-factory cmd-box)]
     (pan-zoom-factory pan-zoom)
     (show-settings settings input-box-factory xchar-box ychar-box filename-box)]))
