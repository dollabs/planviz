;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns planviz.tplan
    "temporal planning layout algorithm"
    (:require [cljs.pprint :refer [pprint]] ;; DEBUG
              [clojure.set :as set]
              [avenir.utils :as au :refer [assoc-if]]
              [avenir.math :as math :refer [atan]]
              [webtasks.tasks :as tasks :refer [chain sleep success! error!]]
              [plan-schema.core :as pschema :refer [composite-key]]
              ;; [planviz.components :as comp]
              [planviz.ui :as ui
               :refer [node-key-fn edge-key-fn network-key-fn non-zero?
                       constraint? activity-type? begin?]]))

;; helper function
(defn reversev [s]
  (vec (reverse s)))

(defn safe-min [a b]
  (if a
    (if b
      (min a b)
      a)
    (if b
      b
      math/js-max-int)))

(defn safe-max [a b]
  (if a
    (if b
      (max a b)
      a)
    (if b
      b
      math/js-min-int)))

;; -------------------------------

(def graph-params-tpn {:xsize 10
                       :ysize 10
                       :nodesep 40
                       :ranksep 50 ;; (* ??? (+ ysize nodesep))
                       :label-char 7
                       :min-length 1
                       :plid nil
                       :network-plid-id nil
                       :ranking {}})

(def graph-params-htn {:xsize 10; 50
                       :ysize 20; 50
                       :nodesep 80 ;; 250
                       :ranksep 100 ;; 250 ;; (* ??? (+ ysize nodesep))
                       :label-char 7
                       :min-length 1
                       :plid nil
                       :network-plid-id nil
                       :ranking {}})

(def graph-params (atom {}))
(def graph (atom {}))

(defn set-width-height [width height]
  (swap! graph
    (fn [g]
      (let [{:keys [plid network-plid-id]} @graph-params
            {:keys [network/network-by-plid-id]} g
            network (assoc (get network-by-plid-id network-plid-id)
                      :network/width width
                      :network/height height)
            network-by-plid-id (assoc network-by-plid-id
                                 network-plid-id network)]
          (assoc g
            :network/network-by-plid-id network-by-plid-id)))))

(defn temporal-constraint? [edge]
  (keyword-identical? (:edge/type edge) :temporal-constraint))

(defn all-nodes []
  (keys (:node/node-by-plid-id @graph)))

(defn all-edges []
  (keys (:edge/edge-by-plid-id @graph)))

(defn get-node [node-id]
  (get-in @graph [:node/node-by-plid-id node-id]))

(defn get-edge [edge-id]
  (get-in @graph [:edge/edge-by-plid-id edge-id]))

(defn update-node [node]
  (let [plid-id (node-key-fn node)
        ref [:node/node-by-plid-id plid-id]]
    (swap! graph update-in ref merge node)))

(defn update-edge [edge]
  (let [plid-id (edge-key-fn edge)
        ref [:edge/edge-by-plid-id plid-id]]
    (swap! graph update-in ref merge edge)))

(defn create-vnode [rank]
  (let [{:keys [plid network-plid-id]} @graph-params
        id (keyword (gensym "vnode-"))
        node {:plan/plid plid
              :node/id id
              :node/type :virtual
              :node/actvities #{}
              :node/constraints #{}
              :node/state :normal
              :node/hidden false
              :node/incoming []
              :node/outgoing []
              :node/rank rank
              :node/x 0
              :node/y 0}
        plid-id (node-key-fn node)
        ref [:node/node-by-plid-id plid-id]]
    (swap! graph
      (fn [g]
        (let [{:keys [network/network-by-plid-id
                      node/node-by-plid-id]} g
              network (update-in (get network-by-plid-id network-plid-id)
                        [:network/nodes] conj plid-id) ;; ref)
              network-by-plid-id (assoc network-by-plid-id
                                   network-plid-id network)
              node-by-plid-id (assoc node-by-plid-id
                                plid-id node)]
          (assoc g
            :network/network-by-plid-id network-by-plid-id
            :node/node-by-plid-id node-by-plid-id))))
    (swap! graph-params update-in [:ranking rank] conj plid-id)
    node))

(defn create-vedge [from-id to-id]
  (let [{:keys [plid network-plid-id]} @graph-params
        from (get-node from-id)
        to (get-node to-id)
        id (keyword (gensym "vedge-"))
        edge {:plan/plid plid
              :edge/id id
              :edge/type :virtual
              :edge/from from-id
              :edge/to to-id
              :edge/state :normal
              :edge/weight 1
              :edge/hidden false}
        plid-id (edge-key-fn edge)
        ref [:edge/edge-by-plid-id plid-id]]
    (swap! graph
      (fn [g]
        (let [{:keys [network/network-by-plid-id
                      edge/edge-by-plid-id]} g
              network (update-in (get network-by-plid-id network-plid-id)
                        [:network/edges] conj plid-id) ;; ref)
              network-by-plid-id (assoc network-by-plid-id
                                   network-plid-id network)
              edge-by-plid-id (assoc edge-by-plid-id
                                plid-id edge)]
          (assoc g
            :network/network-by-plid-id network-by-plid-id
            :edge/edge-by-plid-id edge-by-plid-id))))
    (update-node (update-in from [:node/outgoing] conj plid-id))
    (update-node (update-in to [:node/incoming] conj plid-id))
    edge))

;; set incoming and outgoing (ignore constraint edges / zero weight)
(defn add-incoming-outgoing [d]
  (println "TPLAN add-incoming-outgoing")
  (doseq [[edge-id edge] (:edge/edge-by-plid-id @graph)]
    (let [{:keys [edge/weight edge/from edge/to]} edge
          ;; _ (swap! notes conj (str "AIO " edge-id " from " from " to " to))
          from (get-node from)
          to (get-node to)]
      (update-node (update-in from [:node/outgoing] conj edge-id))
      (update-node (update-in to [:node/incoming] conj edge-id))
      ))
  true) ;; for chain

(defn map-nodes [node-fn]
  (doseq [[node-id node] (:node/node-by-plid-id @graph)]
    (node-fn node)))

(defn map-incoming [node edge-fn]
  (doall (map (comp edge-fn get-edge) (:node/incoming node))))

(defn map-outgoing [node edge-fn]
  (doall (map (comp edge-fn get-edge) (:node/outgoing node))))

(defn visit-nodes [node-id prev-visited node-fn]
  (if (prev-visited node-id)
    prev-visited
    (let [node (get-node node-id)
          visited (atom (conj prev-visited node-id))
          tovisit (remove nil? (node-fn node))] ;; visit any nodes returned
      (loop [visit (first tovisit) more (rest tovisit)]
        (when visit
          (swap! visited set/union (visit-nodes visit @visited node-fn))
          (recur (first more) (rest more))))
      @visited)))

(defn rank-sweep [min-length moves]
  (let [node-ids @moves]
    (reset! moves [])
    (doseq [node-id node-ids]
      (let [node (get-node node-id)]
        (map-incoming node
          (fn [edge]
            (when (#{:activity :null-activity :delay-activity
                     :parallel-edge :choice-edge :virtual}
                    (:edge/type edge))
              (let [old-rank (:node/rank node)
                    from (get-node (:edge/from edge))
                    rank (max old-rank (+ (:node/rank from) min-length))]
                ;; (println "DEBUG rank-sweep EDGE rank" rank)
                (when (not= rank old-rank)
                  (update-node (assoc node :node/rank rank))
                  (swap! moves conj (node-key-fn node))
                  nil)))))
        (map-outgoing node
          (fn [edge]
            (when (#{:activity :null-activity :delay-activity
                     :parallel-edge :choice-edge :virtual}
                    (:edge/type edge))
              (let [rank (+ (:node/rank node) min-length)
                    to (get-node (:edge/to edge))
                    old-rank (:node/rank to)
                    rank (max rank old-rank)]
                (when (not= rank old-rank)
                  (update-node (assoc to :node/rank rank))
                  (swap! moves conj (node-key-fn to)))))))))))

(defn rank [d]
  (let [{:keys [network-plid-id min-length]} @graph-params
        {:keys [network/network-by-plid-id]} @graph
        network (get network-by-plid-id network-plid-id)
        {:keys [network/begin]} network
        moves (atom [begin])]
    (println "TPLAN rank")
    (update-node (assoc (get-node begin) :node/rank 0))
    (while (pos? (count @moves))
      ;; (println "  rank moves" (count @moves))
      (rank-sweep min-length moves))
    true ;; for chain
    ))

(defn save-ranking [d]
  (let [{:keys [network-plid-id]} @graph-params
        ;; {:keys [node/node-by-plid-id]} @graph
        {:keys [network/network-by-plid-id]} @graph
        network (get network-by-plid-id network-plid-id)
        {:keys [network/begin]} network
        ]
    (println "TPLAN save-ranking")
    ;; save in ranking map -- in outgoing order!!!
    (swap! graph-params assoc :ranking {})
    (visit-nodes begin #{}
      (fn [node]
        (let [{:keys [node/rank]} node
              plid-id (node-key-fn node)]
          (when (not (neg? rank))
            (swap! graph-params
              (fn [gp]
                (let [nodes (get-in gp [:ranking rank])
                      nodes (if nodes (conj nodes plid-id) [plid-id])]
                  (assoc-in gp [:ranking rank] nodes))))
            (map-outgoing node
              (fn [edge]
                (:edge/to edge)))
            ))))
    true)) ;; for chain

(defn balance-sweep [d min-length node-ids done]
  ;; (println "  balance-sweep" (count node-ids))
  (if (empty? node-ids)
    (do
      (success! d true)
      true)
    (let [visit (atom #{})
          _ (doseq [node-id node-ids]
              (swap! done conj node-id)
              (let [node (get-node node-id)
                    real-incoming (atom [])]
                (map-incoming node
                  (fn [edge]
                    (when-not (constraint? edge)
                      (let [from (:edge/from edge)]
                        (if-not (keyword-identical?
                                  (:edge/type edge) :null-activity)
                          (swap! real-incoming conj from))
                        (if-not (or (@visit from) (@done from))
                          (swap! visit conj from))))))
                (map-outgoing node
                  (fn [edge]
                    (when (activity-type? edge)
                      (let [{:keys [node/rank]} node
                            node-plid-id (node-key-fn node)
                            {:keys [edge/to edge/type]} edge
                            to (get-node to)
                            rightmost (- (:node/rank to) min-length)
                            slack (- rightmost rank)]
                        ;; move last state nodes -OR- state nodes in sequence
                        (when (and (pos? slack)
                                (or (keyword-identical? type :null-activity)
                                  (= 1 (count @real-incoming))))
                          ;; move node rank to rightmost
                          (swap! graph-params
                            (fn [gp]
                              (let [old-nodes (get-in gp [:ranking rank])
                                    old-nodes (vec (filter
                                                     #(not= % node-plid-id)
                                                     old-nodes))]
                                (-> gp
                                  (assoc-in [:ranking rank] old-nodes)
                                  (update-in [:ranking rightmost] conj
                                    node-plid-id)))))
                          (update-node (assoc node :node/rank rightmost)))))))
                ))
          start (tasks/deferred)
          finish (-> start
                   (sleep 30) ;; pace
                   (chain #(balance-sweep d min-length @visit done)))]
      (tasks/on-realized finish #(identity true)) ;; consume finish
      (success! start true)
      true ;; finish
      )))

;; Nodes having equal in- and out-edge weights and multiple feasible
;; ranks are moved to a feasible rank with the fewest nodes.
;; As the ranks are initially biased to be low we'll start with the
;; end node and greedily balance ranks moving towards the head.
;; NOTE we must ignore temporal constraint edges
(defn balance [d]
  (let [dbalance (tasks/deferred)
        {:keys [network-plid-id min-length]} @graph-params
        {:keys [network/network-by-plid-id]} @graph
        network (get network-by-plid-id network-plid-id)
        {:keys [network/end]} network
        done (atom #{})]
    (println "TPLAN balance")
    (balance-sweep dbalance min-length [end] done)
    dbalance)) ;; for chain

;; values must be a sorted vector
(defn calc-median [values]
  (let [len (count values)
        m (quot len 2)]
    (cond
      (zero? len)
      0
      (odd? len)
      (get values m)
      (= 2 len)
      (/ (+ (first values) (second values)) 2)
      :else
      (let [vl (get values (dec m))
            vm (get values m)
            left (- vl (first values))
            right (- (last values) vm)]
        (/ (+ (* vl right) (* vm left)) (+ left right))))))

;; the minimum distance between nodes a and b on the same rank
(defn rho [a b]
  (let [{:keys [ysize nodesep]} @graph-params]
    (+ ysize nodesep)))

(defn calc-min [node node-ids]
  (loop [y-min 0 node-id (first node-ids) more (rest node-ids)]
    (if (or (nil? node-id) (keyword-identical? (node-key-fn node) node-id))
      y-min
      (let [prev (get-node node-id)
            y (+ (:node/y prev) (rho prev prev)) ;; fix if rho by node
            y-min (max y-min y)]
        (recur y-min (first more) (rest more))))))

;; sort nodes by up-priority
;; if (and (> up-priority 1) (not moved?))
;;   gather the y values of (not (or hidden (zero? weight))) incoming
;;   calc median
;;   set y to median
;;   now update y in nodes per rho
;; if y changed set moved?
;; nodes-by-plid-id (reduce #(assoc %1 %2 (get-node %2)) {} node-ids)
(defn medianpos [iteration]
  (let [{:keys [plan-type ranking]} @graph-params
        ranks (count (keys ranking))]
    ;; (println "TPLAN medianpos" iteration) ;; DEBUG
    (if (odd? iteration)
      (doseq [r (range (dec ranks) -1 -1)]  ;; bottom to top, downward priority
        (let [node-ids (get ranking r)
              nodes (map get-node node-ids)
              down-priority (map (fn [n] [(node-key-fn n)
                                          (:node/down-priority n)])
                              nodes)
              nodes-down (map first (sort-by second > down-priority))
              rho-y (rho nil nil)]
          ;; (println "  ranking" r "node-ids" node-ids) ;; DEBUG
          (loop [node-id (first nodes-down) more (rest nodes-down)]
            (when node-id
              (let [{:keys [node/y node/down-priority] :as node}
                    (get-node node-id)
                    y-min (calc-min node node-ids)
                    y-down (vec (sort
                                   (remove nil?
                                     (map-outgoing node
                                       (fn [edge]
                                         (let [{:keys [edge/to
                                                       edge/hidden
                                                       edge/weight]} edge
                                               y (if-not (or hidden
                                                           (zero? weight))
                                                   (:node/y (get-node to)))]
                                           y))))))
                    y-median (calc-median y-down)
                    y-new (max y-min
                            (if (or
                                (and (keyword-identical? plan-type :htn-network)
                                    ;; (nil? down-priority))
                                  (= down-priority 1))
                                (> down-priority 1)
                                ;; (neg? y)
                                )
                              y-median y))]
                (when (not= y-new y)
                  (update-node (assoc node :node/y y-new)))
                (recur (first more) (rest more)))))
          ;; To handle the case where a node with higher down priority
          ;; gets positioned before (op top of or to the left) of
          ;; a lower priority node double check rho here.
          (loop [y-min -1 node-id (first node-ids) more (rest node-ids)]
            (when node-id
              (let [node (get-node node-id)
                    y (:node/y node)
                    y-new (if (neg? y-min)
                            y ;; first one does not change position
                            (max y-min y))
                    y-min (+ y-new rho-y)] ;; min position for next one
                (when (not= y-new y)
                  ;; DEBUG
                  ;; (println "  RHO CHECK node" node-id  "moved" y "to" y-new)
                  (update-node (assoc node :node/y y-new)))
                (recur y-min (first more) (rest more)))))))
      (doseq [r (range ranks)] ;; top to bottom, upward priority
        (let [node-ids (get ranking r)
              nodes (map get-node node-ids)
              up-priority (map (fn [n] [(node-key-fn n)
                                        (:node/up-priority n)])
                            nodes)
              nodes-up (map first (sort-by second > up-priority))]
          (loop [node-id (first nodes-up) more (rest nodes-up)]
            (when node-id
              (let [{:keys [node/y node/up-priority] :as node}
                    (get-node node-id)
                    y-min (calc-min node node-ids)
                    y-up (vec (sort
                                 (remove nil?
                                   (map-incoming node
                                     (fn [edge]
                                       (let [{:keys [edge/from
                                                     edge/hidden
                                                     edge/weight]} edge
                                             y (if-not (or hidden
                                                         (zero? weight))
                                                 (:node/y (get-node from)))]
                                         y))))))
                    y-median (calc-median y-up)
                    y-new (max y-min
                            (if (or
                                (and (keyword-identical? plan-type :htn-network)
                                  (= up-priority 1))
                                (> up-priority 1))
                            y-median y))]
                (if-not (= y-new y)
                  (update-node (assoc node :node/y y-new)))
                (recur (first more) (rest more))))))))))

(defn calc-label-width [label-char n]
  (let [extra (if (> n 25) 4 6)]
    (* (+ n extra) label-char)))

(defn set-coord [plan-type]
  ;; y starts at 2 * nodesep, x starts at 1/2 ranksep
  ;; will calculate w and h
  ;; where w = rank * ranksep (starts at 0.5 ranksep)
  ;; and h = max y + 3 * nodesep (starts at 2 * nodesep)
  ;; (println "set-coord" plan-type)
  (let [{:keys [ranksep nodesep ranking label-char]} @graph-params
        ranks (range (count (keys ranking)))
        x0 (/ ranksep 2)
        y0 (* 2 nodesep)
        x-key (if (keyword-identical? plan-type :htn-network) :node/y :node/x)
        y-key (if (keyword-identical? plan-type :htn-network) :node/x :node/y)
        ]
    (loop [r (first ranks) more (rest ranks) x x0 ymax y0]
      (if-not r ;; done
        (if (keyword-identical? plan-type :htn-network)
          ;; normal
          ;; (set-width-height (+ ymax y0) x)
          ;; extra margin on the bottom
          ;; (set-width-height (+ ymax y0) (+ x ranksep))
          (set-width-height (+ ymax nodesep) x)
          ;; normal
          ;; (set-width-height (+ x x0) (+ ymax y0)))
          ;; extra margin on the bottom
          ;; (set-width-height (+ x x0) (+ ymax y0 y0))
          (set-width-height x (+ ymax y0))
          )
        (let [nodes (get ranking r)
              edge-fn (fn [edge] ;; returns xrank
                        (let [{:keys [edge/hidden edge/weight
                                      edge/name edge/label
                                      edge/type edge/value
                                      edge/sequence-label
                                      edge/plant edge/plantid
                                      edge/command edge/args
                                      edge/cost edge/reward
                                      edge/probability edge/guard]} edge
                              label (ui/construct-label name label sequence-label
                                      plant plantid command args type value)
                              ;; CONSIDER ui/construct-edge-tip length
                              ;; max-label (max (count label) (count tip))
                              max-label (count label)]
                          ;; unhide non-aggregation edges
                          (if (and (keyword-identical? plan-type :tpn-network)
                                hidden (not (keyword-identical? type :aggregation)))
                            (update-edge (assoc edge :edge/hidden false)))
                          (if (or hidden (zero? weight))
                            0
                            (calc-label-width label-char max-label))))
              node-fn (fn [node] ;; returns [xrank ymax]
                        (let [{:keys [node/y]} node
                              y (+ y y0)]
                          (update-node (assoc node x-key x y-key y))
                          [(reduce max 0 (map-outgoing node edge-fn))
                           y]))
              xrank_ymax (map (comp node-fn get-node) nodes)
              xrank (reduce max ranksep (map first xrank_ymax))
              ymax (reduce max ymax (map second xrank_ymax))]
          (recur (first more) (rest more) (+ x xrank) ymax))))))

(defn virtualize-path [real-from long-edge real-to]
  (let [vranks (range (inc (:node/rank real-from)) (:node/rank real-to))
        vnodes (map create-vnode vranks)]
    (loop [from real-from more (concat vnodes [real-to])]
      (let [to (first more)]
        (when to
          (create-vedge (node-key-fn from) (node-key-fn to))
          (recur to (rest more)))))
    ;; hide long-edge
    (update-edge (assoc long-edge :edge/hidden true))
    nil))

(defn add-virtual-nodes []
  (let [{:keys [network-plid-id]} @graph-params
        {:keys [node/node-by-plid-id]} @graph]
    (doseq [node-id (keys node-by-plid-id)]
      (let [node (get node-by-plid-id node-id)]
        (map-outgoing node
          (fn [edge]
            (let [{:keys [edge/to edge/weight edge/hidden edge/type]} edge
                  to-id to
                  to (get-node to-id)
                  from-rank (:node/rank node)
                  to-rank (:node/rank to)]
              (if (and (not hidden) (pos? weight)
                    (> to-rank (inc from-rank)))
                (virtualize-path node edge to)))))))))

;; determine number of crossings assuming left-id is to the
;; left of right-id
(defn crossing-pair [left-id right-id]
  (let [left (get-node left-id)
        right (get-node right-id)
        rank (:node/rank left)]
    (reduce + 0
      (map-incoming left
        (fn [left-edge]
          (let [{:keys [edge/hidden edge/weight
                        edge/from edge/type]} left-edge
                left-from (get-node from)
                left-rank (:node/rank left-from)
                left-tc? (constraint? type)
                ignore? (or (> left-rank rank)
                          (and (not left-tc?)
                            (or hidden (zero? weight))))]
            (if ignore?
              0
              (reduce + 0
                (map-incoming right
                  (fn [right-edge]
                    (let [{:keys [edge/hidden edge/weight
                                  edge/from edge/type]} right-edge
                          right-from (get-node from)
                          right-rank (:node/rank right-from)
                          right-tc? (constraint? type)
                          ignore? (or (> right-rank rank)
                                    (and (not right-tc?)
                                      (or hidden (zero? weight))))
                          left-p (:node/p left-from)
                          right-p (:node/p right-from)
                          c (if (or ignore? (<= left-p right-p))
                              0 (+ (if left-tc? 0.9 1) (if right-tc? 0.9 1)))]
                      c)))))))))))

(defn set-positions [rank]
  (loop [p 0 node-id (first rank) more (rest rank)]
    (when node-id
      (update-node (assoc (get-node node-id) :node/p p))
      (recur (inc p) (first more) (rest more)))))

;; calculate crossing cost from ranking
(defn crossing [ranking]
  (let [ranking (or ranking (:ranking @graph-params))]
    (reduce + 0
      (for [r (range (count (keys ranking)))]
        (let [rank (get ranking r)
              rank-len (count rank)]
          (set-positions rank)
          ;; evaluate crossings
          (if (zero? r)
            0
            (let [c
                  (reduce + 0
                    (for [i (range rank-len) j (range rank-len) :when (< i j)]
                      (crossing-pair (get rank i) (get rank j))))]
              c)))))))

;; returns [node-id median]
(defn median-for-node [top-to-bottom? node-id]
  (let [node (get-node node-id)
        positions
        (vec (remove nil?
               (if top-to-bottom?
                 (map-incoming node
                   (fn [edge]
                     (let [{:keys [edge/from edge/hidden edge/weight]} edge
                           p (if-not (or hidden (zero? weight))
                               (:node/p (get-node from)))]
                       p)))
                 (map-outgoing node
                   (fn [edge]
                     (let [{:keys [edge/to edge/hidden edge/weight]} edge
                           p (if-not (or hidden (zero? weight))
                               (:node/p (get-node to)))]
                       p))))))]
    [node-id (calc-median positions)]))

(defn median-for-node-top [node-id]
  (median-for-node true node-id))

(defn median-for-node-bottom [node-id]
  (median-for-node false node-id))

;; returns new ranking
(defn wmedian [ranking i]
  ;; (println "  DEBUG wmedian" i)
  (let [max-rank (count (keys ranking))]
    (if (even? i)
      (loop [r 0 ranking ranking] ;; top to bottom
        (if (= r max-rank)
          ranking
          (let [rank (get ranking r)
                node-median (map median-for-node-top rank)
                new-rank (mapv first (sort-by second < node-median))]
            (set-positions new-rank)
            (recur (inc r) (assoc ranking r new-rank)))))
      (loop [r (dec max-rank) ranking ranking] ;; bottom to top
        (if (neg? r)
          ranking
          (let [rank (get ranking r)
                node-median (map median-for-node-bottom rank)
                new-rank (mapv first (sort-by second < node-median))]
            (set-positions new-rank)
            (recur (dec r) (assoc ranking r new-rank))))))))

(defn get-forward-constraints [node]
  (let [{:keys [node/rank]} node]
    (remove nil?
      (for [cnstr-id (:node/constraints node)]
        (let [edge (get-edge cnstr-id)
              {:keys [edge/to]} edge
              to (get-node to)
              to-rank (:node/rank to)]
          (if (> to-rank rank)
            (:node/p to)))))))

(defn should-transpose? [u v u-v v-u]
  (if (> u-v v-u)
    true
    (if (< u-v v-u)
      false
      (let [left-node (get-node u)
            left-p (:node/p left-node)
            left-tcs (get-forward-constraints left-node)
            left-avg (if (pos? (count left-tcs))
                       (/ (reduce + 0 left-tcs) (count left-tcs)) left-p)
            right-node (get-node v)
            right-p (:node/p right-node)
            right-tcs (get-forward-constraints right-node)
            right-avg (if (pos? (count right-tcs))
                        (/ (reduce + 0 right-tcs) (count right-tcs)) right-p)
            delta-left (- left-p left-avg)
            delta-right (- right-avg right-p)
            transpose? (cond
                         (and (not (neg? delta-left)) (not (neg? delta-right)))
                         false
                         (and (not (neg? delta-left)) (neg? delta-right))
                         (> (- delta-right) delta-left)
                         (and (neg? delta-left) (not (neg? delta-right)))
                         (> (- delta-left) delta-right)
                         (and (neg? delta-left) (neg? delta-right))
                         (< delta-left delta-right)
                         :else
                         false)]
        ;; if the right constraint average is farther to the left
        ;; then the left constraint average is to the right, then transpose
        transpose?
        ))))

;; rank is a vector
;; returns [rank-changed? new-rank]
(defn transpose-rank [rank iteration]
  ;; (reset! citeration iteration) ;; DEBUG
  (let [rank-size (count rank)
        forward? (even? iteration)
        rank (if forward? rank (reversev rank))]
    (if (= 1 rank-size)
      [false (if forward? rank (reversev rank))]
      (loop [changed? false new-rank [] u (first rank) more (rest rank)]
        (let [v (first more)]
          (if (nil? v)
            [changed? (if forward? (conj new-rank u)
                          (reversev (conj new-rank u)))]
            (if forward?
              (let [u-v (crossing-pair u v)
                    v-u (crossing-pair v u)
                    transposed? (should-transpose? u v u-v v-u)
                    changed? (or changed? transposed?)
                    new-rank (conj new-rank (if transposed? v u))
                    next-u (if transposed? u v)]
                (recur changed? new-rank next-u (rest more)))
              (let [transposed? (> (crossing-pair v u) (crossing-pair u v))
                    changed? (or changed? transposed?)
                    new-rank (conj new-rank (if transposed? v u))
                    next-u (if transposed? u v)]
                (recur changed? new-rank next-u (rest more))))))))))

;; returns [r c]
(defn transpose [ranking iteration]
  (loop [improved? true ranking ranking]
    (if-not improved?
      [ranking (crossing ranking)]
      (let [max-rank (count (keys ranking))
            forward? (even? iteration)
            rank-start (if forward? 0 (dec max-rank))
            rank-end (if forward? max-rank -1)
            next-rank (if forward? inc dec)
            [improved? new-ranking]
            (loop [r rank-start changed? false ranking ranking]
              (if (= r rank-end)
                [changed? ranking]
                (let [rank (get ranking r)
                      [rank-changed? new-rank] (transpose-rank rank iteration)
                      changed? (or changed? rank-changed?)]
                  (when rank-changed?
                    (set-positions new-rank))
                  (recur (next-rank r) changed? (assoc ranking r new-rank)))))]
        (recur improved? new-ranking)))))

(defn mincross-sweep [d max-iterations i br-bc]
  (let [[br bc] br-bc]
    ;; (println "TPLAN mincross-sweep" i "bc" bc)
    (if (or (zero? bc) (= i max-iterations))
      (do
        (swap! graph-params assoc :ranking br)
        (tasks/timeout #(success! d true))
        true)
      (let [start (tasks/deferred)
            finish (-> start
                     (sleep 100) ;; pace
                     (chain #(mincross-sweep d max-iterations
                               (inc i) (transpose (wmedian br i) i))))]
        (tasks/on-realized finish #(identity true)) ;; consume finish
        (success! start true)
        true)))) ;; finish

(defn mincross [d]
  ;; add virtual nodes and edges
  (println "TPLAN mincross")
  (let [{:keys [plan-type]} @graph-params]
    (if (keyword-identical? plan-type :htn-network)
      true ;; for deferreds
      (let [dmincross (tasks/deferred)
            max-iterations 2 ;; heuristic - not found case needing more
            _ (add-virtual-nodes) ;; will mutate ranking!
            br (:ranking @graph-params)]
        (mincross-sweep dmincross max-iterations 0 [br (crossing br)])
        dmincross))))

;; -------------------------------------------------------------------

;; given two vectors [a b] [c d]
(defn min-max-fn
  ([]
   [math/js-max-int math/js-min-int])
  ([ab]
   (min-max-fn ab [nil nil]))
  ([ab cd]
   (let [[a b] ab
         [c d] cd]
     [(safe-min a c) (safe-max b d)])))

;; given opts which is all the options for a given end-id
;; returns beymns for the option option-id (or all option if not specified)
;; where beymns is a vector of beymn
;; beymn is a vector of [begin end y y-min y-max]
;; for each begin in option(s)
(defn get-option-beymns [options opts option-id]
  (let [option-ids (if option-id [option-id] (keys opts))
        begins (apply concat (map #(keys (get opts %)) option-ids))
        begins (sort ;; right to left
                 #(compare
                    (:node/rank (get-node %2))
                    (:node/rank (get-node %1)))
                 begins)
        beys (mapv (fn [begin]
                     (let [{:keys [node/y node/end]} (get-node begin)]
                       [begin end y]))
               begins)
        conj-mn (fn [bey]
                  (let [[b e y] bey
                        [m n] (if e ;; else is a state begin
                                (reduce
                                  min-max-fn [y y]
                                  (apply concat
                                    (map vals
                                      (vals
                                        (get-in @options [e :opts])))))
                                [y y])] ;; medianpos 2 will enforce this
                    (-> bey (conj m) (conj n))))
        beymns (mapv conj-mn beys)]
    beymns))

(defn min-max-begin [options parent mn replace?]
  (swap! options update-in
    parent
    (if replace?
      (constantly mn)
      (partial min-max-fn mn))))

(defn get-nexts [node]
  (let [nexts (remove nil?
                (map-outgoing node
                  (fn [edge]
                    (let [{:keys [edge/hidden edge/weight edge/to]} edge]
                      (if-not (or hidden (zero? weight)) to)))))]
    (vec
      (if (< (count nexts) 2)
        nexts ;; order by y
        (sort
          #(compare (:node/y (get-node %1)) (:node/y (get-node %2)))
          nexts)))))

(defn move-begin-opts [add-delta-mn]
  (letfn [(mbo
            ([opts]
             (reduce-kv mbo {} opts))
            ([opts option-id begins]
             (assoc opts option-id
               (reduce-kv
                 (fn [begins begin-id mn]
                   (assoc begins begin-id (add-delta-mn mn)))
                 {}
                 begins))))]
    mbo))

(defn move-begin [options e add-delta]
  (swap! options update-in [e :opts] (move-begin-opts add-delta)))

(defn sg-option-end [options option-id end-id]
  (let [{:keys [opts]} (get @options end-id)
        begins (keys (get opts option-id))]
    (when (> (count begins) 1)
      (let [beymns (get-option-beymns options opts option-id)
            min-y (reduce min math/js-max-int (map #(get % 3) beymns))
            calc-h (fn [beymn] (let [[b e y m n] beymn] (- y m)))
            hs (map calc-h beymns)
            max-h (reduce max 0 hs)
            new-y (+ min-y max-h)]
        (loop [stop end-id beymn (first beymns) more (rest beymns)]
          (let [[b e y m n] beymn
                delta (- new-y y)
                add-delta (partial + delta)
                add-delta-mn (fn [mn] (mapv add-delta mn))
                stop (or e stop)]
            (when-not (zero? delta) ;; offset inside by delta
              (swap! options update-in [end-id :opts option-id b]
                add-delta-mn) ;; update for this begin
              (when e ;; this is a choice/parallel
                (update-node (update-in (get-node e) [:node/y] add-delta)))
              (visit-nodes b #{stop}
                (fn [node]
                  (update-node (update-in node [:node/y] add-delta))
                  (if-let [end (:node/end node)] ;; interior begin/end
                    (move-begin options end add-delta-mn))
                  (map-outgoing node
                    (fn [edge]
                      (let [{:keys [edge/to edge/weight edge/hidden]} edge]
                        (if-not (or hidden (zero? weight))
                          to)))))))
            (if-not (empty? more)
              (recur b (first more) (rest more)))))))
    ;;node end, here we've balanced option-id
    (swap! options update-in [end-id :todo] dec)
    (let [{:keys [begin opt-order todo opts parent]} (get @options end-id)
          y-rho (rho nil nil)]
      (when (zero? todo)
        (loop [prev-max math/js-min-int
               opt-id (first opt-order)
               more (rest opt-order)]
          (let [beymns (get-option-beymns options opts opt-id)
                mn (reduce min-max-fn (map #(subvec % 3) beymns))
                [y-min y-max] mn
                start-y (if (= prev-max math/js-min-int) ;; work around cljs
                          y-min
                          (+ prev-max y-rho))
                delta (if (> y-min start-y) ;; round up to next y-rho
                        (- start-y y-min) 0)
                y-max (+ y-max delta)] ;; delta will change y-max
            ;; because each option might have been adjusted
            (when-not (zero? delta)
              (let [add-delta (partial + delta)
                    add-delta-mn (fn [mn] (mapv add-delta mn))]
                (visit-nodes opt-id #{end-id}
                  (fn [node]
                    (update-node (update-in node [:node/y] add-delta))
                    (if-let [end (:node/end node)] ;; interior b/end
                      (move-begin options end add-delta-mn))
                    (map-outgoing node
                      (fn [edge]
                        (let [{:keys [edge/to edge/weight edge/hidden]} edge]
                          (if-not (or hidden (zero? weight))
                            to))))))))
            (if parent
              (if (keyword-identical? opt-id (first opt-order)) ;; first one
                (min-max-begin options parent mn true)
                (min-max-begin options parent mn false)))
            (if-not (empty? more)
              (recur y-max (first more) (rest more)))))))))

;; visit from [node-id to end-id) exclusive
(defn sg-balance [options option-id begin-id node-id end-id]
  (let [node (get-node node-id)
        {:keys [node/type node/y node/end]} node
        mn (get-in @options [end-id :opts option-id begin-id])
        begin-mn (min-max-fn [y y] mn)
        nexts (get-nexts node)]
    ;; (println "SB" option-id "BEGIN-ID" begin-id "NODE-ID" node-id
    ;;   "END-ID" end-id "BEGIN-MN" begin-mn "NEXTS" nexts "#" (count nexts))
    (when (keyword-identical? node-id option-id) ;; this node-id is a new option for end-id
      ;; (println "this node-id is a new option for end-id")
      (swap! options assoc-in
        [end-id :opts option-id node-id] begin-mn))
    (if (begin? type)
      (do ;; create blank data for this end
        (swap! options assoc end
          {:begin node-id :opt-order nexts
           :parent [end-id :opts option-id node-id]
           :todo (count nexts)
           :opts {}})
        (when-not (keyword-identical? node-id option-id) ;; new begin
          ;; (println "New begin within this option")
          (swap! options assoc-in [end-id :opts option-id node-id] begin-mn))
        (doseq [next nexts]
          (sg-balance options next next next end))
        (let [node (get-node end) ;; is there more??
              nexts (get-nexts node)]
          (if (= 1 (count nexts)) ;; continue on same option-id end-id
            (sg-balance options option-id (first nexts) (first nexts) end-id)
            (when (zero? (count nexts))
              ;; (println "TPN END (*-begin)!")
              (sg-option-end options option-id :network-end)))))
      (if (keyword-identical? node-id end-id)
        (sg-option-end options option-id end-id)
        (if (and (zero? (count nexts))
              (keyword-identical? option-id (get-in @options [:network-end :begin])))
          (sg-option-end options option-id :network-end)
          (if (= 1 (count nexts)) ;; NOT the end
            (do
              (when-not (= begin-mn mn)
                (swap! options assoc-in
                  [end-id :opts option-id begin-id] begin-mn))
              (sg-balance options option-id begin-id (first nexts) end-id))))))))

(defn subgraph-balance []
  (let [options (atom {})
        {:keys [network/network-by-plid-id]} @graph
        {:keys [plid network-plid-id]} @graph-params
        network (get network-by-plid-id network-plid-id)
        {:keys [network/begin network/end]} network]
    (swap! options assoc :network-end
      {:begin begin
       :opt-order [begin]
       :todo 1
       ;; do NOT presume the first node will start at 0 0
       ;; :opts {begin {begin [0 0]}}
       })
    (sg-balance options begin begin begin :network-end)))

(defn position [d]
  (let [{:keys [plid network-plid-id]} @graph-params
        {:keys [plan/by-plid network/network-by-plid-id]} @graph
        plan0 (get by-plid plid)
        {:keys [plan/type plan/name]} plan0
        plan-type type
        network (get network-by-plid-id network-plid-id)
        {:keys [network/begin network/end network/nodes]} network]
    (println "TPLAN position")
    (doseq [node-id nodes]
      (let [node (get-node node-id)]
        (map-incoming node
          (fn [edge]
            (let [node (get-node node-id)
                  {:keys [edge/hidden edge/weight edge/type edge/from]} edge
                  edge-type type
                  {:keys [node/type node/outgoing]} (get-node from)
                  from-type type
                  edge-fn (fn [sum edge-id]
                            (let [{:keys [edge/type edge/hidden edge/weight]}
                                  (get-edge edge-id)]
                              (+ sum
                                (if (and (activity-type? type)
                                      (not (or hidden (zero? weight))))
                                  1 0))))
                  n-outgoing (reduce edge-fn 0 outgoing)
                  {:keys [node/up-priority node/type]} node
                  up-priority (+ (or up-priority 0)
                                (if (or (keyword-identical? edge-type :virtual)
                                      (and
                                        (or
                                          (keyword-identical? from-type :state)
                                          (keyword-identical? type :state))
                                        (= 1 n-outgoing)))
                                  0.1 0)
                                (if hidden 0 weight))]
              (update-node (assoc node :node/up-priority up-priority)))))
        (map-outgoing node
          (fn [edge]
            (let [node (get-node node-id)
                  {:keys [edge/hidden edge/weight edge/type edge/to]} edge
                  edge-type type
                  {:keys [node/type node/incoming]} (get-node to)
                  to-type type
                  edge-fn (fn [sum edge-id]
                            (let [{:keys [edge/type edge/hidden edge/weight]}
                                  (get-edge edge-id)]
                              (+ sum
                                (if (and (activity-type? type)
                                      (not (or hidden (zero? weight))))
                                  1 0))))
                  n-incoming (reduce edge-fn 0 incoming)
                  {:keys [node/down-priority node/type]} node
                  down-priority (+ (or down-priority 0)
                                  (if (and (keyword-identical? plan-type
                                             :htn-network)
                                        (keyword-identical? to end))
                                    -0.5 0)
                                  (if (or (keyword-identical? edge-type :virtual)
                                        (and
                                          (or
                                            (keyword-identical? to-type :state)
                                            (keyword-identical? type :state))
                                          (= 1 n-incoming)))
                                    0.1 0)
                                  (if hidden 0 weight))]
              (update-node (assoc node :node/down-priority down-priority)))))))
    (medianpos 0)
    (medianpos 1)
    (when (not= plan-type :htn-network)
      (medianpos 2)
      (medianpos 3)
      (subgraph-balance)
      (medianpos 4)
      (medianpos 5))
    (set-coord plan-type)
    true)) ;; for chain

(defn clean-nodes
  ([nodes]
   (reduce-kv clean-nodes {} nodes))
  ([nodes node-id node]
   (assoc nodes node-id
     (dissoc node
       :node/actvities
       :node/constraints
       :node/down-priority
       :node/hidden
       :node/incoming
       ;; :node/outgoing ;; DEBUG
       :node/p
       ;; :node/rank ;; DEBUG
       :node/up-priority))))

(defn edges-ref-nodes
  ([edges]
   (reduce-kv edges-ref-nodes {} edges))
  ([edges edge-id edge]
   (assoc edges edge-id
     (let [{:keys [edge/from edge/to]} edge]
       (-> edge
         (dissoc :edge/weight :edge/hidden)
         (assoc
           :edge/from [:node/node-by-plid-id from]
           :edge/to [:node/node-by-plid-id to]))))))

(defn initialize-network [network]
  (assoc network
    :network/width 700
    :network/height 50))

(defn initialize-nodes [m k node]
  (assoc m k
    (assoc node
      :node/state :normal
      :node/hidden false
      :node/incoming []
      :node/outgoing []
      :node/rank -1 ;; -1 means NOT in current network
      :node/x 0
      :node/y 0)))

(defn initialize-edges [m k edge]
  (assoc m k
    (assoc edge
      :edge/state :normal
      :edge/hidden (constraint? edge)
      :edge/weight (if (constraint? edge) 0 1)
      )))

(defn initialize-graph []
  (let [{:keys [plid network-plid-id]} @graph-params]
    (swap! graph
      (fn [g]
        (let [{:keys [network/network-by-plid-id
                      node/node-by-plid-id
                      edge/edge-by-plid-id]} g
              network (initialize-network (get network-by-plid-id
                                            network-plid-id))
              network-by-plid-id (assoc network-by-plid-id
                                   network-plid-id network)
              node-by-plid-id (reduce-kv initialize-nodes {} node-by-plid-id)
              edge-by-plid-id (reduce-kv initialize-edges {} edge-by-plid-id)]
          (assoc g
            :network/network-by-plid-id network-by-plid-id
            :node/node-by-plid-id node-by-plid-id
            :edge/edge-by-plid-id edge-by-plid-id))))))

(defn initialize-htn []
  (println "TPLAN initialize-htn")
  (map-nodes
    (fn [node]
      (when (> (count (:node/outgoing node)) 1)
        (let [order (atom 0)
              {:keys [node/outgoing]} node
              out-edges (map get-edge outgoing)
              pio-fn #(vector (or (:edge/order %) (swap! order inc))
                        (:plan/plid %) (:edge/id %))
              out-order (sort-by first (mapv pio-fn out-edges))
              outgoing (mapv (fn [[o p i]] (composite-key p i))
                         out-order)]
          (update-node (assoc node :node/outgoing outgoing))))))
  true)

;; annotate c-begin nodes with the sum of probabilities (if apropos)
(defn initialize-tpn []
  (println "TPLAN initialize-tpn")
  (map-nodes
    (fn [node]
      (let [{:keys [node/type node/outgoing node/end]} node
            sum-probability (fn [p edge-id]
                              (+ p
                                (or (:edge/probability (get-edge edge-id)) 0)))
            probability (if (keyword-identical? :c-begin type)
                          (reduce sum-probability 0 outgoing) 0)
            probability (if (non-zero? probability) probability)
            order (atom 0)
            out-edges (map get-edge outgoing)
            pio-fn #(vector (or (:edge/order %) (swap! order inc))
                      (:plan/plid %) (:edge/id %))
            out-order (sort-by first (mapv pio-fn out-edges))
            outgoing (mapv (fn [[o p i]] (composite-key p i))
                       out-order)]
        (update-node (assoc-if (assoc node :node/outgoing outgoing)
                       :node/probability probability))
        ;; if an unchoice node, indicate that on the c-end as well
        (when probability
          (update-node (assoc (get-node end) :node/probability probability)))
        (if (begin? type) ;; create aggregation edge
          (update-edge (assoc (create-vedge (node-key-fn node) end)
                         :edge/type :aggregation
                         :edge/hidden true
                         :edge/weight 0)))
        )))
  true)

(defn initialize-plan [d]
  (let [{:keys [plid network-plid-id]} @graph-params
        {:keys [plan/by-plid network/network-by-plid-id]} @graph
        {:keys [plan/type]} (get by-plid plid)]
    (if (keyword-identical? type :htn-network)
      (initialize-htn)
      (initialize-tpn))
    true))

(defn setup-graph [plan]
  (let [plid (first (keys (:plan/by-plid plan)))
        plan0 (get-in plan [:plan/by-plid plid])
        {:keys [plan/type plan/name plan/begin]} plan0
        gp (if (keyword-identical? type :htn-network)
             graph-params-htn graph-params-tpn)]
    (reset! graph plan)
    (reset! graph-params
      (assoc gp
        :plid plid
        :plan-type type
        :network-plid-id begin))
    (initialize-graph)
    (println "TPLAN" plid "type" type "network" begin)
    true))

(defn return-graph [& args]
  (println "TPLAN return-graph")
  @graph)

(defn layout [plan]
  (let [a-little 30
        start (tasks/deferred)
        finish (-> start
                 (sleep a-little)
                 (chain setup-graph)
                 (sleep a-little)
                 (chain add-incoming-outgoing)
                 (sleep a-little)
                 (chain initialize-plan)
                 (sleep a-little)
                 (chain rank)
                 (sleep (* 3 a-little))
                 (chain save-ranking)
                 (sleep a-little)
                 (chain balance)
                 (sleep a-little)
                 (chain mincross)
                 (sleep a-little)
                 (chain position)
                 (sleep a-little)
                 (chain return-graph)
                 (sleep a-little)
                 (tasks/catch
                     #(do (println "Layout failed... ")
                          (return-graph))))]
    (success! start plan)
    finish))
