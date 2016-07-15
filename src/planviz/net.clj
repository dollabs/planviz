;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns planviz.net
  (:require [clojure.string :as string])
  (:import [java.net
            DatagramSocket ServerSocket SocketException]
           [java.io
            IOException]))

(defn port-available?
  "Returns true if the port is available to be bound (for both UDP and TCP)"
  [port]
  (let [state (atom {:available? false :ss nil :ds nil})]
    (if (and (number? port) (> port 0) (<= port 65535))
      (try
        (swap! state assoc :ss (ServerSocket. port))
        (.setReuseAddress (:ss @state) true)
        (swap! state assoc :ds (DatagramSocket. port))
        (.setReuseAddress (:ds @state) true)
        (swap! state assoc :available? true)
        (catch IOException e
          false)
        (catch SocketException e
          false)
        (finally
          (if-let [ds (:ds @state)]
            (.close ds))
          (if-let [ss (:ss @state)]
            (.close ss)))))
    (:available? @state)))
