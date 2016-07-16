;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns planviz.transit
  (:require [cognitect.transit :as transit])
  (:import [java.io
            ByteArrayInputStream ByteArrayOutputStream]))

;; x is a Clojure value, returns a JSON string
(defn transit-to-json [x]
  (let [out (ByteArrayOutputStream. 65536)
        writer (transit/writer out :json)]
    (transit/write writer x)
    (.toString out)))

;; j is a json string, returns a Clojure value
(defn transit-from-json [j]
  (let [in (ByteArrayInputStream. (.getBytes j))
        reader (transit/reader in :json)
        x (transit/read reader)]
    x))
