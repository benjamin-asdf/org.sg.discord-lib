(ns src.org.sg.discord-lib.utils
  (:require
   [clojure.spec.alpha :as s]
   [clojure.pprint]
   [clojure.core.async :as a]))

(defn
  <throttle
  "Creates a throttled channel that can be read at most once per given ms."
  [<in ms]
  (let [>out (a/chan)]
    (a/go-loop
        [v (a/<! <in)]
        (when
            v
            (a/>! >out v)
            (a/<! (a/timeout ms))
            (recur (a/<! <in))))
    >out))

(defn
  <throttle-by-topic
  ([<in topic-fn]
   (<throttle-by-topic <in topic-fn (a/chan)))
  ([<in topic-fn >out]
   (let [topic->c (atom {})
         ensure-sub (fn
                      [topic]
                      (or
                       (@topic->c topic)
                       (let [c1 (a/chan (a/sliding-buffer 5))
                             c (<throttle c1 2000)]
                         (a/pipe c >out)
                         (get
                          (swap! topic->c assoc topic c1)
                          topic))))]
     (a/go-loop
         [m (a/<! <in)]
       (when-let
           [topic (and m (topic-fn m))]
         (a/>! (ensure-sub topic) m)
         (recur (a/<! <in))))
     >out)))


(defn truncate [s n]
  (subs s 0 (min n (count s))))

(defn
  remove-newlines
  [s]
  (str/replace s #"\s+" " "))

(defmacro if-let*
  ([bindings then]
   `(if-let* ~bindings ~then nil))
  ([bindings then else]
   (if (seq bindings)
     `(if-let [~(first bindings) ~(second bindings)]
        (if-let* ~(drop 2 bindings) ~then ~else)
        ~else)
     then)))

(defmacro when-let*
  [bindings then]
  `(if-let* ~bindings ~then nil))

(defn sorted-map-by-order [rows]
  (into (sorted-map-by
         (fn [key1 key2]
           (compare
            (get-in rows [key1 :order])
            (get-in rows [key2 :order]))))
        rows))


(defn conform! [spec e]
  (let [r (s/conform spec e)]
    (case r
      :clojure.spec.alpha/invalid
      (throw (Exception. (s/explain-str spec e)))
      r)))

;; below https://github.com/Datomic/ion-starter

;; Copyright (c) Cognitect, Inc.
;; All rights reserved.

(def retryable-anomaly?
  "Set of retryable anomalies."
  #{::anomalies/busy
    ::anomalies/unavailable
    ::anomalies/interrupted})

(defn- anom-map
  "Helper for anomaly!"
  [category msg]
  {::anomalies/category (keyword "cognitect.anomalies" (name category))
   ::anomalies/message msg})

(defn anomaly!
  ([name msg]
   (throw (ex-info msg (anom-map name msg))))
  ([name msg cause]
   (throw (ex-info msg (anom-map name msg) cause))))


(defn with-retry
  "Try op, return result if successful, if op throws, check exception against retry? pred,
  retrying if the predicate returns true. Optional backoff function controls retry wait. Backoff
  function should return msec backoff desired or nil to stop retry.
  Defaults to trying 10 times with linear backoff. "
  [op & {:keys [retry? backoff]
         :or {retry? (fn [e]
                       (-> e ex-data ::anomalies/category retryable-anomaly?))
              backoff (fn [epoch]
                        (when (<= epoch 10)
                          (* 200 epoch)))}}]
  (loop [epoch 1]
    (let [[success val] (try [true (op)]
                             (catch Exception e
                               [false e]))]
      (if success
        val
        (if-let [ms (and (retry? val) (backoff epoch))]
          (do
            (Thread/sleep ms)
            (recur (inc epoch)))
          (throw val))))))

;; datomic end

(defn
  with-retry-http
  [op]
  (let [success? (fn [e] (< (:status e) 300))
        backoff (fn
                  [epoch
                   {{:keys [retry-after]} :headers}]
                  (when
                      (<= epoch 10)
                      (if
                          retry-after
                          (* retry-after 1000)
                          (*
                           400
                           epoch
                           (+ 0.7 (* 0.5 (rand)))))))]
    (loop
        [epoch 1]
        (let [e (op)
              success (success? e)]
          (if
              success
              e
              (if-let
                  [ms (backoff epoch e)]
                  (do
                    (Thread/sleep ms)
                    (recur (inc epoch)))
                  (throw e)))))))

;; thanks clojure.core

(defn update-vals
  "m f => {k (f v) ...}

  Given a map m and a function f of 1-argument, returns a new map where the keys of m
  are mapped to result of applying f to the corresponding values of m."
  [m f]
  (with-meta
    (persistent!
     (reduce-kv (fn [acc k v] (assoc! acc k (f v)))
                (if (instance? clojure.lang.IEditableCollection m)
                  (transient m)
                  (transient {}))
                m))
    (meta m)))




(comment)
