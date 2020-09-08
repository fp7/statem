;; Copyright (c) 2020 Finn Petersen
;;
;; This program and the accompanying materials are made
;; available under the terms of the Eclipse Public License 2.0
;; which is available at https://www.eclipse.org/legal/epl-2.0/
;;
;; SPDX-License-Identifier: EPL-2.0

(ns io.github.fp7.statem-test
  (:refer-clojure :exclude [get])
  (:require [clojure.core :as core]
            [clojure.test :as test]
            [clojure.set :as set]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :as check]
            [clojure.test.check.properties :as properties]
            [io.github.fp7.statem :as statem])
  (:import [java.util.concurrent.atomic AtomicInteger]))

(defrecord MutableCircularQueue
           [^AtomicInteger inp
            ^AtomicInteger outp
            size
            ^ints buf])

(defn queue
  [size]
  (MutableCircularQueue.
   (AtomicInteger. 0)
   (AtomicInteger. 0)
   size
   (make-array Integer/TYPE size)))

(defn put
  [^MutableCircularQueue q  n]
  (aset  ^ints (.-buf q) (.-inp q) (int n))
  (.set ^AtomicInteger (.-inp q) (rem (inc  (.get ^AtomicInteger (.-inp q))) (.-size q))))

(defn get
  [^MutableCircularQueue q]
  (let [ans (aget ^ints (.-buf q) (.get ^AtomicInteger (.-outp q)))]
    (.set ^AtomicInteger (.-outp q) (rem (inc (.get ^AtomicInteger (.-outp q))) (.-size q)))
    ans))

(defn size
  [^MutableCircularQueue q]
  (rem
   (- (.get ^AtomicInteger (.-inp q))
      (.get ^AtomicInteger (.-outp q)))
   (.-size q)))

;; Tests this Queue

(def init-state
  {:ptr nil
   :contents []})

(def cmds
  {#'queue {::statem/static-pre? (fn [s]
                                   (nil? (core/get s :ptr)))
            ::statem/args (fn [s]
                            (gen/fmap vector
                                      (gen/such-that (fn [i]
                                                       (< 0 i))
                                                     gen/nat)))
            ::statem/next (fn [s r [_ [size]]]
                            (assoc s
                                   :ptr r
                                   :size size))}
   #'put {::statem/static-pre? (fn [s]
                                 (and (not (nil? (core/get s :ptr)))
                                      (< (count (core/get s :contents))
                                         (core/get s :size))))
          ::statem/args (fn [s]
                          (gen/fmap (fn [i]
                                      [(core/get s :ptr) i])
                                    gen/nat))
          ::statem/next (fn [s r [_ [_ v]]]
                          (update s :contents conj v))}
   #'get {::statem/static-pre? (fn [s]
                                 (and (not (nil? (core/get s :ptr)))
                                      (not (empty? (core/get s :contents)))))
          ::statem/args (fn [s]
                          (gen/return [(core/get s :ptr)]))
          ::statem/next (fn [s r c]
                          (update s :contents (comp vec rest)))}
   #'size {::statem/static-pre? (fn [s]
                                  (not (nil? (core/get s :ptr))))
           ::statem/args (fn [s]
                           (gen/return [(core/get s :ptr)]))
           ::statem/post? (fn [s [_ args] r]
                            (= r
                               (count (core/get s :contents))))}})

(def prop-queue-with-no-modification
  (properties/for-all [cmd-seq (statem/commands {::statem/cmds cmds
                                                 ::statem/state init-state})]
                      (statem/run-commands
                       cmd-seq
                       {::statem/cmds cmds
                        ::statem/state init-state})))

(comment
  (gen/generate (statem/commands {::statem/state init-state
                                  ::statem/cmds cmds}) 5))

(comment
  (check/quick-check 100 prop-queue-with-no-modification))

(test/deftest queue-with-no-modification-fails
  (test/testing "The origignal fails with only 3 inputs"
    (let [r (check/quick-check 100 prop-queue-with-no-modification)]
      (test/is (false? (core/get r :result)))
      (test/is (= 3 (count (first (core/get-in r [:shrunk :smallest]))))))))

(defn queue-2
  [size]
  (MutableCircularQueue.
   (AtomicInteger. 0)
   (AtomicInteger. 0)
   (inc size)
   (make-array Integer/TYPE (inc size))))

(def prop-queue-with-incremented-size
  (let [c {::statem/cmds
           (set/rename-keys cmds
                            {#'queue #'queue-2})
           ::statem/state init-state}]
    (properties/for-all [cmd-seq (statem/commands c)]
                        (statem/run-commands cmd-seq
                                             c))))

(test/deftest queue-with-incremented-size-fails
  (test/testing "The queue with internally incremented size fails with 5 inputs"
    (let [r (check/quick-check 100 prop-queue-with-incremented-size)]
      (test/is (false? (core/get r :result)))
      (test/is (= 5 (count (first (core/get-in r [:shrunk :smallest]))))))))

(defn size-2
  [^MutableCircularQueue q]
  (rem
   (Math/abs
    (- (.get ^AtomicInteger (.-inp q))
       (.get ^AtomicInteger (.-outp q))))
   (.-size q)))

(def prop-queue-with-abs-call-in-size
  (let [c {::statem/cmds
           (set/rename-keys cmds
                            {#'queue #'queue-2
                             #'size #'size-2})
           ::statem/state init-state}]
    (properties/for-all [cmd-seq (statem/commands c)]
                        (statem/run-commands cmd-seq
                                             c))))

(test/deftest queue-with-abs-fails
  (test/testing "The queue with abs also fails"
    (let [r (check/quick-check 100 prop-queue-with-abs-call-in-size)]
      (test/is (false? (core/get r :result)))
      (test/is (= 6 (count (first (core/get-in r [:shrunk :smallest]))))))))

(defn size-3
  [^MutableCircularQueue q]
  (rem
   (+
    (- (.get ^AtomicInteger (.-inp q))
       (.get ^AtomicInteger (.-outp q)))
    (.-size q))
   (.-size q)))

(def prop-queue-which-adds-buffer-size-in-size-call
  (let [c {::statem/cmds
           (set/rename-keys cmds
                            {#'queue #'queue-2
                             #'size #'size-3})
           ::statem/state init-state}]
    (properties/for-all [cmd-seq (statem/commands c)]
                        (statem/run-commands cmd-seq
                                             c))))

(test/deftest queue-with-abs-fails
  (test/testing "The queue with abs also fails"
    (let [r (check/quick-check 100 prop-queue-which-adds-buffer-size-in-size-call)]
      (test/is (core/get r :result)))))

(comment
  (check/quick-check 100 prop-queue-with-no-modification))
