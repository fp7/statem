;; Copyright (c) 2020 Finn Petersen
;;
;; This program and the accompanying materials are made
;; available under the terms of the Eclipse Public License 2.0
;; which is available at https://www.eclipse.org/legal/epl-2.0/
;;
;; SPDX-License-Identifier: EPL-2.0

(ns io.github.fp7.statem
  (:refer-clojure :exclude [next])
  (:require [clojure.core :as core]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.random :as random]
            [clojure.test.check.results :as results]
            [clojure.test.check.properties :as properties]
            [clojure.walk :as walk]))


(defrecord SymbolicVar [var])

(defn symbolic
  [var]
  (->SymbolicVar var))


(defn symbolic->dynamic
  [mapping data]
  (walk/postwalk
   (fn [d]
     (if (and (vector? d)
              (= (count d) 3)
              (= ::call (first d)))
       (let [[_ f args] d]
         (apply f args))
       (get mapping d d)))
   data))


(defn command
  [state {::keys [cmds]}]
  (let [available-cmds (into []
                             (comp
                              (filter (fn [[cmd {::keys [static-pre?]
                                                 :or {static-pre? (constantly true)}}]]
                                        (static-pre? state)))
                              (map (fn [[cmd {::keys [args]}]]
                                     (gen/fmap (fn [gen-args]
                                                 [cmd gen-args])
                                               (args state)))))
                             cmds)]
    (gen/such-that
     (fn [[cmd args]]
       (let [dynamic-pre? (get-in cmds [cmd ::dynamic-pre?] (constantly true))]
         (dynamic-pre? state [cmd args])))
     (gen/one-of available-cmds))))

(defn valid-sequence?
  [state {::keys [cmds]} cmd-seq]
  (when (seq cmd-seq)
    (reduce (fn state-reducer [s [_ r [_ cmd args]]]
              (let [static-pre? (get-in cmds [cmd ::static-pre?] (constantly true))
                    dynamic-pre? (get-in cmds [cmd ::dynamic-pre?] (constantly true))
                    next (get-in cmds [cmd ::next] (fn [s _ _] s))]
                (if (and (static-pre? s)
                         (dynamic-pre? s [cmd args]))
                  (next s r [cmd args])
                  (reduced false))))
            state
            cmd-seq)))

(defn commands
  [{::keys [cmds
            state]
    :as opts}]
  (gen/->Generator
   (fn [rnd size]
     (let [cmd-seq (fn cmd-seq [state cnt r]
                     (lazy-seq
                      (when (< cnt size)
                        (let [[r1 r2] (random/split r)
                              [[cmd] args :as cmd-rose] (gen/call-gen (command state opts) r1 size)
                              next (get-in cmds [cmd ::next] (fn [s _ _] s))]
                          (cons (rose/fmap (fn [cmd-with-args]
                                             [::set (symbolic cnt) (into [] (cons ::call cmd-with-args))])
                                           cmd-rose)
                                (cmd-seq (next state (symbolic cnt) (rose/root cmd-rose))
                                         (inc cnt)
                                         r2))))))]
       (let [data (into []
                        (cmd-seq state 0 rnd))]
         (->> data
              (rose/shrink-vector vector )
              (rose/collapse)
              (rose/filter (partial valid-sequence? state opts))))))))


(defn run
  [mappings call]
  (symbolic->dynamic mappings call))


(defn run-commands
  [cmd-seq {::keys [cmds state]}]
  (let [result
        (reduce (fn [{::keys [mappings state] :as env} [op res [call-kw fn-sym args :as call]]]
                  (case op
                    ::set
                    (let [dyn-res (run mappings call)
                          post (get-in cmds [fn-sym ::post?] (constantly true))
                          next (get-in cmds [fn-sym ::next] (fn [s _ _] s))
                          new-mappings (assoc mappings res dyn-res)
                          replaced-args (run new-mappings args)]
                      (if (run new-mappings (post state [fn-sym replaced-args] dyn-res))
                        (let [next-state (run new-mappings (next state dyn-res [fn-sym replaced-args]))]
                          (assoc env
                                 ::mappings new-mappings
                                 ::state next-state))
                        (reduced (assoc env
                                        ::failed? true
                                        ::call call))))))
                {::state state
                 ::mappings {}
                 ::failed? false}
                cmd-seq)]
    (reify results/Result
      (pass? [_] (not(get result ::failed?)))
      (result-data [_] result))))
