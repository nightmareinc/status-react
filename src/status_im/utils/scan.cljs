(ns status-im.utils.scan
  (:require [status-im.ethereum.json-rpc :as rpc]
            [taoensso.timbre :as log]))

(defonce cache (atom {}))
(defonce calls-count (atom 0))
(defonce single-calls-count (atom 0))
(defonce batch-calls-count (atom 0))
(def batch-calls-limit 50)

(defn reset-stats! []
  (reset! cache {})
  (reset! calls-count 0)
  (reset! single-calls-count 0)
  (reset! batch-calls-count 0))

(defn memoized-rpc
  [{:keys [method params]}]
  (js/Promise.
   (fn [resolve reject]
     (swap! calls-count inc)
     (let [result (get-in @cache [method params])]
       (if result
         (apply resolve result)
         (do
           (swap! single-calls-count inc)
           (rpc/call
            {:method     method
             :params     params
             :on-success (fn [& args]
                           (swap! cache assoc-in [method params] args)
                           (apply resolve args))
             :on-error   reject})))))))

(defn get-balance
  [address block-number]
  (let [block-number-hex (str "0x" (.toString block-number 16))]
    (swap! calls-count inc)
    (let [balance
          (first (get-in @cache ["eth_getBalance" [address block-number-hex]]))]
      (when (nil? balance)
        (log/debug "nil balance" block-number))
      balance)))

(defn get-transactions-count
  [address block-number]
  (let [block-number-hex (str "0x" (.toString block-number 16))]
    (memoized-rpc
     {:method "eth_getTransactionCount"
      :params [address block-number-hex]})))

(defn split-range [from to]
  (cond
    (zero? (- to from))
    []

    (= 1 (- to from))
    [[from from]]

    :else
    (let [mid (js/Math.floor (/ (+ from to) 2))]
      [[from mid] [mid to]])))

(defn check-range [address from to]
  (js/Promise.
   (fn [resolve reject]
     (let [to-balance   (get-balance address to)
           from-balance (get-balance address from)]
       (cond
         (not (.eq to-balance from-balance))
         (resolve (split-range from to))

         (not (.isZero to-balance))
         (resolve [])

         :else
         (.. (js/Promise.all
              #js [(get-transactions-count address to)
                   (get-transactions-count address from)])
             (then
              (fn [res]
                (let [to-count   (aget res 0)
                      from-count (aget res 1)]
                  (if-not (.eq to-count from-count)
                    (resolve (split-range from to))
                    (resolve [])))))))))))

(defn check-ranges [address ranges callback]
  (log/debug "Check ranges:"
             "count" (count ranges)
             "range" (let [[f t] (first ranges)] (- t f)))
  (..
   (js/Promise.all
      (reduce
       (fn [a [from to]]
         (.push a (check-range address from to))
         a)
       #js []
       ranges))
   (then (fn [res-js]
           (callback (apply concat (js->clj res-js)))))))

(defn warm-up-balances [address blocks]
  (let [method "eth_getBalance"
        requests
        (keep
          (fn [block]
            (let [params [address (str "0x" (.toString block 16))]]
              (when-not (get-in @cache [method params])
                {:method method
                 :params params})))
          blocks)]
    (log/debug "Warmup:"
               "all requests" (count blocks)
               "new requests" (count requests)
               "number of batches" (js/Math.ceil (/ (count requests)
                                                    batch-calls-limit)))
    (js/Promise.all
     (clj->js
      (mapv
       (fn [requests]
         (js/Promise.
          (fn [resolve reject]
            (let [requests (vec requests)]
              (swap! batch-calls-count inc)
              (rpc/call-batch
               requests
               (fn [results]
                 (let [requests-count (count requests)
                       results-count (count results)]
                   (when (not= requests-count results-count)
                     (log/debug "requests-count" (count requests)
                                "results-count" (count results)))
                   (doseq [idx (range requests-count)]
                     (let [{:keys [params method]}
                           (get requests idx)

                           {:keys [result error]}
                           (get results idx)]
                       (when error
                         (log/debug "ERROR" params error))
                       (swap! cache assoc-in [method params] [result]))))
                 (resolve results)))))))
       (partition-all batch-calls-limit requests))))))

(defn get-blocks-with-transactions
  [address from to callback]
  (let [blocks (atom (sorted-set))
        ranges-callback
        (fn ranges-callback [next-ranges]
          (let [result      (group-by
                             (fn [[from to]]
                               (zero? (- to from)))
                             next-ranges)
                new-blocks  (map second (get result true))
                next-ranges (get result false)]
            (swap! blocks (fn [bl]
                            (reduce (fn [a bl]
                                      (conj a bl))
                                    bl
                                    new-blocks)))
            (if (seq next-ranges)
              (.. (warm-up-balances address (set (flatten next-ranges)))
                  (then
                   (fn [_]
                     (log/debug "Warmup ended")
                     (check-ranges address next-ranges ranges-callback))))
              (callback @blocks))))]
    (.. (warm-up-balances address #{from to})
        (then
         (fn [_] 
           (check-ranges
            address
            [[from to]]
            ranges-callback))))))

(defn get-blocks-stats
  [address from to]
  (reset-stats!)
  (let [start (js/Date.now)]
    (get-blocks-with-transactions
     address
     from
     to
     (fn [blocks]
       (log/debug
        "Get blocks res: "
        (let [stop (js/Date.now)]
          {:total-rpc-requests @calls-count
           :cached-entries     (reduce
                                (fn [a [k v]]
                                  (+ a (count v)))
                                0
                                @cache)
           :blocks-count       (count blocks)
           :single-calls-count @single-calls-count
           :batch-calls-count  @batch-calls-count
           :duration           (- stop start)
           :blocks             blocks})))))
  nil)

(comment
  (let [from    0
        to      9025828
        ;; mainnet acc
        address "0x2f88d65f3cB52605a54A833ae118fb1363aCccd2"]
    (get-blocks-stats
     address
     from
     to))

  ;; Get blocks res (batch-calls-limit = 1):
  ;; {:total-rpc-requests 5446
  ;;  :cached-entries     1369
  ;;  :blocks-count       131
  ;;  :single-calls-count 11
  ;;  :batch-calls-count  1358}
  ;; 50 => 157853 
  ;; 25 => 95751 
  ;; 10 => 49344 
  ;; 5  => 30286 
  ;; 3  => 23877 
  ;; 2  => 20429 
  ;; 1  => 18062

  (let [from    6672198
        to      6972198
        ;; ropsten acc
        address "0xf184747445c3B85CEb147DfB136067CB93d95F1D"]
    (get-blocks-stats
     address
     from
     to))

  ;; Get blocks res (batch-calls-limit = 1):
  ;; {:total-rpc-requests 13030
  ;;  :cached-entries     3259
  ;;  :blocks-count       512
  ;;  :single-calls-count 0
  ;;  :batch-calls-count  3259}
  ;; 50 => 149496
  ;; 25 => 84173
  ;; 10 => 47975
  ;; 5  => 29129
  ;; 3  => 23746
  ;; 2  => 22804, 23686
  ;; 1  => 26297, 24552
)
