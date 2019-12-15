(ns status-im.utils.scan
  (:require [status-im.ethereum.json-rpc :as rpc]
            [taoensso.timbre :as log]))

(defonce cache (atom {}))
(defonce calls-count (atom 0))

(defn memoized-rpc
  [{:keys [method params]}]
  (js/Promise.
   (fn [resolve reject]
     (swap! calls-count inc)
     (if-let [result (get-in @cache [method params])]
       (apply resolve result)
       (rpc/call
        {:method     method
         :params     params
         :on-success (fn [& args]
                       (swap! cache assoc-in [method params] args)
                       (apply resolve args))
         :on-error   reject})))))

(defn get-balance
  [address block-number]
  (let [block-number-hex (str "0x" (.toString block-number 16))]
    (memoized-rpc
     {:method "eth_getBalance"
      :params [address block-number-hex]})))

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
  (let [to-balance-promise   (get-balance address to)
        from-balance-promise (get-balance address from)]
    (js/Promise.
     (fn [resolve reject]
       (.. (js/Promise.all
            #js [to-balance-promise
                 from-balance-promise])
           (then
            (fn [res]
              (let [to-balance   (aget res 0)
                    from-balance (aget res 1)]
                (cond
                  (not (.eq to-balance from-balance))
                  (resolve (split-range from to))

                  (not (.isZero to-balance))
                  (resolve [])

                  :else
                  (let [to-count-promise   (get-transactions-count address to)
                        from-count-promise (get-transactions-count address from)]
                    (.. (js/Promise.all
                         #js [to-count-promise
                              from-count-promise])
                        (then
                         (fn [res]
                           (let [to-count   (aget res 0)
                                 from-count (aget res 1)]
                             (if-not (.eq to-count from-count)
                               (resolve (split-range from to))
                               (resolve []))))))))))))))))

(defn check-ranges [address ranges callback]
  (log/debug "Check ranges" ranges)
  (.. (js/Promise.all
       (clj->js
        (map (fn [[from to]]
               (check-range address from to))
             ranges)))
      (then (fn [res-js]
              (callback (apply concat (js->clj res-js)))))))

(defn get-blocks-with-transactions
  [address from to callback]
  (let [blocks (atom (sorted-set))
        ranges-callback
        (fn ranges-callback [next-ranges]
          (let [result (group-by
                        (fn [[from to]]
                          (zero? (- to from)))
                        next-ranges)
                new-blocks (map second (get result true))
                next-ranges (get result false)]
            (swap! blocks (fn [bl]
                            (reduce (fn [a bl]
                                      (conj a bl))
                                    bl
                                    new-blocks)))
            (if (seq next-ranges)
              (check-ranges address next-ranges ranges-callback)
              (callback @blocks))))]
    (check-ranges
     address
     [[from to]]
     ranges-callback)))

(defn get-blocks-stats
  [address from to]
  (reset! calls-count 0)
  (reset! cache {})

  (get-blocks-with-transactions
   address
   from
   to
   (fn [blocks]
     (log/debug
      "Get blocks res: "
      {:total-rpc-requests @calls-count
       :cached-entries     (reduce
                            (fn [a [k v]]
                              (+ a (count v)))
                            0
                            @cache)
       :blocks-count       (count blocks)
       :blocks             blocks})))
  nil)

(comment
  (let [from    0
        to      9025828
        address "0x2f88d65f3cB52605a54A833ae118fb1363aCccd2"]
    (get-blocks-stats
     address
     from
     to))
  ;; Get blocks res:
  ;; {:total-rpc-requests 5446
  ;;  :cached-entries     1369
  ;;  :blocks-count       131}
)
