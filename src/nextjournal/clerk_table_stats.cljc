;; # 📊 Clerk Table Stats
^{:nextjournal.clerk/visibility :hide-ns}
(ns nextjournal.clerk-table-stats
  (:require #?(:clj [nextjournal.clerk :as clerk]
               :cljs [nextjournal.clerk :as-alias clerk])
            [nextjournal.clerk.viewer :as viewer]
            [clojure.set :as set]
            [clojure.string :as str]))

(clerk/require-cljs 'nextjournal.clerk-table-stats-sci)

(comment
  (nextjournal.clerk.cljs-libs/clear-cljs!)
  @#'nextjournal.clerk.cljs-libs/cljs-graph
  (clojure.java.io/resource "nextjournal/clerk_table_stats_sci.cljs")
  )

(defn deep-merge
  ([])
  ([a] a)
  ([a b]
   (cond (when-let [m (meta b)]
           (:replace m)) b
         (and (map? a) (map? b)) (merge-with deep-merge a b)
         :else b))
  ([a b & more]
   (apply merge-with deep-merge a b more)))

(comment
  (deep-merge {:a {:b {:c 3}}} {:a {:b {:c 4 :d 5}}})
  (deep-merge {:a {:b {:c 4 :d 5}}} {:a {:b {:c 3}}})
  (deep-merge {:a {:pred inc}} {:a {:pred {:dude 1}}})
  (deep-merge {:a {:pred {:dude 1}}} {:a {:pred inc}})
  )

(defn paths->head [paths]
  (->> paths
       (partition-by first)
       (map (fn [paths] (if (< 1 (count paths))
                              [(ffirst paths) (mapv (comp first rest) paths)]
                              (ffirst paths))))))

(defn head->paths [head]
  (mapcat (fn [k]
            (if (vector? k)
              (mapv #(vector (first k) %) (last k))
              [[k]])) head))

(defn ->ordered-paths [order paths]
  (->> paths
       (sort-by #(let [i (.indexOf order %)]
                   (if (< i 0) (count paths) i)))
       vec))

(defn ->visible-paths [hidden paths]
  (filterv (complement hidden) paths))

(comment
  (->visible-paths #{[:a] [:b]} [[:a] [:c]])
  (->visible-paths (complement #{[:a] [:b]}) [[:a] [:c]])
  )

(comment
  (let [group-headers false]
    (->> [{:pred {}} {:pred {:dude 1}}]
         (take 1000)
         (apply deep-merge)
         (mapcat (fn [[k v]]
                   (if (and (map? v) group-headers)
                     (map #(if (or (true? group-headers) (contains? (set group-headers) k))
                             (vector k %)
                             [k])
                          (keys v))
                     [[k]])))
         (remove nil?)
         distinct))
  )

(defn normalize-seq-of-map
  ([s] (normalize-seq-of-map {} s))
  ([{:keys [column-order computed-columns hide-columns select-columns group-headers schema]} s]
   (let [paths (if schema
                 (head->paths schema)
                 (->> s
                      (take 1000)
                      (apply deep-merge)
                      (mapcat (fn [[k v]]
                                (if (and (map? v) group-headers)
                                  (map #(if (or (true? group-headers) (contains? (set group-headers) k))
                                          (vector k %)
                                          [k])
                                       (keys v))
                                  [[k]])))
                      (remove nil?)
                      distinct))
         computed-paths (set (map vector (keys computed-columns)))
         ordered-paths (cond->> paths
                         (and (not schema) (seq computed-columns)) (concat computed-paths)
                         (seq column-order) (->ordered-paths (head->paths column-order)))
         hidden-paths (if (seq hide-columns) (set (head->paths hide-columns)) #{})
         selected-paths (when (seq select-columns)
                          (head->paths select-columns))
         visible-paths (cond->> ordered-paths
                         (seq hidden-paths) (->visible-paths hidden-paths)
                         (seq selected-paths) (->visible-paths (complement (set selected-paths))))]
     {:schema (paths->head ordered-paths)
      :paths ordered-paths
      :hidden-paths hidden-paths
      :head (paths->head visible-paths)
      :visible-paths visible-paths
      :rows (map (fn [m] (map (fn [path]
                                (if (contains? computed-paths path)
                                  ((get-in computed-columns path) m)
                                  (get-in m path viewer/missing-pred))) visible-paths)) s)})))

(defn normalize-map-of-seq
  ([s] (normalize-map-of-seq {} s))
  ([{:keys [column-order hide-columns group-headers schema]} m]
   (let [paths (if schema
                 (head->paths schema)
                 (->> m
                      (mapcat (fn [[k s]]
                                (if (and group-headers (map? (first s)))
                                  (let [mm (apply deep-merge s)]
                                    (map #(if (or (true? group-headers) (contains? (set group-headers) k))
                                            (vector k %)
                                            [k])
                                         (keys mm)))
                                  [[k]])))
                      distinct))
         ordered-paths (cond->> paths
                         (seq column-order) (->ordered-paths (head->paths column-order)))
         hidden-paths (if (seq hide-columns) (set (head->paths hide-columns)) #{})
         visible-paths (cond->> ordered-paths
                         (seq hidden-paths) (->visible-paths hidden-paths))]
     {:schema (paths->head ordered-paths)
      :paths ordered-paths
      :hidden-paths hidden-paths
      :head (paths->head visible-paths)
      :rows (map-indexed (fn [i _] (map (fn [[k1 k2]]
                                          (let [v (nth (get m k1) i viewer/missing-pred)]
                                            (if k2
                                              (get v k2)
                                              v)))
                                        visible-paths))
                         (val (apply max-key (comp viewer/count-bounded val) m)))})))

(defn use-headers [s]
  (let [{:as table :keys [rows]} (viewer/normalize-seq-of-seq s)]
    (-> table
        (assoc :head (first rows))
        (update :rows rest))))

(defn classified-type [x]
  (cond
    (string? x) 'string
    (number? x) 'number
    (boolean? x) 'boolean
    (keyword? x) 'keyword
    (symbol? x) 'symbol
    :else (type x)))

(comment
  (map classified-type ["a" "b" "a" "a" nil "" "c" "d" "d" :foo 'bar 1 1/3 1.2]))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn sturges [values]
  (-> (count values) log2 int inc))

(defn fixed-size-bins [min-value max-value bin-count]
  (let [size (/ (- max-value min-value) bin-count)]
    (vec
     (for [step (range (inc bin-count))]
       (+ min-value (* size step))))))

(defn binary-search [v target]
  (loop [low 0
         high (dec (count v))]
    (if (> low high)
      (- (inc low))
      (let [mid (quot (+ low high) 2)
            mid-val (v mid)]
        (cond (< mid-val target) (recur (inc mid) high)
              (< target mid-val) (recur low (dec mid))
              :else mid)))))

(defn histogram [xs]
  (let [xs-count (count xs)
        min-value (apply min xs)
        max-value (apply max xs)
        ;; Calculate number of bins assuming ~normal dist using Sturges’ Rule
        ;; TODO: Investigate Freedman–Diaconis
        thresholds (fixed-size-bins min-value max-value (sturges xs))
        groups (->> xs
                    (filter #(and (>= % min-value) (<= % max-value)))
                    ;; Remove trailing threshold number
                    (group-by #(->> % (binary-search (subvec thresholds 0 (dec (count thresholds)))) inc Math/abs dec)))]
    (map-indexed
     (fn [i [group-min group-max]]
       (let [group (or (groups i) [])
             group-count (count group)]
         {:range [group-min group-max]
          :count group-count
          :percentage (/ group-count xs-count)}))
     (partition 2 1 thresholds))))

(comment
  (histogram [1 1 1 5 2 2 0 0 1 40 51 21])
  (histogram [1]))

(defn categories [xs]
  (let [row-count (count xs)
        category (fn [label count] {:label label :count count :percentage (/ count row-count)})
        {:keys [rest unique empty]}
        (->> xs
             frequencies
             (reduce (fn [acc [label count]]
                       (cond
                         (nil? label) (update acc :empty (fnil inc 0))
                         (< 1 count) (update acc :rest #(conj % (category label count)))
                         :else (update acc :unique (fnil inc 0))))
                     {:rest []}))]

    (concat rest
            (when unique [(category :unique unique)])
            (when empty [(category :empty empty)]))))

(comment
  (categories ["a" "b" "a" "a" nil "" "c" "d" "d"]))

(defn compute-col-summary [xs]
  (let [continuous? (every? number? xs)
        dist (if continuous? (histogram xs) (categories xs))]
    (cond->
        {:continuous? continuous?
         :col-type (->> xs (remove nil?) (map classified-type) distinct (str/join ", "))
         :distribution dist}
      (not continuous?) (assoc :category-count (count dist)))))

(defn transpose [rows]
  (apply mapv vector rows))

(defn compute-table-summary [{:as data :keys [rows visible-paths]} {:keys [pre-process-stats]}]
  (cond-> data
    visible-paths (assoc :summary
                         (let [cols (transpose rows)]
                           (->> (map (fn [path i]
                                       (when-let [col (nth cols i #_nil)]
                                         (let [col (if-let [f (get-in pre-process-stats path)]
                                                     (map f col)
                                                     col)]
                                           [path (compute-col-summary col)])))
                                     visible-paths
                                     (range))
                                (reduce (fn [acc [k v]]
                                          (assoc-in acc k v))
                                        {})
                                )))))

(comment
  (compute-col-summary ["a" "b" "a" "a" nil "" "c" "d" "d" :foo])
  (compute-col-summary [1 1 1 5 2 2 0 0 1 40 51 21])
  (def grouped (normalize-seq-of-map
                {:group-headers true}
                [{:category {:category/a :foo :category/b :foo}}
                 {:category {:category/a :foo :category/b :bar}}
                 {:category {:category/a :bar :category/b :foo}}]))
  )

(defn normalize-table-data
  ([data] (normalize-table-data {} data))
  ([{:as opts
     filter-spec :filter
     :keys [stats]
     :or {stats true}} data]
   (cond-> (cond
             (and (map? data) (-> data (viewer/get-safe :rows) sequential?)) (viewer/normalize-seq-to-vec data)
             (and (map? data) (sequential? (first (vals data)))) (normalize-map-of-seq opts data)
             (and (sequential? data) (map? (first data))) (normalize-seq-of-map opts data)
             (and (sequential? data) (sequential? (first data))) (viewer/normalize-seq-of-seq data)
             :else nil)
     stats (compute-table-summary opts)
     true (update :rows (partial filter (fn [row]
                                          (let [ks (keys filter-spec)]
                                            (or (empty? ks)
                                                (let [filters (map #(get filter-spec %) ks)
                                                      values (map #(viewer/->value (nth row %)) ks)]
                                                  (every? true?
                                                          (map (fn [col-filter col-value]
                                                                 (or (empty? col-filter)
                                                                     (if
                                                                         ;; histogram
                                                                         (:range (first col-filter))
                                                                       (some #(let [[from to] (:range %)]
                                                                                (<= from col-value to))
                                                                             col-filter)
                                                                       (contains? col-filter col-value))))
                                                               filters values)))))))))))
(def table-markup-viewer
  {:render-fn '(fn [head+body {:as opts :keys [sync-var]}]
                 (reagent.core/with-let [table-state (if sync-var
                                                       (deref sync-var)
                                                       #?(:clj (throw (js/Error. (str "no sync var: " sync-var)))
                                                          :cljs nil))]
                   [:div.bg-white.rounded-lg.border.border-slate-300.shadow-sm.font-sans.text-sm.not-prose.overflow-x-auto
                    {:class "print:overflow-none print:text-[10px] print:shadow-none print:rounded-none print:border-none"}
                    (into
                     [:table.w-full]
                     (nextjournal.clerk.render/inspect-children (assoc opts :table-state table-state))
                     ;; debug atom+head+body #_
                     head+body)]))})

(def table-head-viewer
  {:render-fn 'nextjournal.clerk-table-stats-sci/table-head-viewer})

(def table-body-viewer
  {:render-fn '(fn [rows opts] (into [:tbody] (map-indexed (fn [idx row] (nextjournal.clerk.render/inspect-presented (update opts :path conj idx) row))) rows))})

(def table-row-viewer
  {:render-fn 'nextjournal.clerk-table-stats-sci/table-row-viewer})

(defn tabular? [xs]
  (and (seqable? xs)
       (sequential? xs)
       (let [sample (take 100 xs)]
         ;; TODO: do we also need some normalization to be successful?
         ;; normalize-seq-of-map currently always returns truthy on a seq of maps
         (every? map? sample))))

{::clerk/render-opts {:group-headers true}
 ::clerk/page-size 5
 ::clerk/width :full}

(def view-as-table-viewer
  ;; TODO: decide if we want to opt out of matching only top-level forms
  {:pred {:wrapped (every-pred (comp #{1} count (viewer/get-safe :path))
                               (comp tabular? viewer/->value))}
   :transform-fn (partial viewer/with-viewer 'nextjournal.clerk.viewer/table-viewer)})

;; usage with default global options
(comment
  (update view-as-table-viewer
          :transform-fn
          (fn [f]
            (fn [wv]
              (f {::clerk/render-opts {:group-headers true}
                  ::clerk/page-size 5
                  ::clerk/width :full} wv)))))

(def viewer
  (assoc viewer/table-viewer
         :transform-fn
         (fn transform-fn [{:as wrapped-value :nextjournal/keys [applied-viewer render-opts]}]
           (let [#?@(:clj [id (:id wrapped-value)
                           var-name (symbol (namespace id) (str (name id) "-table"))])
                 _ #?(:clj (when-not (resolve var-name)
                            (when-some [ns' (find-ns (symbol (namespace var-name)))]
                              (intern ns' (symbol (name var-name)) (doto (atom {:filter {}})
                                                                     (add-watch :foo (fn [_k _r _o _n] (nextjournal.clerk/recompute!)))))))
                      :cljs nil)
                 table-state #?(:clj @@(resolve var-name)
                                :cljs nil)]

             (if-let [{:keys [head rows summary state]} (normalize-table-data (merge render-opts table-state)
                                                                              (viewer/->value wrapped-value))]
               (-> wrapped-value
                   (assoc :nextjournal/viewer table-markup-viewer)
                   (update :nextjournal/width #(or % :wide))
                   (update :nextjournal/render-opts merge {:num-cols (count (or head (first rows)))
                                                           #?@(:clj [:sync-var (viewer/->viewer-eval
                                                                                (list 'nextjournal.clerk.render/intern-atom!
                                                                                    (list 'quote var-name) {:filter {} :init 2}))])
                                                           :number-col? (into #{}
                                                                              (comp (map-indexed vector)
                                                                                    (keep #(when (number? (second %)) (first %))))
                                                                              (not-empty (first rows)))
                                                           :summary summary
                                                           :state state
                                                           })
                   (update :nextjournal/render-opts dissoc :computed-columns :pre-process-stats)
                   (assoc :nextjournal/value (cond->> []
                                               (seq rows) (cons (viewer/with-viewer table-body-viewer (merge (-> applied-viewer
                                                                                                                 (select-keys [:page-size])
                                                                                                                 (set/rename-keys {:page-size :nextjournal/page-size}))
                                                                                                             (select-keys wrapped-value [:nextjournal/page-size]))
                                                                  (map (partial viewer/with-viewer table-row-viewer) rows)))
                                               head (cons (viewer/with-viewer (:name table-head-viewer table-head-viewer) head)))))
               (-> wrapped-value
                   viewer/mark-presented
                   (assoc :nextjournal/width :wide)
                   (assoc :nextjournal/value [(viewer/present wrapped-value)])
                   (assoc :nextjournal/viewer {:render-fn 'nextjournal.clerk.render/render-table-error})))))))

#_
(viewer/reset-viewers! :default
                       (viewer/add-viewers (viewer/get-default-viewers)
                                           [viewer]))

{::clerk/visibility {:code :hide :result :show}}

#?(:clj
   (comment
     ;; ## Seq of map

     (def seq-of-map
       [{:ars/id "1"
         :compound/name "Krefeld"
         :ductile/id #uuid "1174774f-17ec-442c-803f-2906015be68f"
         :entry/datetime #inst "2023-09-28T06:33:01Z"}
        {:ars/id "2"
         :compound/name "Krefeld"
         :ductile/id #uuid "774f1174-7ec1-2c44-3f80-15be68f29060"}])

     (clerk/example
       (normalize-seq-of-map seq-of-map)
       (normalize-seq-of-map {:column-order [:compound/name :entry/datetime]
                              :hide-columns [:ars/id :ductile/id]} seq-of-map)
       (normalize-seq-of-map {:column-order [:compound/abbreviation :compound/name :entry/datetime]
                              :hide-columns [:ars/id :ductile/id]
                              :computed-columns {:compound/abbreviation (fn [m]
                                                                          (str/upper-case (str/join (take 3 (:compound/name m)))))}} seq-of-map))

     (clerk/table seq-of-map)
     (clerk/table {::clerk/render-opts {:column-order [:compound/name :entry/datetime]}} seq-of-map)
     (clerk/table {::clerk/render-opts {:hide-columns [:ars/id :ductile/id]}} seq-of-map)
     #_(clerk/table {::clerk/render-opts {:column-order [#_:compound/abbreviation :compound/name :entry/datetime]
                                          :hide-columns [:ars/id :ductile/id]
                                          :computed-columns {:compound/abbreviation (fn [m]
                                                                                      (str/upper-case (str/join (take 3 (:compound/name m)))))}}} seq-of-map)))

;; ## Nested seq of map

(def nested-seq-of-map
  [{:ars/id "1"
    :compound/name "Krefeld"
    :ductile/id #uuid "1174774f-17ec-442c-803f-2906015be68f"
    :entry/datetime #inst "2023-09-28T06:33:01Z"
    :entry/transport {:transport/mode :mode/truck
                      :transport/name "Kempers"}
    :exit/transport {:transport/mode :mode/truck
                     :transport/name "Kempers"}}
   {:ars/id "2"
    :compound/name "Krefeld"
    :ductile/id #uuid "774f1174-7ec1-2c44-3f80-15be68f29060"
    :entry/transport {:transport/mode :mode/truck
                      :transport/name "Kempers"}
    :exit/transport {:transport/mode :mode/truck}}])

#?(:clj
   (clerk/example
     #_(normalize-seq-of-map nested-seq-of-map)
     (normalize-seq-of-map {:group-headers true} nested-seq-of-map)
     (normalize-seq-of-map {:group-headers [:entry/transport]} nested-seq-of-map)
     (normalize-seq-of-map {:group-headers true
                            :column-order [:compound/name
                                           [:entry/transport [:transport/name :transport/mode]]
                                           [:exit/transport [:transport/name :transport/mode]]
                                           :entry/datetime]
                            :hide-columns [:ars/id :ductile/id]} nested-seq-of-map)

     (clerk/table {::clerk/render-opts {:group-headers true}} nested-seq-of-map)
     (clerk/table {::clerk/render-opts {:group-headers [:entry/transport]}} nested-seq-of-map)
     (clerk/table {::clerk/render-opts {:group-headers true
                                        :column-order [:compound/name
                                                       [:entry/transport [:transport/name :transport/mode]]
                                                       [:exit/transport [:transport/name :transport/mode]]
                                                       :entry/datetime]
                                        :hide-columns [:ars/id :ductile/id]}} nested-seq-of-map)
))


