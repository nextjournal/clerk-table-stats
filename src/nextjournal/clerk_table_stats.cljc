;; # 📊 Clerk Table Stats
^{:nextjournal.clerk/visibility :hide-ns}
(ns nextjournal.clerk-table-stats
  (:refer-clojure :exclude [find])
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [nextjournal.clerk #?(:clj :as :cljs :as-alias) clerk]
            [nextjournal.clerk-table-stats.render :as-alias render]
            [nextjournal.clerk.viewer :as viewer]))

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
  (deep-merge {:a {:pred {:dude 1}}} {:a {:pred inc}}))

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
  (->visible-paths (complement #{[:a] [:b]}) [[:a] [:c]]))

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
         distinct)))

(defn normalize-seq-of-map
  ([s] (normalize-seq-of-map {} s))
  ([{:keys [column-order transform-columns computed-columns hide-columns select-columns group-headers schema]} s]
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
      :rows (cond->> s
              (seq computed-columns) (map (fn [m]
                                            (into m (map (fn [[k compute-fn]]
                                                           [k (compute-fn m)]) computed-columns))))
              true (map (fn [m]
                          (with-meta
                            (map (fn [path]
                                   (if-let [transform-fn (get transform-columns path)]
                                     (transform-fn m)
                                     (get-in m path viewer/missing-pred))) visible-paths)
                            {:data m}))))})))

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
                                        {}))))))

(defn compute-filters-data [{:as data :keys [rows visible-paths]}
                            {:as opts :keys [active-filters]}]
  (reduce-kv
   (fn [data idx filter]
     (let [filter-type (first (keys filter))]
       (case filter-type
         :one-of
         (let [values (into #{} (map #(nth % idx)) rows)]
           (update data :filter-data assoc idx {:values values}))
         #_else
         data)))
   data active-filters))

(defn compute-autocomplete-data [{:as data :keys [rows visible-paths]} opts]
  (reduce
   (fn [data idx]
     (let [values (into #{} (map #(nth % idx)) rows)]
       (update data :autocomplete-data assoc idx {:values values})))
   data (range 0 (count visible-paths))))

(comment
  (compute-col-summary ["a" "b" "a" "a" nil "" "c" "d" "d" :foo])
  (compute-col-summary [1 1 1 5 2 2 0 0 1 40 51 21])
  (def grouped (normalize-seq-of-map
                {:group-headers true}
                [{:category {:category/a :foo :category/b :foo}}
                 {:category {:category/a :foo :category/b :bar}}
                 {:category {:category/a :bar :category/b :foo}}])))

(defmulti col-filter-fn
  (fn [[filter-type & _]]
    filter-type))

(defmethod col-filter-fn :substring [[_ s]]
  (when (and (not (nil? s)) (not= "" s))
    (let [ls (-> s str/trim str/lower-case)]
      (fn [value]
        (str/includes? (str/lower-case (str value)) ls)))))

(defmethod col-filter-fn :regexp [[_ s]]
  (when (and (not (nil? s)) (not= "" s))
    (when-let [re #?(:clj (try (java.util.regex.Pattern/compile s)
                               (catch java.util.regex.PatternSyntaxException e
                                 (println (.getMessage e))))
                     :cljs (js/RegExp. s))]
      (fn [value]
        (re-find re (str value))))))

(defmethod col-filter-fn :ranges [[_ ranges]]
  (when-not (empty? ranges)
    (fn [value]
      (some #(let [[from to] (:range %)]
               (<= from value to))
            ranges))))

(defmethod col-filter-fn :one-of [[_ set]]
  (when-not (empty? set)
    (fn [value]
      (contains? set value))))

(defn row-filter-fn [active-filters]
  (let [idx+filter-fns (for [[idx col-filters] active-filters
                             active-filter col-filters
                             :let [filter-fn (col-filter-fn active-filter)]
                             :when filter-fn]
                         [idx filter-fn])]
    (fn [row]
      (->> idx+filter-fns
           (map (fn [[idx filter-fn]]
                  (let [value (viewer/->value (nth row idx))]
                    (filter-fn value))))
           (every? identity)))))

(defn find [pred xs]
  (reduce #(when (pred %2) (reduced %2)) nil xs))

(defn not-blank [s]
  (when-not (str/blank? s)
    s))

(def filter-regexp
  (let [modifier  "(?<modifier>[+-])?"
        key-qq    "\"(?<keyqq>[^\"]*)(?:\"|$)"
        key-q     "'(?<keyq>[^']*)(?:'|$)"
        key       "(?<key>[^\\s:]*)"
        delimeter "(?<delimeter>:)"
        value-qq  "\"(?<valueqq>[^\"]*)(?:\"|$)"
        value-q   "'(?<valueq>[^']*)(?:'|$)"
        value     "(?<value>[^\\s]*)"]
    (re-pattern (str modifier
                     "(?<fullkey>" key-qq "|" key-q "|" key ")"
                     "(?:" delimeter
                     "(?<fullvalue>" value-qq "|" value-q "|" value ")"
                     ")?"))))

(defn match-all [re s]
  #? (:clj (let [m ^java.util.regex.Matcher (re-matcher re s)]
             (loop [acc (transient [])]
               (if (.find m)
                 (recur (conj! acc
                               {:groups (mapv #(.group m %) (range (.groupCount m)))
                                :named  (persistent!
                                         (reduce-kv
                                          (fn [acc k i]
                                            (assoc! acc (keyword k) (.group m ^int i)))
                                          (transient {})
                                          (.namedGroups m)))
                                :start  (.start m)
                                :end    (.end m)}))
                 (persistent! acc))))
      :cljs (.matchAll s re)))

(defn parse-query [s]
  (vec
   (for [match (match-all filter-regexp s)
         :let [groups    #?(:clj (:named match) :cljs (.-groups match))
               start     #?(:clj (:start match) :cljs (.-index match))
               full      #?(:clj (-> match :groups first) :cljs (aget match 0))
               modifier  #?(:clj (-> groups :modifier not-blank) :cljs (-> groups .-modifier not-blank))
               key       #?(:clj (or
                                  (-> groups :keyqq not-blank)
                                  (-> groups :keyq not-blank)
                                  (-> groups :key not-blank))
                            :cljs (-> groups .-key not-blank))
               fullkey   #?(:clj (-> groups :fullkey not-blank) :cljs nil);;TODO
               delimeter #?(:clj (-> groups :delimeter not-blank) :cljs (-> groups .-fullkey not-blank))
               value     #?(:clj (or
                                  (-> groups :valueqq not-blank)
                                  (-> groups :valueq not-blank)
                                  (-> groups :value not-blank))
                            :cljs (-> groups .-value not-blank))
               fullvalue #?(:clj (-> groups :fullvalue not-blank) :cljs nil) ;;TODO
               ]]
     {:start-idx   start
      :modifier    modifier
      :key-quote   (when fullkey
                     (#{"'" "\""} (subs fullkey 0 1)))
      :key         key
      :value-idx   (when delimeter
                     (+ start
                        (count (or modifier ""))
                        (count fullkey)
                        (count delimeter)))
      :value-quote (when fullvalue
                     (#{"'" "\""} (subs fullvalue 0 1)))
      :value       value
      :end-idx     (+ start (count full))})))

(comment
  (doseq [modifier ["" "+" "-"]
          key      ["key" "'a key'" "'a:key'" "\"a key\"" "\"a:key\""]
          delim    [nil ":"]
          value    (if delim
                     [nil "value" "a:value" "'a value'" "'a:value'" "\"a value\"" "\"a:value\""]
                     [nil])
          :let     [s (str modifier key delim value)]]
    (prn s #_(match-all filter-regexp s) (parse-query s))))

(defn filter-by-query [{:as data
                        :keys [rows visible-paths]} query]
  (let [names      (into {}
                         (for [[path idx] (map vector visible-paths (range))]
                           [(str/join "/" (map name path)) idx]))
        filters    (for [{:keys [modifier key value]} (parse-query query)
                         :let [idx (names key)]
                         :when idx]
                     (cond
                       (and (= "+" modifier) (nil? value))
                       #(some? (nth % idx))

                       (and (= "-" modifier) (nil? value))
                       #(nil? (nth % idx))

                       (and (= "-" modifier) value)
                       #(not (str/index-of (str (nth % idx)) value))

                       value
                       #(str/index-of (str (nth % idx)) value)))
        filters    (remove nil? filters)]
    (if (seq filters)
      (update data :rows #(filter (apply every-pred filters) %))
      data)))

(defn normalize-table-data
  ([data] (normalize-table-data {} data))
  ([{:as opts
     :keys [active-filters search-query stats]
     :or {stats false}} data]
   (let [row-filter-fn (row-filter-fn active-filters)]
     (cond-> (cond
               (and (map? data) (-> data (viewer/get-safe :rows) sequential?)) (viewer/normalize-seq-to-vec data)
               (and (map? data) (sequential? (first (vals data)))) (normalize-map-of-seq opts data)
               (and (sequential? data) (map? (first data))) (normalize-seq-of-map opts data)
               (and (sequential? data) (sequential? (first data))) (viewer/normalize-seq-of-seq data)
               (empty? data) {:rows []}
               :else nil)
       stats (compute-table-summary opts)
       active-filters (compute-filters-data opts)
       true (compute-autocomplete-data opts)
       true (update :rows #(filter row-filter-fn %))
       search-query #?(:clj (filter-by-query search-query)
                       :cljs identity)))))

(def table-markup-viewer
  (assoc viewer/table-markup-viewer
         ;;FIXME this does not get expanded correctly: :render-fn `render/render-table-markup
         :render-fn 'nextjournal.clerk-table-stats.render/render-table-markup
         ;;:render-fn `render/render-table-markup
         :require-cljs true))

(def table-head-viewer
  (assoc viewer/table-head-viewer
         :render-fn 'nextjournal.clerk-table-stats.render/render-table-head
         ;; :render-fn `render/render-table-head
         :require-cljs true))

(def table-body-viewer
  (assoc viewer/table-body-viewer
         :render-fn '(fn [rows opts] (into [:tbody] (map-indexed (fn [idx row] (nextjournal.clerk.render/inspect-presented (update opts :path conj idx) row))) rows))))

(def table-row-viewer
  (assoc viewer/table-row-viewer
         :render-fn 'nextjournal.clerk-table-stats.render/render-table-row
         ;; :render-fn `render/render-table-row
         :require-cljs true))

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

(def table-viewers [table-head-viewer
                    table-body-viewer
                    table-markup-viewer
                    table-row-viewer])

(def viewer
  (-> viewer/table-viewer
      (assoc :transform-fn
             (fn transform-fn [{:as wrapped-value :nextjournal/keys [applied-viewer render-opts]}]
               (let [#?@(:clj [id (or (:id wrapped-value)
                                      (symbol (str (ns-name *ns*)) (str (gensym))))
                               var-name (symbol (namespace id) (str (name id) "-table"))
                               _ (when-not (resolve var-name)
                                   (when-some [ns' (find-ns (symbol (namespace var-name)))]
                                     (intern ns' (symbol (name var-name)) (doto (atom {:active-filters {}})
                                                                            (add-watch ::recompute
                                                                                       (fn [_ _ _ _]
                                                                                         (nextjournal.clerk/recompute!)))))))])
                     table-state #?(:clj @@(resolve var-name)
                                    :cljs nil)]

                 (if-let [{:keys [head rows summary filter-data autocomplete-data state]}
                          (normalize-table-data (merge render-opts table-state)
                                                (viewer/->value wrapped-value))]
                   (-> wrapped-value
                       (assoc :nextjournal/viewer table-markup-viewer)
                       (update :nextjournal/width #(or % :wide))
                       (update :nextjournal/render-opts merge {:num-cols (count (or head (first rows)))
                                                               #?@(:clj [:sync-var (viewer/->viewer-eval
                                                                                    (list 'nextjournal.clerk.render/intern-atom!
                                                                                          (list 'quote var-name) table-state))])
                                                               :number-col? (into #{}
                                                                                  (comp (map-indexed vector)
                                                                                        (keep #(when (number? (second %)) (first %))))
                                                                                  (not-empty (first rows)))
                                                               :summary summary
                                                               :initial-table-state table-state
                                                               :filter-data filter-data
                                                               :autocomplete-data autocomplete-data
                                                               :state state})
                       (update :nextjournal/render-opts dissoc :computed-columns :pre-process-stats)
                       (assoc :nextjournal/value (cond->> [(viewer/with-viewer table-body-viewer (merge (-> applied-viewer
                                                                                                            (select-keys [:page-size])
                                                                                                            (set/rename-keys {:page-size :nextjournal/page-size}))
                                                                                                        (select-keys wrapped-value [:nextjournal/page-size]))
                                                             (if (seq rows)
                                                               (map (partial viewer/with-viewer table-row-viewer) rows)
                                                               [(viewer/html [:span.italic "this table has no rows"])]))]
                                                   head (cons (viewer/with-viewer (:name table-head-viewer table-head-viewer) head)))))
                   (-> wrapped-value
                       viewer/mark-presented
                       (assoc :nextjournal/width :wide)
                       (assoc :nextjournal/value [(viewer/present wrapped-value)])
                       (assoc :nextjournal/viewer {:render-fn 'nextjournal.clerk.render/render-table-error}))))))
      (update :add-viewers (fn [v] (concat (filter (fn [{:keys [name]}] (not (contains? (set (map :name table-viewers)) name))) v)
                                           table-viewers)))))

#_(viewer/reset-viewers! :default
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
                                       :hide-columns [:ars/id :ductile/id]}} nested-seq-of-map)))
