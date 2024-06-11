;; # 📊 Clerk Table Stats
^{:nextjournal.clerk/visibility :hide-ns}
(ns nextjournal.clerk-table-stats
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as viewer]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def table-col-bars
  '(fn table-col-bars [{:keys [col-type category-count distribution width height idx]} {:keys [table-state idx]}]
     (reagent.core/with-let [!selected-bar (reagent.core/atom nil)]
       (let [width 140
             height 30
             last-index (dec (count distribution))
             filtered-bars (-> (get (:filter @table-state) idx)
                               not-empty)]
         [:div
          #_[:pre (pr-str filtered-bars)]
          [:div.text-slate-500.dark:text-slate-400.font-normal
           {:class "text-[12px] h-[24px] leading-[24px]"}
           (if-let [{:keys [count percentage]} @!selected-bar]
             (str count " rows (" (.toFixed (* 100 percentage) 2) "%)")
             col-type)]
          (into
           [:div.flex.relative
            {:style {:width width :height height}
             :class "rounded-sm overflow-hidden items-center "}]
           (map-indexed
            (fn [i {:as bar :keys [label count percentage range]}]
              (let [bar-width (* width percentage)
                    filtered? (contains? filtered-bars label)
                    selected? (or (= @!selected-bar bar)
                                  filtered?)]
                [:div.relative.overflow-hidden
                 {:on-click #(if filtered?
                               (swap! table-state update :filter update idx disj label)
                               (swap! table-state update :filter update idx (fnil conj #{}) label))
                  :on-mouse-enter #(reset! !selected-bar bar)
                  :on-mouse-leave #(reset! !selected-bar nil)
                  :class (case label
                           :unique "bg-gray-100 hover:bg-gray-200 dark:bg-gray-800 dark:hover:bg-gray-700 "
                           :empty "bg-orange-200 hover:bg-orange-300 dark:bg-pink-900 dark:bg-opacity-[0.7] dark:hover:bg-pink-800 "
                           (cond-> ["bg-indigo-200 hover:bg-indigo-300 dark:bg-sky-700 dark:hover:bg-sky-500"]
                             selected? (conj "bg-indigo-400")))
                  :style {:width bar-width
                          :height height}}
                 (when (and (contains? #{:unique :empty} label) (< 30 bar-width))
                   [:div.text-slate-500.dark:text-slate-300.font-normal.absolute.left-0.top-0.right-0.bottom-0.flex.items-center.justify-center.whitespace-nowrap
                    {:class "text-[12px]"}
                    (str (.toFixed (* 100 percentage) 2) "%"
                         (when (and (= label :unique) (< 80 bar-width))
                           " unique")
                         (when (and (= label :empty) (< 110 bar-width))
                           " empty/nil"))])
                 (when-not (= i last-index)
                   [:div.absolute.top-0.right-0.bottom-0
                    {:class "bg-white bg-opacity-[0.7] dark:bg-black w-[1px]"}])]))
            distribution))
          [:div.text-slate-500.dark:text-slate-400.font-normal.truncate
           {:class "text-[12px] h-[24px] mt-[1px] leading-[24px] "
            :style {:width width}}
           (if-let [{:keys [count label]} @!selected-bar]
             (case label
               :unique (str count " unique values")
               :empty (str count " empty/nil values")
               (str label))
             (str "(" category-count " categories)"))]]))))

(def table-col-histogram
  '(fn table-col-histogram [{:keys [col-type distribution width height] :as col} {:keys [table-state idx]}]
     (reagent.core/with-let [!selected-bar (reagent.core/atom nil)
                             fmt (fn [x]
                                   (cond (and (>= x 1000) (< x 1000000))
                                         (str (.toFixed (/ x 1000) 0) "K")
                                         (>= x 1000000)
                                         (str (.toFixed (/ x 1000000) 0) "M")
                                         :else (str (.toFixed x 0))))]
       (let [filtered-bars (-> (get (:filter @table-state) idx)
                               not-empty)
             max (:count (apply max-key :count distribution))
             last-index (dec (count distribution))
             from (-> distribution first :range first)
             to (-> distribution last :range last)]
         [:div
          #_[:pre (pr-str filtered-bars)]
          [:div.text-slate-500.dark:text-slate-400.font-normal
           {:class "text-[12px] h-[24px] leading-[24px]"}
           (if-let [{:keys [count percentage]} @!selected-bar]
             (str count " rows (" (.toFixed (* percentage 100) 2) "%)")
             col-type)]
          (into
           [:div.flex.relative
            {:style {:width width :height height}}]
           (map-indexed
            (fn [i {:as bar row-count :count :keys [range]}]
              (let [bar-width (/ width (count distribution))
                    filtered? (contains? filtered-bars bar)
                    selected? (or (= @!selected-bar bar)
                                  filtered?)
                    last? (= i last-index)]
                [:div.relative.group
                 {:on-mouse-enter #(reset! !selected-bar bar)
                  :on-mouse-leave #(reset! !selected-bar nil)
                  :style {:width bar-width
                          :height (+ height 24)}}
                 [:div.w-full.flex.items-end
                  {:style {:height height}}
                  [:div.w-full.relative
                   {:on-click #(if filtered?
                                 (swap! table-state update :filter update idx disj bar)
                                 (swap! table-state update :filter update idx (fnil conj #{}) bar))
                    :style {:height (* (/ row-count max) height)}
                    :class (let [css ["group-hover:bg-red-300 dark:bg-sky-700 dark:group-hover:bg-sky-500 "]]
                             (if selected?
                               (conj css "bg-red-400")
                               (conj css "bg-red-200")))}
                   (when-not last?
                     [:div.absolute.top-0.right-0.bottom-0
                      {:class "bg-white dark:bg-black w-[1px]"}])]]
                 [:div.relative
                  {:class "mt-[1px] h-[1px] bg-slate-300 dark:bg-slate-700"}
                  (when selected?
                    [:div.absolute.left-0.top-0.bg-black.dark:bg-white
                     {:class (str "h-[2px] " (if last? "right-0" "right-[1px]"))}])]
                 (when selected?
                   [:<>
                    [:div.absolute.left-0.text-left.text-slate-500.dark:text-slate-400.font-normal.pointer-events-none
                     {:class "text-[12px] h-[24px] leading-[24px] -translate-x-full"
                      :style {:top height}}
                     (fmt (first range))]
                    [:div.absolute.right-0.text-right.text-slate-500.dark:text-slate-400.font-normal.pointer-events-none
                     {:class "text-[12px] h-[24px] leading-[24px] translate-x-full"
                      :style {:top height}}
                     (fmt (last range))]])]))
            distribution))
          [:div.text-slate-500.dark:text-slate-400.font-normal.truncate
           {:class "text-[12px] h-[24px] leading-[24px] "
            :style {:width width}}
           (when-not @!selected-bar
             [:div.relative.pointer-events-none
              [:div.absolute.left-0.top-0 (fmt from)]
              [:div.absolute.right-0.top-0 (fmt to)]])]]))))

(def table-col-summary
  (walk/postwalk-replace {'table-col-histogram table-col-histogram
                          'table-col-bars table-col-bars}
                         '(defn table-col-summary
                            [{:as summary :keys [continuous?]} {:keys [table-state idx] :as opts}]
                            (let [summary (assoc summary :width 140 :height 30)
                                  filtered? (get (:filter @table-state) idx)]
                              [:div.flex
                               [:div
                                {:class (cond-> ["text-indigo-200"]
                                          filtered?
                                          (conj "text-black" "cursor-pointer"))
                                 :on-click #(when filtered?
                                              (swap! table-state update :filter dissoc idx))}
                                "x"
                                ]
                               (if continuous?
                                 [table-col-histogram summary opts]
                                 [table-col-bars summary opts])]))))

(def table-head-viewer-fn
  (walk/postwalk-replace
   {'table-col-summary table-col-summary}
   '(fn table-head-viewer [header-row {:as opts :keys [path table-state]}]
      (let [header-cells (nextjournal.clerk.viewer/desc->values header-row)
            sub-headers (filter vector? header-cells)
            _ (prn :header-cells header-cells :sub-headers sub-headers)
            ;; _ (prn :paths (paths header-cells))
            sub-headers? (seq sub-headers)]
        [:thead
         (into [:tr.print:border-b-2.print:border-black]
               (map-indexed (fn [index cell]
                              (let [header-cell cell]
                                (let [v (cond
                                          (vector? header-cell) (first header-cell)
                                          :else header-cell)
                                      k (if (and (vector? header-cell)) (first header-cell) header-cell)
                                      title (when (or (string? v) (keyword? v) (symbol? v)) v)
                                      {:keys [translated-keys column-layout number-col? filters update-filters! !expanded-at] :or {translated-keys {}}} opts]
                                  [:th.text-slate-600.text-xs.px-4.py-1.bg-slate-100.first:rounded-md-tl.last:rounded-md-r.border-l.border-slate-300.text-center.whitespace-nowrap.border-b
                                   (cond-> {:class (str
                                                    "print:text-[10px] print:bg-transparent print:px-[5px] print:py-[2px] "
                                                    (when sub-headers? "first:border-l-0 ")
                                                    (if (and (ifn? number-col?) (number-col? index)) "text-right " "text-left "))}
                                     (and column-layout (column-layout k)) (assoc :style (column-layout k))
                                     (vector? header-cell) (assoc :col-span (count (first (rest header-cell))))
                                     (and sub-headers? (not (vector? header-cell))) (assoc :row-span 2)
                                     title (assoc :title title))
                                   [:div v]
                                   (when-not (vector? cell)
                                     (when-let [summary (:summary opts)]
                                       [table-col-summary (get-in summary [k])
                                        {:table-state table-state
                                         :idx index}]))]))))
               header-cells)
         (when-not (empty? sub-headers)
           (into [:tr.print:border-b-2.print:border-black]
                 (map-indexed
                  (fn [idx cell]
                    [:th.text-slate-600.text-xs.px-4.py-1.bg-slate-100.first:rounded-md-tl.last:rounded-md-r.border-l.border-slate-300.text-center.whitespace-nowrap.border-b

                     (when-let [summary (:summary opts)]
                       [table-col-summary (get-in summary cell)
                        {:table-state table-state
                         :idx idx}])
                     (second cell)])
                  sub-headers)
                 ))]))))

(defn deep-merge [& maps]
  (letfn [(m [& xs]
            (if (some #(and (map? %) (not (record? %))) xs)
              (apply merge-with m xs)
              (last xs)))]
    (reduce m maps)))

(comment
  (deep-merge {:a {:b {:c 3}}} {:a {:b {:c 4 :d 5}}})
  )

(defn paths->head [paths]
  (->> paths
       (group-by first)
       (map (fn [[k paths]] (if (< 1 (count paths)) [k (mapv (comp first rest) paths)] k)))))

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
  (reduce (fn [ps p]
            (if (contains? hidden p) ps (vec (conj ps p))))
          []
          paths))

(defn normalize-seq-of-map
  ([s] (normalize-seq-of-map {} s))
  ([{:keys [column-order computed-columns hide-columns group-headers schema]} s]
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
         visible-paths (cond->> ordered-paths
                         (seq hidden-paths) (->visible-paths hidden-paths))]
     {:schema (paths->head ordered-paths)
      :paths ordered-paths
      :hidden-paths hidden-paths
      :head (paths->head visible-paths)
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

(defn compute-table-summary [{:as data :keys [rows paths]}]
  (cond-> data
    paths (assoc :summary
                 (let [cols (transpose rows)]
                   (->> (map (fn [path i]
                              (let [col (nth cols i)]
                                [path (compute-col-summary col)]))
                            paths
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
  (compute-table-summary grouped)
  )

(defn normalize-table-data
  ([data] (normalize-table-data {} data))
  ([{:as opts filter-spec :filter} data]
   (-> (cond
         (and (map? data) (-> data (viewer/get-safe :rows) sequential?)) (viewer/normalize-seq-to-vec data)
         (and (map? data) (sequential? (first (vals data)))) (normalize-map-of-seq opts data)
         (and (sequential? data) (map? (first data))) (normalize-seq-of-map opts data)
         (and (sequential? data) (sequential? (first data))) (viewer/normalize-seq-of-seq data)
         :else nil)
       compute-table-summary
       (update :rows (partial filter (fn [row]
                                       (let [ks (keys filter-spec)]
                                         (or (empty? ks)
                                             (let [filters (map #(get filter-spec %) ks)
                                                   values (map #(nextjournal.clerk/->value (nth row %)) ks)]
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
                                                       (throw (js/Error. (str "no sync var: " sync-var))))]
                   [:div.bg-white.rounded-lg.border.border-slate-300.shadow-sm.font-sans.text-sm.not-prose.overflow-x-auto
                    {:class "print:overflow-none print:text-[10px] print:shadow-none print:rounded-none print:border-none"}
                    (into
                     [:table.w-full]
                     (nextjournal.clerk.render/inspect-children (assoc opts :table-state table-state))
                     ;; debug atom+head+body #_
                     head+body)]))})

(def table-head-viewer
  {:render-fn table-head-viewer-fn})

(def table-body-viewer
  {:render-fn '(fn [rows opts] (into [:tbody] (map-indexed (fn [idx row] (nextjournal.clerk.render/inspect-presented (update opts :path conj idx) row))) rows))})

(def table-row-viewer
  {:render-fn '(let []
                 (fn [row {:as opts :keys [path number-col? table-state]}]
                   (into [:tr.print:border-b-gray-500.hover:bg-gray-200.print:hover:bg-transparent
                          {:class (str "print:border-b-[1px] "
                                       (if (even? (peek path)) "bg-white" "bg-slate-50"))}]
                         (map-indexed
                          (fn [idx cell]
                            [:td.px-4.py-2.text-sm.border-r.last:border-r-0
                             {:class (str "print:text-[10px] print:bg-transparent print:px-[5px] print:py-[2px] "
                                          (when (and (ifn? number-col?) (number-col? idx)) "text-right"))}
                             (nextjournal.clerk.render/inspect-presented opts cell)]))
                         row)))})

(def viewer
  (assoc viewer/table-viewer
         :transform-fn
         (fn transform-fn [{:as wrapped-value :keys [id] :nextjournal/keys [applied-viewer render-opts]}]
           (let [var-name (symbol (namespace id) (str (name id) "-table"))
                 _ (when-not (resolve var-name)
                     (when-some [ns' (find-ns (symbol (namespace var-name)))]
                       (intern ns' (symbol (name var-name)) (doto (atom {:filter {}})
                                                              (add-watch :foo (fn [_k _r _o _n] (clerk/recompute!)))))))
                 table-state @@(resolve var-name)]

             (if-let [{:keys [head rows summary state]} (normalize-table-data (merge render-opts table-state)
                                                                              (viewer/->value wrapped-value))]
               (-> wrapped-value
                   (assoc :nextjournal/viewer table-markup-viewer)
                   (update :nextjournal/width #(or % :wide))
                   (update :nextjournal/render-opts merge {:num-cols (count (or head (first rows)))
                                                           :sync-var (viewer/->viewer-eval
                                                                      (list 'nextjournal.clerk.render/intern-atom!
                                                                            (list 'quote var-name) {:filter {} :init 2}))
                                                           :number-col? (into #{}
                                                                              (comp (map-indexed vector)
                                                                                    (keep #(when (number? (second %)) (first %))))
                                                                              (not-empty (first rows)))
                                                           :summary summary
                                                           :state state})
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
                                                                                   (str/upper-case (str/join (take 3 (:compound/name m)))))}}} seq-of-map))

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

(clerk/example
  #_(normalize-seq-of-map nested-seq-of-map)
  (normalize-seq-of-map {:group-headers true} nested-seq-of-map)
  (normalize-seq-of-map {:group-headers [:entry/transport]} nested-seq-of-map)
  (normalize-seq-of-map {:group-headers true
                         :column-order [:compound/name
                                        [:entry/transport [:transport/name :transport/mode]]
                                        [:exit/transport [:transport/name :transport/mode]]
                                        :entry/datetime]
                         :hide-columns [:ars/id :ductile/id]} nested-seq-of-map))

(clerk/table {::clerk/render-opts {:group-headers true}} nested-seq-of-map)
(clerk/table {::clerk/render-opts {:group-headers [:entry/transport]}} nested-seq-of-map)
(clerk/table {::clerk/render-opts {:group-headers true
                                   :column-order [:compound/name
                                                  [:entry/transport [:transport/name :transport/mode]]
                                                  [:exit/transport [:transport/name :transport/mode]]
                                                  :entry/datetime]
                                   :hide-columns [:ars/id :ductile/id]}} nested-seq-of-map)
