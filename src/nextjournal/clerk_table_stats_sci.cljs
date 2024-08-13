(ns nextjournal.clerk-table-stats-sci
  (:require [reagent.core]
            [nextjournal.clerk.viewer]
            [nextjournal.clerk.render]))

(prn :load)

(defn table-col-bars [{:keys [col-type category-count distribution]} {:keys [table-state idx]}]
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
         (fn [i {:as bar :keys [label percentage]}]
           (let [bar-width (* width percentage)
                 filtered? (contains? filtered-bars label)
                 selected? (or (= @!selected-bar bar)
                               filtered?)]
             [:div.relative.overflow-hidden
              {:on-click #(do
                            (if filtered?
                              (swap! table-state update :filter update idx disj label)
                              (swap! table-state update :filter update idx (fnil conj #{}) label)))
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
                      (when (and (= :unique label) (< 80 bar-width))
                        " unique")
                      (when (and (= :empty label) (< 110 bar-width))
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
          (str "(" category-count " categories)"))]])))

(defn table-col-histogram
  [{:keys [col-type distribution width height]} {:keys [table-state idx]}]
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
       #_[:pre (pr-str [idx (:filter @table-state)])]
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
           [:div.absolute.right-0.top-0 (fmt to)]])]])))

(defn table-col-summary
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
      "x"]
     (if continuous?
       [table-col-histogram summary opts]
       [table-col-bars summary opts])]))

(defn table-head-viewer
  [header-row {:as opts :keys [table-state]}]
  (let [cells* (nextjournal.clerk.viewer/desc->values header-row)
        cells (mapcat #(if (vector? %)
                         (let [fst (first %)
                               vs (second %)]
                           (map (fn [v]
                                  {:cell [fst v]
                                   :sub true})
                                vs))
                         [{:cell %
                           :sub false}])
                      cells*)
        cells (map-indexed (fn [i e]
                             (assoc e :idx i))
                           cells)
        cell->idx (zipmap (map :cell cells) (map :idx cells))
        sub-headers (seq (filter :sub cells))
        header-cells (map (fn [cell]
                            {:idx (get cell->idx cell)
                             :cell cell}) cells*)]
    [:thead
     (into [:tr.print:border-b-2.print:border-black]
           (keep (fn [cell]
                   (let [header-cell (:cell cell)
                         index (:idx cell)
                         k (if (vector? header-cell)
                             (first header-cell)
                             header-cell)
                         title (when (or (string? k) (keyword? k) (symbol? k)) k)
                         {:keys [translated-keys column-layout number-col?] :or {translated-keys {}}} opts]
                     [:th.text-slate-600.text-xs.px-4.py-1.bg-slate-100.first:rounded-md-tl.last:rounded-md-r.border-l.first:border-l-0.border-slate-300.text-center.whitespace-nowrap.border-b
                      (cond-> {:class (str
                                       "print:text-[10px] print:bg-transparent print:px-[5px] print:py-[2px] "
                                       (when sub-headers "first:border-l-0 ")
                                       (if (and (ifn? number-col?) (number-col? index)) "text-right " "text-left "))}
                        (and column-layout (column-layout k)) (assoc :style (column-layout k))
                        (vector? header-cell) (assoc :col-span (count (first (rest header-cell))))
                        (and sub-headers (not (vector? header-cell))) (assoc :row-span 2)
                        title (assoc :title title))
                      [:div (get translated-keys k k)]
                      (when-not (vector? header-cell)
                        (when-let [summary (:summary opts)]
                          [table-col-summary (get-in summary [k])
                           {:table-state table-state
                            :idx index}]))])))
           header-cells)
     (when-not (empty? sub-headers)
       (into [:tr.print:border-b-2.print:border-black]
             (map
              (fn [{:keys [cell idx]}]
                [:th.text-slate-600.text-xs.px-4.py-1.bg-slate-100.first:rounded-md-tl.last:rounded-md-r.border-slate-300.text-center.whitespace-nowrap.border-b
                 {:class (if (< 0 idx) "border-l")}
                 (let [sub-header-key (second cell)]
                   [:<> (get (:translated-keys opts {}) sub-header-key sub-header-key)
                    (when-let [summary (:summary opts)]
                      [table-col-summary (get-in summary cell)
                       {:table-state table-state
                        :idx idx}])])])
              sub-headers)))]))

(defn table-row-viewer
  [row {:as opts :keys [path number-col?]}]
  (into [:tr.print:border-b-gray-500.hover:bg-gray-200.print:hover:bg-transparent
         {:class (str "print:border-b-[1px] "
                      (if (even? (peek path)) "bg-white" "bg-slate-50"))}]
        (map-indexed
         (fn [idx cell]
           [:td.px-4.py-2.text-sm.border-r.last:border-r-0
            {:class (str "print:text-[10px] print:bg-transparent print:px-[5px] print:py-[2px] "
                         (when (and (ifn? number-col?) (number-col? idx)) "text-right"))}
            (nextjournal.clerk.render/inspect-presented opts cell)]))
        row))
