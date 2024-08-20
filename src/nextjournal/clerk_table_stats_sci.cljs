(ns nextjournal.clerk-table-stats-sci
  (:require [clojure.string :as str]
            ["react-dom" :as react-dom]
            [reagent.core :as r]
            [nextjournal.clerk.render.hooks :as hooks]
            [nextjournal.clerk.viewer]
            [nextjournal.clerk.render]))

(defn table-col-bars [{:keys [col-type category-count distribution]} {:keys [table-state idx]}]
  (r/with-let [!selected-bar (r/atom nil)]
    (let [width 140
          height 30
          last-index (dec (count distribution))
          selected (-> @table-state :active-filters (get idx) :one-of (or #{}))]
      [:div
       [:div.text-slate-500.dark:text-slate-400.font-normal
        {:class "text-[12px] h-[24px] leading-[24px]"}
        (if-let [{:keys [count percentage]} @!selected-bar]
          (str count " rows (" (.toFixed (* 100 percentage) 2) "%)")
          col-type)]
       [:div.flex.relative
        {:style {:width width :height height}
         :class "rounded-sm overflow-hidden items-center "}
        (map-indexed
         (fn [i {:as bar :keys [label percentage]}]
           (let [bar-width (* width percentage)
                 filtered? (contains? selected label)
                 selected? (= @!selected-bar bar)]
             [:div.relative.overflow-hidden.text-slate-500.dark:text-slate-300.font-normal
              {:on-click #(do
                            (if filtered?
                              (swap! table-state update-in [:active-filters idx :one-of] disj label)
                              (swap! table-state update-in [:active-filters idx :one-of] (fnil conj #{}) label)))
               :on-mouse-enter #(reset! !selected-bar bar)
               :on-mouse-leave #(reset! !selected-bar nil)
               :class (cond
                        (and (empty? selected) (= :unique label))
                        ["bg-gray-300" "hover:bg-[#3B5FC0]" "hover:text-white" "dark:bg-gray-800" "dark:hover:bg-gray-700"]

                        (= :empty label)
                        ["bg-orange-200" "hover:bg-orange-300" "dark:bg-pink-900" "dark:bg-opacity-[0.7]" "dark:hover:bg-pink-800"]

                        (empty? selected)
                        ["bg-[#889DD7]" "hover:bg-[#3B5FC0]" "text-white" "dark:bg-sky-700" "dark:hover:bg-sky-500"]

                        filtered?
                        ["bg-[#889DD7]" "hover:bg-[#3B5FC0]" "text-white" "dark:bg-sky-700" "dark:hover:bg-sky-500"]

                        :else
                        ["bg-gray-300" "hover:bg-[#3B5FC0]" "hover:text-white" "dark:bg-gray-800" "dark:hover:bg-gray-700"])
               :style {:width bar-width
                       :height height}}
              (when (and (contains? #{:unique :empty} label) (< 30 bar-width))
                [:div.absolute.left-0.top-0.right-0.bottom-0.flex.items-center.justify-center.whitespace-nowrap
                 {:class "text-[12px]"}
                 (str (.toFixed (* 100 percentage) 2) "%"
                      (when (and (= :unique label) (< 80 bar-width))
                        " unique")
                      (when (and (= :empty label) (< 110 bar-width))
                        " empty/nil"))])
              (when-not (= i last-index)
                [:div.absolute.top-0.right-0.bottom-0
                 {:class "bg-white bg-opacity-[0.7] dark:bg-black w-[1px]"}])]))
         (sort-by #(str (:label %)) distribution))]
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
  (r/with-let [!selected-bar (r/atom nil)
               fmt (fn [x]
                     (cond (and (>= x 1000) (< x 1000000))
                           (str (.toFixed (/ x 1000) 0) "K")
                           (>= x 1000000)
                           (str (.toFixed (/ x 1000000) 0) "M")
                           :else (str (.toFixed x 0))))]
    (let [filtered-bars (-> @table-state :active-filters (get idx) :ranges not-empty)
          max (:count (apply max-key :count distribution))
          last-index (dec (count distribution))
          from (-> distribution first :range first)
          to (-> distribution last :range last)]
      [:div
       [:div.text-slate-500.dark:text-slate-400.font-normal
        {:class "text-[12px] h-[24px] leading-[24px]"}
        (if-let [{:keys [count percentage]} @!selected-bar]
          (str count " rows (" (.toFixed (* percentage 100) 2) "%)")
          col-type)]
       [:div.flex.relative
        {:style {:width width :height height}}
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
                              (swap! table-state update-in [:active-filters idx :ranges] disj bar)
                              (swap! table-state update-in [:active-filters idx :ranges] (fnil conj #{}) bar))
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
         (sort-by #(str (:label %)) distribution))]
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
        filtered? (if continuous?
                    (not-empty (get-in @table-state [:active-filters idx :ranges]))
                    (not-empty (get-in @table-state [:active-filters idx :one-of])))]
    [:div.flex
     [:div
      {:class (cond-> ["text-indigo-200"]
                filtered?
                (conj "text-black" "cursor-pointer"))
       :on-click #(when filtered?
                    (swap! table-state update-in [:active-filters idx] dissoc (if continuous? :ranges :one-of)))}
      "x"]
     (if continuous?
       [table-col-histogram summary opts]
       [table-col-bars summary opts])]))

(defn icon-chevron []
  [:svg {:class "h-5 w-5 text-slate-400"
         :viewBox "0 0 20 20"
         :fill "currentColor"
         :aria-hidden "true"}
   [:path {:d "M5.23 7.21a.75.75 0 011.06.02L10 11.168l3.71-3.938a.75.75 0 111.08 1.04l-4.25 4.5a.75.75 0 01-1.08 0l-4.25-4.5a.75.75 0 01.02-1.06z"
           :fill-rule "evenodd"
           :clip-rule "evenodd"}]])

(defn icon-reset []
  [:svg {:class "h-5 w-5 text-slate-400"
         :viewBox "0 0 20 20"
         :stroke "currentColor"
         :stroke-width "1.5"
         :stroke-linecap "round"
         :aria-hidden "true"}
   [:path {:d "M6.5 6.5L13.5 13.5M13.5 6.5L6.5 13.5"}]])

(defn checkbox [checked?]
  [:input.size-4.appearance-none.rounded.border
   {:type :checkbox
    :checked? (boolean checked?)
    :class (if checked?
             ["border-blue-500" "bg-blue-500" "bg-contain" "bg-no-repeat"]
             ["border-slate-300" "bg-white"])
    :style (when checked?
             {:background-image "url(\"data:image/svg+xml,%3csvg viewBox='0 0 16 16' fill='white' xmlns='http://www.w3.org/2000/svg'%3e%3cpath d='M12.207 4.793a1 1 0 010 1.414l-5 5a1 1 0 01-1.414 0l-2-2a1 1 0 011.414-1.414L6.5 9.086l4.293-4.293a1 1 0 011.414 0z'/%3e%3c/svg%3e\")"})}])

(defn table-col-filter-text [filter-type {:keys [table-state idx]}]
  (let [value (-> @table-state :active-filters (get idx) filter-type (or ""))]
    [:div.relative
     [:input.w-full.cursor-default.rounded.px-2.shadow-sm.ring-1.ring-slate-300.font-normal
      {:class ["placeholder-slate-400"
               "py-0.5"
               "focus:outline-none"
               "focus:ring-2"
               "focus:ring-blue-500"
               "sm:text-sm"
               "sm:leading-6"
               "bg-white"]
       :type :text
       :placeholder (-> filter-type name str/capitalize (str "..."))
       :defaultValue value
       :on-input (fn [event]
                   (let [value (.. event -currentTarget -value)]
                     (if (str/blank? value)
                       (swap! table-state update-in [:active-filters idx] dissoc filter-type)
                       (swap! table-state assoc-in [:active-filters idx filter-type] (str/trim value)))))}]
     (when (not= "" value)
       [:span.absolute.inset-y-0.right-0.flex.items-center.pr-1
        {:on-click (fn [event]
                     (let [target (.-currentTarget event)
                           input  (.-previousElementSibling target)]
                       (swap! table-state update-in [:active-filters idx] dissoc filter-type)
                       (set! (.-value input) "")))}
        [icon-reset]])]))

(defn find-parent [node pred]
  (loop [node node]
    (cond
      (pred node)
      node

      (nil? node)
      nil

      :else
      (recur (.-parentNode node)))))

(defn child? [node parent]
  (boolean
   (find-parent node #(identical? % parent))))

(defn table-col-filter-multiselect [{:keys [filter-data table-state idx]}]
  (r/with-let [!expanded    (r/atom false)
               !portal-root (atom nil)
               value->str   #(cond
                               (= :nextjournal/missing %)
                               "N/A"

                               :else
                               (str %))
               !scroll-left (r/atom 0)]
    (let [!button-ref (hooks/use-ref nil)
          !popup-ref  (hooks/use-ref nil)
          selected    (-> @table-state :active-filters (get idx) :one-of (or #{}))]
      ;; close popup when clicking outside
      (hooks/use-effect
       (fn []
         (let [on-click (fn [event]
                          (cond
                            (child? (.-target event) @!button-ref)
                            (swap! !expanded not)

                            (and @!expanded
                                 (not (child? (.-target event) @!popup-ref)))
                            (reset! !expanded false)))]
           (js/document.addEventListener "click" on-click)
           #(js/document.removeEventListener "click" on-click))))
      ;; find portal root
      (hooks/use-effect
       (fn []
         (reset! !portal-root
                 (-> @!button-ref
                     (find-parent  #(-> % .-classList (.contains "result-viewer")))
                     (.querySelector ".relative")))))
      ;; update horizontal scroll position
      (hooks/use-effect
       (fn []
         (let [container (-> @!button-ref
                             (find-parent #(= "TABLE" (.-nodeName %)))
                             .-parentNode)
               callback  (fn [event]
                           (reset! !scroll-left (-> event .-currentTarget .-scrollLeft)))]
           (reset! !scroll-left (-> container .-scrollLeft))
           (.addEventListener container "scroll" callback)
           #(.removeEventListener container "scroll" callback))))
      ;; DOM
      [:<>
       [:button.block.font-normal.relative.w-full.cursor-default.rounded.text-left.shadow-sm.ring-1.ring-slate-300
        {:ref !button-ref
         :type "button"
         :aria-haspopup "listbox"
         :aria-expanded (str (boolean @!expanded))
         :class ["pl-2"
                 "pr-7"
                 "py-0.5"
                 "focus:outline-none"
                 "focus:ring-2"
                 "focus:ring-blue-500"
                 "sm:text-sm"
                 "sm:leading-6"
                 (if @!expanded
                   "bg-slate-100"
                   "bg-white")]}
        (if (empty? selected)
          [:span.block.truncate.text-slate-400 "Select..."]
          [:span.block.truncate (->> selected
                                     (map value->str)
                                     (str/join ", "))])
        [:span.pointer-events-none.absolute.inset-y-0.right-0.flex.items-center.pr-1
         [icon-chevron]]]
       (when @!expanded
         (react-dom/createPortal
          (r/as-element
           [:ul.absolute.z-10.rounded.font-sans.bg-white.py-1.text-base.shadow-lg.ring-1.ring-slate-300.not-prose.overflow-y-scroll
            {:ref !popup-ref
             :tabindex "-1"
             :role "listbox"
             :class ["focus:outline-none" "sm:text-sm"]
             :style (let [button    @!button-ref
                          bottom    (+ (.-offsetTop button)
                                       (.-offsetHeight button))
                          left      (- (.-offsetLeft button) @!scroll-left)]
                      {:left       left
                       :top        (str "calc(" bottom "px + 0.25rem)")
                       :min-width  (.-offsetWidth button)
                       :max-height "50svh"})}
            (for [value (sort-by str (:values filter-data))]
              [:li.cursor-default.select-none.flex
               {:role "option"
                :class ["pl-2"
                        "pr-3"
                        "py-0.5"
                        "gap-1.5"
                        "hover:bg-slate-200"
                        "sm:text-sm"
                        "sm:leading-6"]
                :on-pointer-down
                (fn [_]
                  (if (some? (selected value))
                    (swap! table-state update-in [:active-filters idx :one-of] disj value)
                    (swap! table-state update-in [:active-filters idx :one-of] (fnil conj #{}) value)))}
               [:span.flex.items-center
                [checkbox (some? (selected value))]]
               (value->str value)])])
          @!portal-root))])))

(defn table-col-filter [{:as opts
                         :keys [filter-type]
                         :or {filter-type :text}}]
  [:div
   (case filter-type
     :text
     [table-col-filter-text :substring opts]

     :regexp
     [table-col-filter-text :regexp opts]

     :multiselect
     [table-col-filter-multiselect opts])])

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
                             :cell cell}) cells*)
        !thead       (hooks/use-ref nil)]
    (hooks/use-effect
     (fn []
       (let [thead @!thead]
         (doseq [tr (.-children thead)
                 th (.-children tr)]
           (set! (.-width (.-style th)) (str (.-offsetWidth th) "px"))
           (set! (.-maxWidth (.-style th)) (str (.-offsetWidth th) "px"))))))
    [:thead {:ref !thead}
     (into [:tr.print:border-b-2.print:border-black]
           (keep (fn [cell]
                   (let [header-cell (:cell cell)
                         idx (:idx cell)
                         k (if (vector? header-cell)
                             (first header-cell)
                             header-cell)
                         title (when (or (string? k) (keyword? k) (symbol? k)) k)
                         {:keys [translated-keys column-layout number-col?] :or {translated-keys {}}} opts]
                     [:th.text-slate-600.text-xs.px-1.py-1.bg-slate-100.first:rounded-md-tl.last:rounded-md-r.border-l.first:border-l-0.border-slate-300.text-center.whitespace-nowrap.border-b.align-bottom
                      (cond-> {:class ["print:text-[10px]"
                                       "print:bg-transparent"
                                       "print:px-[5px]"
                                       "print:py-[2px]"
                                       (when sub-headers
                                         "first:border-l-0 ")]}
                        (and column-layout (column-layout k)) (assoc :style (column-layout k))
                        (vector? header-cell) (assoc :col-span (count (first (rest header-cell))))
                        (and sub-headers (not (vector? header-cell))) (assoc :row-span 2)
                        title (assoc :title title))
                      [:div.flex.flex-col.gap-1
                       [:div (get translated-keys k k)]
                       (when-not (vector? header-cell)
                         [:<>
                          (let [col-filter (get-in (:filters opts) [k])]
                            (when (or col-filter (true? (:filters opts)) (keyword? (:filters opts)))
                              [table-col-filter {:filter-type col-filter
                                                 :filter-data (get (:filter-data opts) idx)
                                                 :table-state table-state
                                                 :idx idx}]))
                          (when-let [summary (:summary opts)]
                            [table-col-summary (get-in summary [k])
                             {:table-state table-state
                              :idx idx}])])]])))
           header-cells)
     (when-not (empty? sub-headers)
       (into [:tr.print:border-b-2.print:border-black]
             (map
              (fn [{:keys [cell idx]}]
                [:th.text-slate-600.text-xs.px-1.py-1.bg-slate-100.first:rounded-md-tl.last:rounded-md-r.border-slate-300.text-center.whitespace-nowrap.border-b.align-bottom
                 {:class (if (< 0 idx) "border-l")}
                 (let [sub-header-key (second cell)
                       col-filter (get (:filters opts) cell)]
                   [:div.flex.flex-col.gap-1
                    (get (:translated-keys opts {}) sub-header-key sub-header-key)
                    (when (or col-filter (true? (:filters opts)) (keyword? (:filters opts)))
                      [table-col-filter {:filter-type col-filter
                                         :filter-data (get (:filter-data opts) idx)
                                         :table-state table-state
                                         :idx idx}])
                    (when-let [summary (:summary opts)]
                      [table-col-summary (get-in summary cell)
                       {:table-state table-state
                        :idx idx}])])])
              sub-headers)))]))

(defn table-row-viewer
  [row {:as opts :keys [path number-col?]}]
  (into [:tr.print:border-b-gray-500.hover:bg-slate-200.print:hover:bg-transparent.group
         {:class (str "print:border-b-[1px] "
                      (if (even? (peek path)) "bg-white" "bg-slate-50"))}]
        (map-indexed
         (fn [idx cell]
           [:td.px-2.text-sm.border-r.last:border-r-0.group-hover:border-slate-300
            {:class ["py-1.5"
                     "print:text-[10px]"
                     "print:bg-transparent"
                     "print:px-[5px]"
                     "print:py-[2px]"
                     (when (and (ifn? number-col?) (number-col? idx))
                       "text-right")]}
            (nextjournal.clerk.render/inspect-presented opts cell)]))
        row))

(comment
  ;; add :jvm-opts ["-Dclerk.render_repl={}"
  ;; cider-connect, use port 1339
  ;; Switch buffer to clojure-mode
  ;; sesman-link-with-buffer, select sci repl
  (nextjournal.clerk.render/re-render)
  (doseq [[k var] (ns-publics *ns*)]
    (add-watch var (keyword (str k))
               (fn [_ _ _ _]
                 (nextjournal.clerk.render/re-render)))))
