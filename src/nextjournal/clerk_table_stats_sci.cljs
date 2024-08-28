(ns nextjournal.clerk-table-stats-sci
  (:require ["@codemirror/language" :refer [HighlightStyle syntaxHighlighting LanguageDescription]]
            ["@codemirror/state" :refer [Compartment EditorState RangeSet RangeSetBuilder Text]]
            ["@codemirror/view" :refer [Decoration EditorView keymap ViewPlugin]]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
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

(def input-classes
  ["w-full" "cursor-default" "rounded" "px-2" "shadow-sm" "ring-1" "ring-slate-300"
   "font-normal" "placeholder-slate-400" "py-0.5" "focus:outline-none" "focus:ring-2"
   "focus:ring-blue-500" "sm:text-sm" "sm:leading-6" "bg-white"])

(defn merge-attrs [a b]
  (cond
    (and (string? a) (string? b))
    (str a " " b)

    (and (sequential? a) (sequential? b))
    (concat a b)

    :else
    b))

(defn input [value set-value opts]
  [:div.relative.flex.items-center
   [:input
    (merge-with merge-attrs
                {:class     input-classes
                 :type      :text
                 :value     value
                 :on-change (fn [event]
                              (set-value (.. event -currentTarget -value)))}
                opts)]
   (when (not= "" value)
     [:span.absolute.right-1
      {:on-pointer-down
       (fn [event]
         (let [target (.-currentTarget event)
               input  (.-previousElementSibling target)]
           (set-value "")
           ;; keep focus
           (.preventDefault event)))}
      [icon-reset]])])

(defn table-col-filter-text [filter-type {:keys [table-state idx]}]
  (let [value     (-> @table-state :active-filters (get idx) filter-type (or ""))
        set-value #(swap! table-state assoc-in [:active-filters idx filter-type] %)]
    [input value set-value
     {:placeholder (-> filter-type name str/capitalize (str "..."))
      :auto-focus  true}]))

(defn find-parent [node pred]
  (loop [node node]
    (cond
      (pred node) node
      (nil? node) nil
      :else (recur (.-parentNode node)))))

(defn child? [node parent]
  (boolean
   (find-parent node #(identical? % parent))))

(defn adjust-offset-x [x]
  (-> x
      (+ (or js/window.pageXOffset
             (.-scrollLeft js/document.documentElement)
             (.-scrollLeft js/document.body)))
      (- (or (.-clientLeft js/document.documentElement)
             (.-clientLeft js/document.body)
             0))))

(defn adjust-offset-y [y]
  (-> y
      (+ (or js/window.pageYOffset
             (.-scrollTop js/document.documentElement)
             (.-scrollTop js/document.body)))
      (- (or (.-clientTop js/document.documentElement)
             (.-clientTop js/document.body)
             0))))

(defn offset [^js el]
  (let [box (.getBoundingClientRect el)]
    {:top (adjust-offset-y (.-top box))
     :left (adjust-offset-x (.-left box))}))

(defn popup [opts button-markup popup-markup]
  (r/with-let [!expanded-default (r/atom false)
               !scroll-left (r/atom 0)
               !portal-root (atom nil)]
    (let [!button-ref (hooks/use-ref nil)
          !popup-ref  (hooks/use-ref nil)
          !expanded   (or (:!expanded opts) !expanded-default)
          {:keys [on-close]} opts]
      ;; close popup when clicking outside
      (hooks/use-effect
       (fn []
         (let [on-click (fn [event]
                          (cond
                            (child? (.-target event) @!button-ref)
                            (swap! !expanded not)

                            (and @!expanded
                                 (not (child? (.-target event) @!popup-ref)))
                            (do
                              (reset! !expanded false)
                              (when on-close
                                (on-close)))))]
           (js/document.addEventListener "click" on-click)
           #(js/document.removeEventListener "click" on-click))))
      ;; update horizontal scroll position
      (hooks/use-effect
       (fn []
         (let [container (-> @!button-ref
                             (find-parent  #(-> % .-classList (.contains "result-viewer")))
                             (.querySelector ".relative > .overflow-x-auto > div"))
               callback  (fn [event]
                           (reset! !scroll-left (-> event .-currentTarget .-scrollLeft)))]
           (reset! !scroll-left (-> container .-scrollLeft))
           (.addEventListener container "scroll" callback)
           #(.removeEventListener container "scroll" callback))))
      [:<>
       [:div {:class ["relative"]
              :ref !button-ref}
        button-markup]
       (when @!expanded
         (react-dom/createPortal
          (r/as-element
           [:div {:ref !popup-ref
                  :class ["absolute" "z-10" "overflow-y-scroll"]
                  :style (let [button @!button-ref
                               scroll-left @!scroll-left ;; trigger recalc
                               {:keys [left top]} (offset button)
                               bottom (+ top (.-offsetHeight button))]
                           {:left       left
                            :top        (str "calc(" bottom "px + 0.25rem)")
                            :min-width  (.-offsetWidth button)
                            :max-height "50svh"})}
            popup-markup])
          js/document.body))])))

(defn normalize-str [s]
  (when s
    (-> s str/trim str/lower-case)))

(defn table-col-filter-multiselect [{:keys [filter-data table-state idx]}]
  (r/with-let [!expanded   (r/atom false)
               !input-text (r/atom "")
               value->str  #(if (= :nextjournal/missing %) "N/A" (str %))]
    (let [selected (-> @table-state :active-filters (get idx) :one-of (or #{}))]
      [popup {:!expanded !expanded
              :on-close  #(reset! !input-text "")}
       [:button
        {:class ["block" "relative" "font-normal" "w-full" "cursor-default"
                 "rounded" "text-left" "shadow-sm" "ring-1" "ring-slate-300"
                 "pl-2" "pr-7" "py-0.5" "bg-white" "focus:outline-none"
                 "focus:ring-2" "focus:ring-blue-500" "sm:text-sm" "sm:leading-6"]}
        (if (empty? selected)
          [:span.block.truncate.text-slate-400 "Select..."]
          [:span.block.truncate (->> selected
                                     (map value->str)
                                     (str/join ", "))])
        [:span
         {:class (concat
                  ["pointer-events-none" "absolute" "inset-y-0" "right-1" "flex"
                   "items-center" "transition" "duration-75"]
                  (when @!expanded ["rotate-180"]))}
         [icon-chevron]]]
       [:ul.rounded.font-sans.bg-white.py-1.text-base.shadow-lg.border.border-slate-300.not-prose
        {:tabindex "-1"
         :role "listbox"
         :class ["focus:outline-none" "sm:text-sm"]}
        [:li {:class ["px-2" "pt-1" "pb-1.5"]}
         [input @!input-text #(reset! !input-text %)
          {:placeholder "Filter..."}]]
        (let [input-text (normalize-str @!input-text)
              values     (->> (:values filter-data)
                              (map #(vector % (value->str %) (-> % value->str normalize-str)))
                              (sort-by
                               (fn [[value _ normalized-str]]
                                 (if (= :nextjournal/missing value)
                                   "\uD7FF" ;; sort last
                                   normalized-str)))
                              (filter (fn [[_ _ normalized-str]] (str/index-of normalized-str input-text))))
              li-classes ["cursor-default" "select-none" "flex" "px-2" "py-0.5"
                          "gap-1.5" "sm:text-sm" "sm:leading-6"]]
          (if (empty? values)
            [:li {:role "option"
                  :class (concat li-classes ["text-slate-400"])}
             "-- No matches --"]
            (for [[value value-str _] values]
              [:li
               {:role "option"
                :class (concat li-classes ["hover:bg-slate-200"])
                :on-pointer-down
                (fn [e]
                  (if (some? (selected value))
                    (swap! table-state update-in [:active-filters idx :one-of] disj value)
                    (swap! table-state update-in [:active-filters idx :one-of] (fnil conj #{}) value))
                  (.preventDefault e))}
               [:span.flex.items-center
                [checkbox (some? (selected value))]]
               value-str])))]])))

(defn table-col-filter [{:as opts
                         :keys [filter-type]}]
  [:div
   (case filter-type
     :substring
     [table-col-filter-text :substring opts]

     :regexp
     [table-col-filter-text :regexp opts]

     :one-of
     [table-col-filter-multiselect opts])])

(defn icon-filter []
  [:svg {:viewBox "0 0 20 20"
         :fill "currentColor"
         :xmlns "http://www.w3.org/2000/svg"}
   [:path {:d "M18 1H2V3L10 13L18 3V1Z"}]
   [:path {:d "M8 5H12V18L8 15V5Z"}]])

(defn icon-checkmark []
  [:svg {:xmlns "http://www.w3.org/2000/svg"
         :fill "none"
         :viewBox "0 0 20 20"
         :stroke-width "1.5"
         :stroke "currentColor"
         :stroke-linecap "round"
         :stroke-linejoin "round"}
   [:path {:d "M5 10L9 14L15 5"}]])

(defn filter-button [table-state opts cell idx]
  (r/with-let [!expanded (r/atom false)]
    (let [filter (-> @table-state :active-filters (get idx) keys first)]
      [:div.absolute.right-1.size-4.text-slate-300.hover:text-slate-500
       [popup {:!expanded !expanded}
        [:div
         {:class [(when filter "text-slate-500")]}
         [icon-filter]]
        [:ul.rounded.font-sans.bg-white.py-1.text-base.shadow-lg.border.border-slate-300.not-prose
         (for [[type name] [[nil        "No filter"]
                            [:substring "Substring"]
                            [:regexp    "Regexp"]
                            [:one-of    "Multiselect"]]
               :let [selected? (= filter type)]]
           [:li.cursor-default.select-none.flex.items-center
            {:class ["pl-2" "pr-3" "py-0.5" "sm:text-sm" "sm:leading-6" "hover:bg-slate-200"]
             :on-click (fn [_]
                         (when-not selected?
                           (if type
                             (swap! table-state update :active-filters assoc idx {type nil})
                             (swap! table-state update :active-filters dissoc idx)))
                         (reset! !expanded false))}
            [:span.size-5.mr-1
             (when selected?
               [icon-checkmark])]
            name])]]])))

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
                         has-subheaders? (vector? header-cell)
                         idx (:idx cell)
                         k (if (vector? header-cell)
                             (first header-cell)
                             header-cell)
                         title (when (or (string? k) (keyword? k) (symbol? k)) k)
                         {:keys [translated-keys column-layout number-col?] :or {translated-keys {}}} opts]
                     [:th.text-slate-600.text-xs.px-1.py-1.bg-slate-100.first:rounded-md-tl.last:rounded-md-r.border-l.first:border-l-0.border-slate-300.text-center.whitespace-nowrap.border-b.align-top
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
                       [:div.relative.flex.justify-center
                        [:span.px-6 (get translated-keys k k)]
                        (when-not has-subheaders?
                          [filter-button table-state opts header-cell idx])]
                       (when-not has-subheaders?
                         [:<>
                          (when-some [col-filter (-> @table-state :active-filters (get idx) keys first)]
                            [table-col-filter {:filter-type col-filter
                                               :filter-data (get (:filter-data opts) idx)
                                               :table-state table-state
                                               :idx idx}])
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
                       col-filter     (get (:filters opts) cell)]
                   [:div.flex.flex-col.gap-1
                    [:div.relative.flex.justify-center
                     (get (:translated-keys opts {}) sub-header-key sub-header-key)
                     [filter-button table-state opts cell idx]]
                    (when-some [col-filter (-> @table-state :active-filters (get idx) keys first)]
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

(defn get-theme []
  (.theme EditorView
          (j/lit {"&" {:box-shadow "#CBD5E1 0 0 0 1px inset"
                       :border-radius "0.25rem"
                       :padding "0.125rem 0"}
                  "&.cm-focused" {:outline "none"
                                  :box-shadow "#3C82F6 0 0 0 2px inset"}
                  ".cm-line" {; :padding "0"
                              :line-height "1.25rem"
                              :font-size "0.875rem"
                              :font-family "Fira Sans, -apple-system, BlinkMacSystemFont, sans-serif"}})))

(defn on-change-ext [!view f]
  (.. EditorState -transactionExtender
      (of (fn [^js tr]
            (when (.-docChanged tr)
              (f (.. tr -state sliceDoc)))
            #js {}))))

(def theme
  (Compartment.))

(defn editor [{:keys [!text !state !view on-change on-view-update key-bindings]}]
  (r/with-let [!state (or !view (atom nil))
               !view  (or !view (atom nil))]
    (let [!container-el (hooks/use-ref nil)]
      (hooks/use-effect
       (fn []
         (let [extensions (cond-> #js [(.of theme (get-theme))
                                       (on-change-ext !view #(reset! !text %))]
                            on-change
                            (.concat
                             (on-change-ext !view on-change))
                            
                            on-view-update
                            (.concat
                             (.define ViewPlugin
                                      (fn [^js view]
                                        #js {:update (j/fn [update]
                                                       (js/setTimeout #(on-view-update view update) 0))})))

                            key-bindings
                            (.concat
                             (.of keymap (clj->js
                                          (for [[key callback] key-bindings]
                                            {:key key :run callback})))))
               state (.create EditorState #js {:doc @!text
                                               :extensions extensions})
               view  (EditorView. #js {:state state
                                       :parent @!container-el})]
           (reset! !state state)
           (reset! !view view)
           #(.destroy view))))
      [:div {:ref !container-el}])))

(defn find [pred xs]
  (reduce #(when (pred %2) (reduced %2)) nil xs))

(defn not-blank [s]
  (when-not (str/blank? s)
    s))

(defn matches [keys text #_prefix]
  (if (str/blank? text)
    keys
    (let [text (-> text str/lower-case str/trim)]
      (->>
       (concat
        (filter #(str/starts-with? (str/lower-case %) text) keys)
        #_(filter #(str/starts-with? % prefix) keys)
        (filter #(str/index-of (str/lower-case %) text) keys)
        #_(filter #(str/index-of % prefix) keys))
       distinct
       not-empty))))

(def filter-regexp
  (let [modifier  "(?<modifier>[+-])?"
        key-q     "'(?<key>[^']*)(?:'|$)"
        key-qq    "\"(?<key>[^\"]*)(?:\"|$)"
        key       "(?<key>[^\\s:]*)"
        delimeter "(?<delimeter>:)"
        value-q   "'(?<value>[^']*)(?:'|$)"
        value-qq  "\"(?<value>[^\"]*)(?:\"|$)"
        value     "(?<value>[^\\s]*)"]
    (js/RegExp. (str modifier
                     "(?<fullkey>" key-q "|" key-qq "|" key ")"
                     "(?:" delimeter
                     "(?<fullvalue>" value-q "|" value-qq "|" value ")"
                     ")?")
                "g")))

#_(doseq [modifier ["" "+" "-"]
          key      ["key" "'a key'" "'a:key'" "\"a key\"" "\"a:key\""]
          delim    [nil ":"]
          value    (if delim
                     [nil "value" "a:value" "'a value'" "'a:value'" "\"a value\"" "\"a:value\""]
                     [nil])
          :let     [s    (str modifier key delim value)]
          m        (.matchAll s filter-regexp)
          :let     [mod' (-> m .-groups .-modifier)
                    key' (-> m .-groups .-key)
                    val' (-> m .-groups .-value)]]
    (js/console.log s mod' key' val'))

(defn parse-query [s]
  (for [match (.matchAll s filter-regexp)
        :let [groups    (.-groups match)
              start     (.-index match)
              full      (aget match 0)
              modifier  (-> groups .-modifier not-blank)
              key       (-> groups .-key not-blank)
              delimeter (-> groups .-delimeter not-blank)
              value     (-> groups .-value not-blank)]]
    {:start-idx   start
     :modifier    modifier
     :key-quote   (when-some [fullkey (-> groups .-fullkey not-blank)]
                    (#{"'" "\""} (subs fullkey 0 1)))
     :key         key
     :value-idx   (when delimeter
                    (+ start
                       (count (or modifier ""))
                       (count (-> groups .-fullkey))
                       (count delimeter)))
     :value-quote (when-some [fullvalue (-> groups .-fullvalue not-blank)]
                    (#{"'" "\""} (subs fullvalue 0 1)))
     :value       value
     :end-idx     (+ start (count full))}))

(defn quote-key [s quote]
  (cond
    quote
    (str quote s quote)

    (or (str/index-of s " ")
        (str/index-of s ":"))
    (str "'" s "'")

    :else
    s))

(defn quote-value [s quote]
  (cond
    quote
    (str quote s quote)

    (str/index-of s " ")
    (str "'" s "'")

    :else
    s))

(defn calculate-suggestions [cells cell->column text pos ^js view opts]
  (let [regions (parse-query text)
        region (find (fn [{:keys [start-idx end-idx]}]
                       (and (< start-idx pos) (<= pos end-idx)))
                     regions)
        {:keys [start-idx modifier key key-quote value value-quote value-idx end-idx]} region
        key-column (cell->column key)
        
        [labels from-idx to-idx]
        (cond
          #_#_(and (nil? region) (str/blank? text))
          [(mapv #(str (quote-key % nil) ":") cells) pos pos]

          (nil? region)
          nil

          (and value-idx (>= pos value-idx) key-column)
          (let [values (-> opts :autocomplete-data (get key-column) :values (->> (map str) sort) (matches value))]
            [(mapv #(str (quote-value % value-quote) " ") values) value-idx end-idx])

          (and value-idx (>= pos value-idx) (nil? key-column))
          [[(str "No column “" key "”")] value-idx value-idx]

          :else
          (let [keys (matches cells key)]
            (if modifier
              [(mapv #(str modifier (quote-key % key-quote) " ") keys) start-idx (or value-idx end-idx)]
              [(mapv #(str (quote-key % key-quote) ":") keys) start-idx (or value-idx end-idx)])))]
    (when (seq labels)
      {:labels    labels
       :selected  0
       :from-idx  from-idx
       :to-idx    to-idx
       :right     (.-right (.coordsAtPos view from-idx))
       :bottom    (.-bottom (.coordsAtPos view from-idx))})))

(defn table-search [head+body table-state opts]
  (r/with-let [!text             (r/atom "")
               !state            (atom nil)
               !view             (atom nil)
               !suggestions      (r/atom nil)
               cells             (nextjournal.clerk.viewer/desc->values (first head+body))
               cells             (mapcat #(if (keyword? %)
                                            [(name %)]
                                            (for [sub (second %)]
                                              (str (name (first %)) "/" (name sub)))) cells)
               cell->column      (into {} (map vector cells (range)))
               accept-suggestion (fn [view selected]
                                   (when selected
                                     (when-some [{:keys [from-idx to-idx labels]} @!suggestions]
                                       (let [label  (nth labels selected)
                                             text'  (str
                                                     (subs @!text 0 from-idx)
                                                     label
                                                     (subs @!text to-idx (count @!text)))
                                             sel'   (+ from-idx (count label))
                                             update #js {:changes #js [#js {:insert label
                                                                            :from   from-idx
                                                                            :to     to-idx}]
                                                         :selection #js {:anchor sel'
                                                                         :head   sel'}}]
                                         (reset! !text text')
                                         (.dispatch view (.update (.-state view) update))))))]
    (let [!div (hooks/use-ref nil)]
      [:div.mb-2
       {:ref !div
        :style {:min-height 35}}
       [editor
        {:!text  !text
         :!state !state
         :!view  !view
         :on-change
         (fn [text']
           (swap! table-state assoc :search-query text'))
         :on-view-update
         (fn [view update]
           (let [range (-> update .-state .-selection .-main)]
             (if (and
                  (.-hasFocus view)
                  (.-empty range))
               (reset! !suggestions (calculate-suggestions cells cell->column @!text (.-to range) view opts))
               (reset! !suggestions nil))))
         :key-bindings
         {"ArrowUp"   (fn [view]
                        (when-some [suggestions @!suggestions]
                          (swap! !suggestions update :selected
                                 #(-> % (or (count (:labels suggestions))) dec (max 0)))
                          true))
          "ArrowDown" (fn [view]
                        (when-some [suggestions @!suggestions]
                          (swap! !suggestions update :selected
                                 #(-> % (or -1) inc (min (dec (count (:labels suggestions))))))
                          true))
          "Enter"     (fn [view]
                        (accept-suggestion view (:selected @!suggestions))
                        true)
          "Tab"       (fn [view]
                        (accept-suggestion view (:selected @!suggestions))
                        true)
          "Escape"    (fn [view]
                        (when-some [suggestions @!suggestions]
                          (reset! !suggestions nil)
                          true))}}]
       (when-some [{:keys [bottom right from-idx to-idx labels selected]} @!suggestions]
         (react-dom/createPortal
          (r/as-element
           [:div
            {:class ["rounded-b" "font-sans" "bg-white" "py-1" "text-base"
                     "shadow-lg" "border" "border-slate-300" "border-t-white"
                     "not-prose" "absolute" "z-10" "overflow-y-scroll" "shadow-xl"]
             :style {:left       (- (adjust-offset-x right) 7)
                     :top        (-> (offset @!div) :top (+ (.-offsetHeight @!div) #_(- 3)))
                     :max-height "50svh"}}
            [:ul
             (for [[idx label] (map vector (range) labels)]
               [:li {:class
                     ["px-[6px]" "py-0.5" "sm:text-sm" "sm:leading-6"
                      (when (= idx selected) "bg-slate-200")]
                     :on-mouse-over
                     (fn [_]
                       (swap! !suggestions assoc :selected idx))
                     :on-click
                     (fn [_]
                       (accept-suggestion @!view idx))}
                label])]])
          js/document.body))])))

(defn table-markup-viewer [head+body {:as opts :keys [sync-var]}]
  (reagent.core/with-let [table-state (if sync-var
                                        (deref sync-var)
                                        #?(:clj (throw (js/Error. (str "no sync var: " sync-var)))
                                           :cljs nil))]
    [:div
     [table-search head+body table-state opts]
     [:div.bg-white.rounded.border.border-slate-300.shadow-sm.font-sans.text-sm.not-prose.overflow-x-auto
      {:class "print:overflow-none print:text-[10px] print:shadow-none print:rounded-none print:border-none"}

      (into
       [:table.w-full]
       (nextjournal.clerk.render/inspect-children (assoc opts :table-state table-state))
        ;; debug atom+head+body #_
       head+body)]]))

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
