(ns cljc.graphs.core
  (:require [clojure.string :as st]))

(defn round-to-decimal
  [n decimals]
  (let [a (reduce * (repeat decimals 10))]
    (-> n (* a) (#?(:clj  #(Math/round %)
                    :cljs #(js/Math.round %))
                  (/ a)))))

(defn draw-axis
  [{:keys [x-min x-max y-min y-max title x-label y-label x-tick y-tick
           x-tick-values y-tick-values y-label-orientation width height]}
   {:keys [origin-x origin-y axis-width axis-height]}
   {:keys [x-txt-padding x-axis-font-size y-txt-padding y-axis-font-size]}]
  (let [x-tick (cond
                 x-tick x-tick
                 (and (< (count x-tick-values) 10)
                      (not (nil? x-tick-values))) (count x-tick-values)
                 :else 10)
        y-tick (cond
                 y-tick y-tick
                 (and (< (count y-tick-values) 10)
                      (not (nil? y-tick-values))) (count y-tick-values)
                 :else 10)
        x-tick-values (or x-tick-values (mapv #(-> x-max (/ (dec x-tick)) (* %) (round-to-decimal 2))
                                              (range x-tick)))
        y-tick-values (or y-tick-values (mapv #(-> y-max (/ (dec y-tick)) (* %) (round-to-decimal 0))
                                              (range y-tick)))]
    ;määrritellään ne muuttujat svg:ssä, jotka on pakko
    (seq
      [
       ^{:key "axis-def"}
       [:defs
        [:marker {:id "markerArrow" :markerWidth 20 :markerHeight 15
                  :refX 0 :refY 10 :orient "auto"}
         [:path {:d "M0,10 L0,15 L20,10 L0,5 L0,10"}]]]
       ^{:key "diagram-axis"}
       [:g {:stroke "black" :stroke-width 0.5}
        ;x-axis
        [:line {:x1 origin-x :y1 origin-y
                :x2 (+ axis-width origin-x) :y2 origin-y :markerEnd "url(#markerArrow)"}]
        ;y-axis
        [:line {:x1 origin-x :y1 origin-y
                :x2 origin-x :y2 (- origin-y axis-height) :markerEnd "url(#markerArrow)"}]
        ;x-ticks
        (mapcat #(let [x-tick-space (/ axis-width x-tick)
                       x-position (-> x-tick-space (* %) (+ origin-x))
                       x-tick-value (get x-tick-values %)]
                   [^{:key (str % "-tick")}
                   [:line {:x1 x-position :y1 origin-y
                           :x2 x-position :y2 (- origin-y 4)}]
                    ^{:key (str % "-txt")}
                    [:text {:x x-position :y (+ origin-y x-txt-padding x-axis-font-size)
                            :text-anchor "middle" :font-size (str x-axis-font-size)}
                     x-tick-value]])
                (range x-tick))
        ;y-ticks
        (mapcat #(let [y-tick-space (/ axis-height y-tick)
                       y-position (->> y-tick-space (* %) (- axis-height) (+ 20))
                       y-tick-value (get y-tick-values %)]
                   [^{:key (str % "-tick")}
                   [:line {:x1 origin-x :y1 y-position
                           :x2 (+ origin-x 4) :y2 y-position}]
                    ^{:key (str % "-txt")}
                    [:text {:x (- origin-x y-txt-padding y-axis-font-size) :y y-position
                            :text-anchor "middle" :font-size (str y-axis-font-size)}
                     y-tick-value]])
                (range y-tick))]])))

(defn axis-points
  [values x-ratio y-ratio origin-x origin-y]
  (st/trim
    (apply str (map #(str (+ origin-x (* x-ratio (:x %))) ","
                          (- origin-y (* y-ratio (:y %2))) " ")
                    values))))

(defn style-map
  [{:keys [stroke stroke-width]}]
  [:style {:stroke stroke
           :stroke-width stroke-width}])

(defn vector-disj
  [vector index]
  (vec
    (concat (subvec vector 0 index)
            (subvec vector (inc index)))))

(defn legend-sizes
  "Gives a map of sizes relevant to a legend. If some value is not given by
   the user, gives some default value"
  [{:keys [legend-font-size marker-width legendbox-thickness legendbox-padding
           legend-label-x legend-label-y max-legend-height n-of-legend-columns]}]
  {:legend-font-size (or legend-font-size 10)
   :legendbox-thickness (or legendbox-thickness 15)
   :legendbox-padding (or legendbox-padding 10)
   :legend-label-x (or legend-label-x 20)
   :legend-label-y (or legend-label-y 11)
   :max-legend-height (or max-legend-height 100)
   :n-of-legend-columns (or n-of-legend-columns 1)})

(defn axes-sizes
  "Gives a map of sizes relevant to axes. If some value is not given by
   the user, gives some default value"
  [{:keys [x-axis-font-size y-axis-font-size marker-width
           x-txt-padding y-txt-padding x-label-space y-label-space]}]
  {:x-axis-font-size (or x-axis-font-size 10)
   :y-axis-font-size (or y-axis-font-size 10)
   :marker-width (or marker-width 20)
   :x-txt-padding (or x-txt-padding 4)
   :y-txt-padding (or y-txt-padding 10)
   :x-label-space (or x-label-space 20)
   :y-label-space (or y-label-space 30)})

(defn canvas-points
  [{:keys [x-axis-font-size y-axis-font-size marker-width x-txt-padding y-txt-padding
           x-label-space y-label-space]}
   {:keys [legendbox-thickness legendbox-padding
           max-legend-height n-of-legend-columns]}
   lines width height]
  (let [n-of-legend-rows (count (partition-all n-of-legend-columns lines))
        legend-area-true-height (* (+ legendbox-padding legendbox-thickness)
                                   n-of-legend-rows)
        legend-area-height (if (> legend-area-true-height max-legend-height)
                             max-legend-height legend-area-true-height)
        axis-width (- width y-label-space y-txt-padding y-axis-font-size marker-width)
        axis-height (- height legend-area-height x-label-space x-txt-padding x-axis-font-size marker-width)
        origin-x (+ y-label-space y-axis-font-size y-txt-padding)
        origin-y (+ axis-height marker-width)
        legend-area-top-x origin-x
        legend-area-top-y (+ origin-y x-txt-padding x-axis-font-size x-label-space)]
    {:legend-area-top-x legend-area-top-x
     :legend-area-top-y legend-area-top-y
     :legend-area-height legend-area-height
     :legend-area-true-height legend-area-true-height
     :axis-width axis-width
     :axis-height axis-height
     :origin-x origin-x
     :origin-y origin-y}))


(defn draw-legend
  [vals
   {:keys [legend-label-x legend-label-y legendbox-thickness legendbox-padding
           max-legend-height legend-font-size]}
   {:keys [legend-area-top-y legend-area-top-x legend-area-height
           legend-area-true-height]}
   {:keys [clicked-fn mouse-enter-fn mouse-leave-fn n-of-legend-columns scroll-bars?
           width height]}]
  (let [n-of-legend-columns (or n-of-legend-columns 1)
        column-width (/ width n-of-legend-columns)
        vals-partioned (partition-all n-of-legend-columns vals)
        legend-svg-elements (apply concat
                                   (map-indexed
                                     #(map-indexed (fn [column-index val]
                                                     (let [rect-x-val (+ legend-area-top-x (* column-width column-index))
                                                           rect-y-val (+ legend-area-top-y
                                                                         (* (+ legendbox-thickness legendbox-padding) %1))
                                                           val-index (dec (* (inc column-index) (inc %1)))]
                                                       ^{:key (gensym "legend-")}
                                                       [:g {:on-click (clicked-fn val-index)
                                                            :on-mouse-enter (mouse-enter-fn val-index)
                                                            :on-mouse-leave (mouse-leave-fn val-index)}
                                                        [:rect {:x rect-x-val
                                                                :y rect-y-val
                                                                :height legendbox-thickness
                                                                :width legendbox-thickness
                                                                :fill (if-let [hover-fill (get-in val [:style :legend-hover-fill])]
                                                                        hover-fill
                                                                        (get-in val [:style :fill]))}]
                                                        [:text {:x (+ legend-label-x rect-x-val)
                                                                :y (+ legend-label-y rect-y-val)
                                                                :font-size legend-font-size}
                                                         (if-let [label (:label val)]
                                                           label "line")]]))
                                                   %2)
                                     vals-partioned))]
    (if (and scroll-bars? (= legend-area-height max-legend-height))
      [:foreignObject {:width width
                       :height legend-area-height
                       :y legend-area-top-y}
       [:div {:style {:xmlns "http://www.w3.org/1999/xhtml"
                      :overflow "scroll"
                      :width width
                      :height max-legend-height}}
        [:svg {:xmlns "http://www.w3.org/2000/svg"
               :width width
               :height legend-area-true-height
               :view-box (str 0 " " legend-area-top-y " " width " " max-legend-height)}
         legend-svg-elements]]]
      legend-svg-elements)))

(defn draw-axis-and-legend
  [vals clicked-fn mouse-enter-fn mouse-leave-fn
   {:keys [axes-sizes-comp legend-sizes-comp
           canvas-points-comp options-comp] :as computed-parameters}]
  (let [legend-params (merge options-comp {:clicked-fn clicked-fn :mouse-enter-fn mouse-enter-fn
                                           :mouse-leave-fn mouse-leave-fn})]
    [:g
     (draw-axis options-comp canvas-points-comp axes-sizes-comp)
     (draw-legend vals legend-sizes-comp canvas-points legend-params)]))

(defn index-of
  [vektori elementti]
  #?(:clj  (.indexOf vektori elementti)
     :cljs (.indexOf (to-array vektori) elementti)))

(defn str->float
  [txt]
  #?(:clj  (Float. txt)
     :cljs (js/parseFloat txt)))

(defn distinct-values
  "Used to draw distinct values form either of the axis"
  [lines axis]
  (sequence (comp
              (mapcat :values)
              (map axis)
              (distinct))
            lines))

(defn graph-parameters
  [{:keys [width height x-max y-max x-min y-min] :as options} lines]
  (let [x-number? (-> lines first :values first :x number?)
        y-number? (-> lines first :values first :y number?)
        x-tick-values (or (:x-tick-values options)
                          (if x-number?
                            nil (distinct-values lines :x)))
        axs (axes-sizes options)
        ls (legend-sizes options)
        {:keys [axis-width axis-height] :as cp} (canvas-points axs ls lines width height)
        x-max (or x-max (apply max (distinct-values lines :x)))
        y-max (or y-max (apply max (distinct-values lines :y)))
        x-min (or x-min 0)
        y-min (or y-min 0)
        x-ratio (/ axis-width x-max)
        y-ratio (/ axis-height y-max)]
    {:axes-sizes-comp axs
     :legend-sizes-comp ls
     :canvas-points-comp cp
     :options-comp (merge options {:x-tick-values x-tick-values
                                   :x-min x-min
                                   :x-max x-max
                                   :x-max x-max
                                   :y-max y-max
                                   :x-ratio x-ratio
                                   :y-ratio y-ratio})}))

(defmulti plot
          (fn [{plot-type :plot-type}]
            (identity plot-type)))

(defmethod plot :line-plot
  [options lines]                                           ;; [{:values [{:x 1 :y 2} ...] :color "blue"} ...]
  (let [state (atom lines)
        info-map (atom nil)]
    (fn [{:keys [width height clicked hovered] :as options} lines]
      [:svg#container-svg [:xmlns "http://www.w3.org/2000/svg"]
       (let [{:keys [axes-sizes-comp legend-sizes-comp
                     canvas-points-comp options-comp] :as computed-parameters} (graph-parameters options @state)
             {:keys [x-ratio y-ratio]} options-comp
             {:keys [origin-x origin-y axis-height]} canvas-points-comp
             {:keys [x-tick-values]} options-comp
             points (mapv #(axis-points (:values %) x-ratio y-ratio origin-x origin-y)
                          @state)
             action-fn (fn [style-map index add?]
                         (if (= style-map :delete)
                           (reset! vals (vector-disj @vals index))
                           (if add?
                             (swap! vals update-in [index :style] #(merge % style-map))
                             (swap! vals update-in [index :style] #(apply dissoc % (keys style-map))))))
             clicked-fn (fn [index]
                          (fn [event] (action-fn clicked index false)))
             mouse-enter-fn (fn [index]
                              (fn [event] (action-fn hovered index true)))
             mouse-leave-fn (fn [index]
                              (fn [event] (action-fn hovered index false)))]
         [:g
          (draw-axis-and-legend @state clicked-fn mouse-enter-fn mouse-leave-fn computed-parameters)
          [:g
           [:text {:x (+ origin-x 15)
                   :y (- origin-y axis-height)
                   :font-size 7}
            (str (:x @info-map) " " (:y @info-map) " " (:label @info-map))]]
          ;draw lines
          (doall
            (map-indexed #(identity
                            ^{:key (gensym "plot-line-")}
                            [:g.klikattava
                             {:stroke "black" :stroke-width 1
                              :pointer-events "painted"
                              :on-mouse-over #?(:cljs (fn [evt] ;;TODO Tämä funktion vois tehdä tehokkaamminkin. Pointsit on järjestyksessä.
                                                        (let [svg-left (.-left (.getBoundingClientRect (.getElementById js/document "container-svg")))
                                                              m-pos-x (- (.-pageX evt) svg-left)
                                                              the-points (st/split (get points %1) #" ")
                                                              point-of-interest (first (filter (fn [xy-point]
                                                                                                 (let [x-point (second (re-find #"(.+)," xy-point))
                                                                                                       x-point-float (str->float x-point)]
                                                                                                   (> x-point-float m-pos-x)))
                                                                                               the-points))
                                                              point-index (index-of the-points point-of-interest)]
                                                          (swap! info-map assoc :x (get x-tick-values point-index)
                                                                 :y (get-in %2 [point-index :values :y])
                                                                 :label (:label %2))))
                                                :clj  nil)
                              :on-click (clicked-fn %1)
                              :on-mouse-enter (mouse-enter-fn %1)
                              :on-mouse-leave (mouse-leave-fn %1)}
                             (if (= 1 (count (:values %2)))
                               (do
                                 [:circle {:cx (str->float (second (re-find #"(.+)," (get points %1))))
                                           :cy (str->float (second (re-find #",(.+)" (get points %1))))
                                           :r 2
                                           :fill (get-in %2 [:style :fill])}])
                               [:polyline (merge {:points (get points %1)
                                                  :fill "none"}
                                                 (style-map (:style %2)))])])
                         @state))])])))