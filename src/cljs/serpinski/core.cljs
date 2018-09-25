(ns serpinski.core
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [cljsjs.three]
            [cljsjs.bootstrap]
            [re-com.core :as re-com]
            [clojure.string :as string])
  (:require-macros [reagent.ratom :refer [reaction]]))

;; -------------------------
;; Views
;
(defn a-better-mouse-trap [mouse]
      (let [mice (reagent/atom 1)]
           (fn render-mouse-trap [mouse]
               (into
                 [:div
                  [:button.btn.btn-secondary.mx-auto
                   {:on-click
                    (fn [e]
                        (swap! mice (fn [m] (inc (mod m 4)))))}
                   "Catch!"]]
                 (repeat @mice mouse)))))

(defn lambda [rotation x y]
      [:g {:transform (str "translate(" x "," y ")"
                           "rotate(" rotation ") ")}
       [:circle {:r 50, :fill "green"}]
       [:circle {:r 25, :fill "blue"}]
       [:path {:stroke-width 12
               :stroke "white"
               :fill "none"
               :d "M -45,-35 C 25,-35 -25,35 45,35 M 0,0 -45,45"}]])

(defn spinnable []
      (reagent/with-let [rotation (reagent/atom 0)]
                        [:svg
                         {:width 150 :height 150
                          :on-mouse-move
                                 (fn [e]
                                     (swap! rotation + 30))}
                         [lambda @rotation 75 75]]))

(defn several-spinnables []
      [:div
       [:h3 "Move your mouse over me"]
       [a-better-mouse-trap [spinnable]]])

(defn announcement []
      (reagent/create-class
        {:reagent-render
         (fn []
             [:h3 "I for one welcome our new insect overlords."])}))

(defn mouse-position []
      (reagent/with-let [pointer (reagent/atom nil)
                         handler (fn [e]
                                     (swap! pointer assoc
                                                      :x (.-pageX e)
                                                      :y (.-pageY e)))
                         _ (js/document.addEventListener "mousemove" handler)]
                        [:div "Pointer moved to: " (str @pointer)]
                        (finally
                          (js/document.removeEventListener "mousemove" handler))))
;
(defn create-renderer [element]
      (doto (js/THREE.WebGLRenderer. #js {:canvas element :antialias true})
            (.setPixelRatio js/window.devicePixelRatio)))
;
(defn three-canvas [attributes camera scene tick]
      (let [requested-animation (atom nil)]
           (reagent/create-class
             {:display-name "three-canvas"
              :reagent-render
                            (fn three-canvas-render []
                                [:canvas attributes])
              :component-did-mount
                            (fn three-canvas-did-mount [this]
                                (let [e (reagent/dom-node this)
                                      r (create-renderer e)]
                                     ((fn animate []
                                          (tick)
                                          (.render r scene camera)
                                          (reset! requested-animation (js/window.requestAnimationFrame animate))))))
              :component-will-unmount
                            (fn [this]
                                (js/window.cancelAnimationFrame @requested-animation))})))

(defn create-scene []
      (doto (js/THREE.Scene.)
        (.add (js/THREE.AmbientLight. 0x888888))
        (.add (doto (js/THREE.DirectionalLight. 0xffff88 0.5)
                (-> (.-position) (.set -600 300 600))))
        (.add (js/THREE.AxisHelper. 50))))

(defn mesh [geometry color]
      (js/THREE.SceneUtils.createMultiMaterialObject.
        geometry
        #js [(js/THREE.MeshBasicMaterial. #js {:color color :wireframe true})
             (js/THREE.MeshLambertMaterial. #js {:color color})]))

(defn fly-around-z-axis [camera scene]
      (let [t (* (js/Date.now) 0.0002)]
           (doto camera
             (-> (.-position) (.set (* 100 (js/Math.cos t)) (* 100 (js/Math.sin t)) 100))
             (.lookAt (.-position scene)))))

(defn v3 [x y z]
      (js/THREE.Vector3. x y z))

(defn lambda-3d []
      (let [camera (js/THREE.PerspectiveCamera. 45 1 1 2000)
            curve (js/THREE.CubicBezierCurve3.
                    (v3 -30 -30 10)
                    (v3 0 -30 10)
                    (v3 0 30 10)
                    (v3 30 30 10))
            path-geometry (js/THREE.TubeGeometry. curve 20 4 8 false)
            scene (doto (create-scene)
                        (.add
                          (doto (mesh (js/THREE.CylinderGeometry. 40 40 5 24) "green")
                                (-> (.-rotation) (.set (/ js/Math.PI 2) 0 0))))
                        (.add
                          (doto (mesh (js/THREE.CylinderGeometry. 20 20 10 24) "blue")
                                (-> (.-rotation) (.set (/ js/Math.PI 2) 0 0))))
                        (.add (mesh path-geometry "white")))
            tick (fn []
                     (fly-around-z-axis camera scene))]
           [three-canvas {:width 150 :height 150} camera scene tick]))

(def pyramid-points
  [[-0.5 -0.5 0 "#63B132"] [-0.5 0.5 0 "#5881D8"] [0.5 0.5 0 "#90B4FE"] [0.5 -0.5 0 "#91DC47"] [0 0 1 "white"]])

(defn add-pyramid [scene x y z size color]
  (.add scene
        (doto
          (let [g (js/THREE.Geometry.)]
            (set! (.-vertices g)
                  (clj->js (for [[i j k] pyramid-points]
                             (v3 i j k))))
            (set! (.-faces g)
                  (clj->js (for [[i j k] [[0 1 2] [0 2 3] [1 0 4] [2 1 4] [3 2 4] [0 3 4]]]
                             (js/THREE.Face3. i j k))))
            (mesh g color))
          (-> (.-position) (.set x y z))
          (-> (.-scale) (.set size size size)))))

(defn add-pyramids [scene x y z size color]
  (if (< size 4)
    (add-pyramid scene x y z (* size 1.75) color)
    (doseq [[i j k color] pyramid-points]
      (add-pyramids scene
                    (+ x (* i size))
                    (+ y (* j size))
                    (+ z (* k size))
                    (/ size 2)
                    color))))

(defn gasket-3d []
  (let [camera (js/THREE.PerspectiveCamera. 45 1 1 2000)
        scene (doto (create-scene)
                (add-pyramids 0 0 0 32 "white"))
        tick (fn [] (fly-around-z-axis camera scene))]
    [three-canvas {:width 640 :height 640} camera scene tick]))

;(defn puppy [x]
;  (.log "puppy created, x:" x)
;  (let [mouse-over? (reagent/atom false)]
;    (fn [y]
;      (.log "puppy rendered, x:" x " y:" y " mouse-over?:" @mouse-over?)
;      [:span {:on-mouse-over (fn [e] (reset! mouse-over? true))
;              :on-mouse-out (fn [e] (reset! mouse-over? false))}
;       [:img {:src "https://goo.gl/fMzXOU"
;              :style {:width "150px",
;                      :border "1px solid",
;                      :transform (str "scale(" (if @mouse-over? 1.1 1) ")")}}]])))
;
;(defn lifecycle-review []
;  (reagent/with-let [x (reagent/atom "1")]
;                    [:div
;                     [:label "Type in a value for x: "
;                      [:input {:on-change (fn [e] (reset! x (.. e -target -value)))}]]
;                     [with-log [a-better-mouse-trap [puppy @x]]]]))

(def favorites (reagent/atom {"d632" {:name "Princess Torte."
                                      :order 2}
                              "1ae2" {:name "Black forest gateau."
                                      :order 3}
                              "5117" {:name "Apple Pie."
                                      :order 4}
                              "42ae" {:name "Ice cream."
                                      :order 1}}))

(defn list-by [entities sort-k]
  [:ul
   (for [[k v] (sort-by (comp sort-k val) @entities)]
     ^{:key k}
     [:li (:name v)])])

(defn favorites-by-order-and-name []
  [:div
   [:h3 "By order"]
   [list-by favorites :order]
   [:h3 "By name"]
   [list-by favorites :name]])

(def words
  ["ice" "cream" "chocolate" "pastry" "pudding" "raspberry" "mousse"
   "vanilla" "wafer" "waffle" "cake" "torte" "gateau" "pie" "cookie"
   "cupcake" "mini" "hot" "caramel" "meringue" "lemon" "marzipan" "mocha"
   "strawberry" "tart" "custard" "fruit" "baklava" "jelly" "banana" "coconut"])

(defn rand-name []
  (string/capitalize (string/join " " (take (+ 2 (rand-int 5)) (shuffle words)))))

(def desserts (reagent/atom ()))

(defn make-a-dessert [e]
  (swap! desserts conj {:id (random-uuid)
                        :name (rand-name)}))

(defn make-many-desserts [e]
  (dotimes [i 100]
    (make-a-dessert nil)))

(defn color-for [x]
  (str "#" (.toString (bit-and (hash x) 0xFFFFFF) 16)))

(defn dessert-item [{:keys [id name]}]
  [:li
   [:svg {:width 50 :height 50}
    [:circle
     {:r 20 :cx 25 :cy 25 :fill (color-for id)}]
    [:rect {:x 15 :y 15 :width 20 :height 20 :fill (color-for name)}]]
   [:span [:em [:strong name]]]])

(defn desserts-list []
  [:ol
   (for [dessert @desserts] ^{:key (:id dessert)} [dessert-item dessert])])

(defn dessertinator []
  [:div
   [:button.btn.btn-primary {:on-click make-a-dessert} "Invent a new dessert"]
   [:button.btn.btn-danger {:on-click make-many-desserts} "Invent 100 new desserts"]
   [desserts-list]])


(def my-drawing (reagent/atom []))

(swap! my-drawing conj [50 100 150 100])

(defn scribble1 [drawing]
  (into
    [:svg
     {:width "100%"
      :height 200
      :stroke "black"
      :fill "none"}]
    (for [[x y & more-points] @drawing]
      [:path {:d (str "M " x " " y " L " (string/join " " more-points))}])))

(defn scribble2 [attrs drawing]
  (into
    [:svg
     (merge-with merge attrs
                 {:width "100%"
                  :height 200
                  :stroke "black"
                  :stroke-width 4
                  :fill "none"
                  :style {:border "1px solid"
                          :box-sizing "border-box"
                          :cursor "crosshair"}})]
    (for [[x y & more-points] @drawing]
      [:path {:d (str "M " x " " y " L " (string/join " " more-points))}])))

(def my-drawing2 (reagent/atom [[50 100 75 150 100 100 125 150 150 100]]))

(defn xy [e]
  (let [rect (.getBoundingClientRect (.-target e))]
    [(- (.-clientX e) (.-left rect))
     (- (.-clientY e) (.-top rect))]))


(defn mouse-handlers [drawing]
  (let [pen-down? (reagent/atom false)
        start-path
        (fn start-path [e]
          (when (not= (.-buttons e) 0)
            (reset! pen-down? true)
            (let [[x y] (xy e)]
              (swap! drawing conj [x y x y]))))
        continue-path
        (fn continue-path [e]
          (when @pen-down?
            (let [[x y] (xy e)]
              (swap! drawing (fn [lines]
                               (let [last-line-idx (dec (count lines))]
                                 (update lines last-line-idx conj x y)))))))
        end-path
        (fn end-path [e]
          (when @pen-down?
            (continue-path e)
            (reset! pen-down? false)))]
    {:on-mouse-down start-path
     :on-mouse-over start-path
     :on-mouse-move continue-path
     :on-mouse-up end-path
     :on-mouse-out end-path}))

(def my-drawing3 (reagent/atom []))

(defn optional-title []
  (let [show? (reagent/atom false)]
    (fn []
      [:div
       [:button.btn.btn-primary
        {:on-click
         (fn [e]
           (swap! show? not))}
        (if @show? "Hide" "Add a title")]
       (when @show?
         [:input {:auto-focus true}])])))

(defn paths [drawing]
  (into
    [:g
     {:style {:pointer-events "none"}
      :fill "none"
      :stroke "black"
      :stroke-width 4}]
    (for [[x y & more-points] @drawing]
      [:path {:d (str "M " x " " y "L " (string/join " " more-points))}])))

(defn scribble3 [attrs drawing]
  [:svg
   (merge-with merge attrs
               {:width "100%"
                :height 400
                :style {:border "1px solid"
                        :box-sizing "border-box"
                        :cursor "crosshair"}})
   [paths drawing]])

(def my-drawing4 (reagent/atom []))

(defn scribble-widget []
  (let [a-drawing (reagent/atom [])
        handlers (mouse-handlers a-drawing)]
    [scribble3 handlers a-drawing]))

(defn button-maker []
  [:button.btn.btn-danger
   {:on-click
    (fn [e]
      (js/alert "You pressed a button"))}
   "Do not press"])

(def rolls (reagent/atom [1 2 3 4]))
(def sorted-rolls (reagent.ratom/reaction (sort @rolls)))

(defn sorted-d20 []
  [:div
   [:button.btn.btn-primary {:on-click (fn [e] (swap! rolls conj (rand-int 20)))} "Roll!"]
   [:p (pr-str @sorted-rolls)]
   [:p (pr-str (reverse @sorted-rolls))]])

(defn navbar-maker []
  [:nav.navbar.navbar-expand-lg.navbar-dark.bg-dark
   [:a.navbar-brand {:href "#"} "ThreeJS/REAGENT"]
   [:button.navbar-toggler
    {:aria-label "Toggle navigation",
     :aria-expanded "false",
     :aria-controls "navbarSupportedContent",
     :data-target "#navbarSupportedContent",
     :data-toggle "collapse",
     :type "button"}
    [:span.navbar-toggler-icon]]
   [:div#navbarSupportedContent.collapse.navbar-collapse
    [:ul.navbar-nav.mr-auto
     [:li.nav-item.active
      [:a.nav-link {:href "/about"} "About " [:span.sr-only "(current)"]]]
     [:li.nav-item.active
      [:a.nav-link {:href "/three"} "Third " [:span.sr-only "(current)"]]]
     [:li.nav-item.active
      [:a.nav-link {:href "/scribble"} "Scribble " [:span.sr-only "(current)"]]]]
    [:form.form-inline.my-2.my-lg-0
     [:input.form-control.mr-sm-2
      {:aria-label "Search", :placeholder "Search", :type "search"}]
     [:button.btn.btn-outline-success.my-2.my-sm-0
      {:type "submit"}
      "Search"]]]])

;PAGES ============================================

(defn home-page []
  [:div.container-fluid
   [navbar-maker]
   [:div.row {:style {:padding-top "70px"}}
    [:div.col-sm-3
     [sorted-d20]]
    [:div.col-sm-3
     [a-better-mouse-trap [lambda-3d]]]
    [:div.col-sm-3
     [button-maker]]
    [:div.col-sm-3
     [a-better-mouse-trap [spinnable]]]]
   [:div.row {:style {:padding-top "70px"}}
    [:div.col-sm-3]
    [:div.col-md-6
     [gasket-3d]]
    [:div.col-sm-3]]])


(defn about-page []
  [:div.container-fluid
   [navbar-maker]
   [:div [:h2 {:style {:padding-top "70px"}} "Welcome to serpinski"]
     [:h3 "Hello world"]
    [:div
     [a-better-mouse-trap
      [:img
       {:src   "https://www.domyownpestcontrol.com/images/content/mouse.jpg"
        :style {:width "150px" :border "1px solid"}}]]
     [a-better-mouse-trap
      [:img
       {:src   "https://avatars1.githubusercontent.com/u/9254615?v=3&s=150"
        :style {:border "1px solid"}}]]]
    [several-spinnables]
    [announcement]
    [mouse-position]]])

(defn three-page []
  [:div.container-fluid
   [navbar-maker]
   [:div [:h1 {:style {:padding-top "70px"}} "The 3rd page"]
    [favorites-by-order-and-name]
    [dessertinator]]])

(defn scribble-page []
  [:div.container-fluid
     [navbar-maker]
   [:div [:h1 {:style {:padding-top "70px"}} "Scribble page"]
    [scribble1 my-drawing]
    [scribble2 {:style {:background-color "lightgreen"}} my-drawing2]
    [:div
     [optional-title]]
    [:div
     [scribble2 (mouse-handlers my-drawing3) my-drawing3]
     [:h3 "Scribble on ME"]]
    [:div
     [scribble3 (mouse-handlers my-drawing4) my-drawing4]]
    [:div
     [a-better-mouse-trap [:div [scribble-widget]]]]]])


;; -------------------------
;; Routes

(defonce page (atom #'home-page))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'home-page))

(secretary/defroute "/about" []
  (reset! page #'about-page))

(secretary/defroute "/three" []
  (reset! page #'three-page))

(secretary/defroute "/scribble" []
  (reset! page #'scribble-page))
;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
