(ns arp-sigils.draw
  (:require [arp-sigils.glyphs :as g]
            [clojure2d.core :as c2d]
            [clojure2d.color :as color]
            [clojure2d.pixels :as pix]
            [fastmath.core :as fm]
            ))

(def window)

(def state (atom {:mode :sigil
                  :addr "94:c6:91:16:48:49"
                  :exception nil
                  :exception-timeout 0}))
(def errstate (atom {}))

(def stroke 3)

(defn reset-state []
  (reset! state {:mode :sigil
                :addr "94:c6:91:16:48:49"
                :exception nil
                :exception-timeout 0}))

(def canvas (c2d/canvas 800 400))

(defn draw-strokes [canref strokes]
                   (doseq [part (:parts strokes)
                         :let [kind   (first part)
                               params (vec (rest part))
                               ;_ (println kind params)
                               ]] 
                     (case kind
                       :line  (apply (partial c2d/line canref) params)
                       :arc   (apply (partial c2d/arc canref) params)
                       :point (do 
                                (c2d/set-stroke canref (* 2 stroke))
                                (c2d/point canref params)
                                (c2d/set-stroke canref  stroke)
                                )
                       )
                     )
                   (:attach strokes)
  )


(defn draw-glyph [canref glyph]
  (let [{:keys [parts width in out outglyph subglyphs attach]} glyph
        [inx iny] in
        ]
    (c2d/translate canref (* 0.5 width) 0)
    (c2d/set-color canref :red)
    (c2d/set-stroke canref 0.5)
    ;(c2d/arc canref 0 0 20 20 0 fm/TWO_PI)
    (c2d/set-color canref :green)
    (c2d/set-stroke canref stroke)
    (doseq [part parts
            :let [kind (first part)
                  params (rest part)]]
      (case kind
        :line  (apply (partial c2d/line canref) params)
        :point (apply (partial c2d/point canref) params)
        :arc   (apply (partial c2d/arc canref) params)
        ))
    (if subglyphs
      (do 

       (c2d/set-color canref :red)
       (c2d/set-stroke canref 5)
       ;(c2d/point canref 0 0)
      (doseq [[attachment glyph] (map vector attach subglyphs)
              :let [[x y theta] attachment]]
        (c2d/push-matrix canref)
        (c2d/translate canref x y)
        (c2d/rotate canref theta)
        ;(c2d/set-color canref :blue)
        ;(c2d/arc canref 0 0 10 10 0 fm/TWO_PI)
        (draw-glyph canref glyph)
        (c2d/pop-matrix canref)
      ))
      )
    (if outglyph
      (do 
        (apply (partial c2d/translate canref) out)
        ;(c2d/translate canref width 0)
        (draw-glyph canref outglyph)
        )
      )))

;(map :parts [(g/zero) (g/one) (g/one) (g/zero) (g/two)])
(defn draw-sigil [canvas sigil]
  (c2d/with-canvas [canref canvas]
                   (let [hw (* 0.5 (:w canref))
                         sw (- hw (* 0.5 hw))
                         sh (* 0.5 (:h canref))
                         strokespacer 20
                         ]

                     (c2d/push-matrix canref)
                     (c2d/translate canref sw sh)
                     (c2d/set-color canref :green)
                     (c2d/set-stroke canref stroke)
                     (draw-glyph canref sigil)
                     (comment loop [glyph sigils
                            ;x 0
                            ;y 0
                            ]
                       (if (nil? sigils)
                         nil
                         (let [sigil (first sigils)
                               in    (:in sigil)
                               out   (:out sigil)
                               width (:width sigil)
                               half  (* 0.5 width)]
                           (c2d/set-color canref :lime)
                           (c2d/line canref [0 0] [strokespacer 0])
                           (c2d/set-color canref :green)
                           (c2d/translate canref [(+ strokespacer half) 0])
                           (draw-strokes canref sigil)
                           (c2d/translate canref [half 0])
                           (recur (next sigils) )

                           )
                         )
                       
                       )
                     )))

(def testsigil 
  (g/attach-out-glyph 
    (g/zero)
    (g/attach-out-glyph (g/join-line) 
                        (g/attach-glyph (g/one) (g/two)))))

(def testsigil (g/size-glyph testsigil))

(defn digit->glyphfn [d]
  (case d
    \0 (g/zero)
    \1 (g/one)
    \2 (g/two)
    \3 (g/three)

    ))
(comment
  (def testsigil (arp->sigil "20:21:23"))
  (def testsigil (arp->sigil "00:21:30"))
  (def testsigil (arp->sigil "10:20:30:31:33"))

  (arp->sigil "03:03:03")
 (g/attach-glyphs-by-line (g/one) (g/two))
  )

(defn arp->sigil [macstr]
  (let [rev    (clojure.string/reverse macstr)
        segs   (clojure.string/split rev #":")
        gpairs (for [seg segs
                     :let [a (first seg)
                           b (second seg)]]
                 (g/attach-glyph (digit->glyphfn b) (digit->glyphfn a)))
        ;gpairs (reverse gpairs)
        ;lined  (map #(g/attach-out-glyph (g/join-line) %) gpairs) 
        ]
    ;(reduce #(g/attach-out-glyph (g/attach-out-glyph  (g/join-line)) %2) lined)
    (reduce #(g/attach-glyphs-by-line %2 %1) gpairs)
    
    ))

(defn draw-testbed [canvas]
  (comment c2d/with-canvas [canvas canvas]
                   ;(draw-sigil canvas (g/two))
                   (c2d/set-color canvas :red)
                   (c2d/set-stroke canvas 1)
                   (c2d/translate canvas 100 100)
                   (c2d/line canvas -100 0 100 0)
                   (c2d/line canvas 0 -100 0 100)
                   (c2d/arc canvas 0 0 100 100 0 fm/TWO_PI)
                   (c2d/arc canvas 0 0 200 200 0 fm/TWO_PI)
                   (c2d/shape canvas (c2d/rect-shape -10 -10 20 20))
                   ;(c2d/shape canvas (g/attach-rect2d-at (c2d/rect-shape -10 -10 20 20) 0 -10 -10 0 fm/-HALF_PI) true)
                   
                   )
  (c2d/with-canvas [canvas canvas]
                   (draw-sigil canvas testsigil)
  ))

(defn draw-error [canvas]
  (let [exc     (:exception @state)
        message (.getMessage exc)
        traces  (.getStackTrace exc)
        lines   (filter #(re-matches #".*arp_sigils.*" %) (concat (map str traces)))
        tmout   (:exception-timeout @state)
        diff    (max 0 (- tmout (System/currentTimeMillis)))  
        ]
    (cond 
      (= 0 tmout)
      (do
        (swap! state assoc :exception-timeout (+ (* 1000 10) (System/currentTimeMillis)))
        )
      (>= (System/currentTimeMillis) tmout)
      (do
        ;(c2d/with-canvas-> canvas (c2d/line 0 0 1000 1000))
        (reset! errstate @state)
        (reset-state)
        ) 
      )
    (c2d/with-canvas [canvas canvas]
                     (c2d/set-font canvas "Hack")
                     (c2d/set-font-attributes canvas 10 :bold)
                     ;(c2d/set-background canvas 45 51 45)
                     (c2d/set-color canvas :lime)
                     (c2d/text canvas message 10 20)
                     (c2d/text canvas (str diff) (- (:w canvas) 200) (- (:h canvas) 20))
                     (doall (map-indexed #(c2d/text canvas %2 10 (+ 50 (* 10 %1))) lines))
                     (c2d/text canvas (:mode @state) (- (:w canvas) 200) (- (:h canvas) 30))
                     (c2d/text canvas (:addr @state) (- (:w canvas) 200) (- (:h canvas) 40))
                     )
    ))

(defn draw [canvas _ _ _]
  (try
    (c2d/with-canvas-> canvas 
                       (c2d/set-background :black)) 

    (case (:mode @state)
      :error (draw-error canvas)
      :sigil nil;(draw-sigil canvas)
      nil)
    (draw-testbed canvas)
    (catch Exception e
      (do
        (swap! state assoc :exception e)
        (swap! state assoc :mode :error)
        (draw-error canvas))))
 )

(defn start [fullscreen?]
  (if fullscreen?
    (let [w (c2d/screen-width)
          h (c2d/screen-height)]
      (def canvas (c2d/canvas w h))
      (def weather-canvas (c2d/canvas w h))
      (def window 
        (c2d/show-window {:canvas canvas 
                          :window-name "arp-sigils"
                          :draw-fn draw
                          :fps 30
                          :position [0 0]
                          :w w
                          :h h
                          }
                         ))
      )
    (def window 
      (c2d/show-window {:canvas canvas 
                        :window-name "arp-sigils"
                        :draw-fn draw
                        :fps 30
                        }
                       ))
    ))

(comment
  (def pi (.. (c2d/rect-shape 0 0 10 10) (getPathIterator nil) ))
  (def a (double-array 6))
  (.isDone pi)
  (.next pi)
  (.currentSegment pi a)
  (vec a)
(identity java.awt.geom.PathIterator/SEG_CLOSE)

  (let [pi (.. (c2d/rect-shape 0 0 10 10) (getPathIterator nil) )]
    (while (not (.isDone pi))
      (.currentSegment pi (double-array 6)))
      (.next pi)
      )
  (type (c2d/rect-shape 0 0 10 10))
  (clojure.reflect/reflect (c2d/rect-shape 0 0 10 10))
  (->  (c2d/rect-shape 0 0 10 10)
    (c2d/shape->path-def
    (.createTransformedShape (java.awt.geom.AffineTransform/getRotateInstance  1.0 0.0 0.0) 
    (c2d/rect-shape 0 0 10 10))
      )
  )
(doto (java.awt.geom.AffineTransform.) (.rotate 1.0) (.translate 10 10))
    (c2d/shape->path-def (c2d/rect-shape 0 0 10 10))
   (c2d/bounding-box )
    (.rotate 90)
  (->  (doto (c2d/rect-shape 0 0 10 10) (.rotate 90))
    (.rotate 90)
   (c2d/bounding-box )
  (-> (g/rotate-shape-at (c2d/crect-shape -10 -10 10 10) -10 0 Math/PI)
   c2d/bounding-box )
  (c2d/path-def->shape (:parts (g/one)))
    (c2d/path-def->shape [[:line 0.0 0.0 1.0 1.0]])
  )
  (g/size-sigil (g/zero))

  (.getBounds (.createUnion (c2d/rect-shape 0 0 10 10) (c2d/rect-shape 10 10 20 20)))
  (g/size-sigil (g/attach-sigil (g/zero) (g/one)))
  
  )
