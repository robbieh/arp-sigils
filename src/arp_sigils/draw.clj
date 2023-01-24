(ns arp-sigils.draw
  (:require [arp-sigils.sigils :as s]
            [arp-sigils.glyphs :as g]
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
  (let [{:keys [parts width in out bbox]} glyph
        [inx iny] in
        [outx outy] out ]
    (c2d/push-matrix canref)
    ;(c2d/translate canref (- (* 0.5 width) 0)
    (c2d/translate canref  (* 0.5 width) 0)
    ;(c2d/set-color canref :red)
    ;(c2d/set-stroke canref 0.5)
    ;(c2d/arc canref 0 0 20 20 0 fm/TWO_PI)
    ;(c2d/set-color canref :green)
    (c2d/set-stroke canref stroke)
    (doseq [part parts
            :let [kind (first part)
                  params (rest part)]]
      (case kind
        :line  (apply (partial c2d/line canref) params)
        :point (apply (partial c2d/point canref) params)
        :arc   (apply (partial c2d/arc canref) params)
        ))
    (c2d/pop-matrix canref)
    ))

(second testsigil)
(identity testsigil)
[{:name :join-line, :next 1, :data {:parts [[:line -20 0 20 0]], :width 40, :in [-20 0], :out [20 0], :bbox #object[java.awt.geom.Rectangle2D$Double 0xbdef600 "java.awt.geom.Rectangle2D$Double[x=-20.0,y=-20.0,w=20.0,h=20.0]"]}}

{:name :one, :next 2, :children [5], :data {:parts [[:line 0 -10.0 0 10.0] [:point 0 -10.0]], :width 20.0, :in [-10.0 0], :out [10.0 0], :bbox #object[java.awt.geom.Rectangle2D$Double 0x2a3facd4 "java.awt.geom.Rectangle2D$Double[x=-10.0,y=-20.0,w=20.0,h=40.0]"], :attach [[0 -10.0 1.5707963267948966]]}}

{:name :join-line, :next 3, :data {:parts [[:line -20 0 20 0]], :width 40, :in [-20 0], :out [20 0], :bbox #object[java.awt.geom.Rectangle2D$Double 0x4a8d4a21 "java.awt.geom.Rectangle2D$Double[x=-20.0,y=-20.0,w=20.0,h=20.0]"]}}
{:name :one, :next 4, :children [6], :data {:parts [[:line 0 -10.0 0 10.0] [:point 0 -10.0]], :width 20.0, :in [-10.0 0], :out [10.0 0], :bbox #object[java.awt.geom.Rectangle2D$Double 0x5cd7ac0a "java.awt.geom.Rectangle2D$Double[x=-10.0,y=-20.0,w=20.0,h=40.0]"], :attach [[0 -10.0 1.5707963267948966]]}}
{:name :join-line, :data {:parts [[:line -20 0 20 0]], :width 40, :in [-20 0], :out [20 0], :bbox #object[java.awt.geom.Rectangle2D$Double 0x20453f66 "java.awt.geom.Rectangle2D$Double[x=-20.0,y=-20.0,w=20.0,h=20.0]"]}}
{:name :zero, :data {:parts [[:arc 0 0 20 20 0.0 6.283185307179586]], :width 20, :in [-10.0 0], :out [10.0 0], :bbox #object[java.awt.geom.Rectangle2D$Double 0x774b7eb6 "java.awt.geom.Rectangle2D$Double[x=-10.0,y=-10.0,w=20.0,h=20.0]"], :attach [[0 0 0.0]]}}
{:name :zero, :data {:parts [[:arc 0 0 20 20 0.0 6.283185307179586]], :width 20, :in [-10.0 0], :out [10.0 0], :bbox #object[java.awt.geom.Rectangle2D$Double 0x5737fe3c "java.awt.geom.Rectangle2D$Double[x=-10.0,y=-10.0,w=20.0,h=20.0]"], :attach [[0 0 0.0]]}}]

;(map :parts [(g/zero) (g/one) (g/one) (g/zero) (g/two)])
(defn draw-sigil [canref sigil node]
  (let [me (get sigil node)
        {:keys [:next :children :data ]} me
        out (:out data [20, 0])
        width (:width data 20)  
        ]
    ;(c2d/push-matrix canref)
    ;(c2d/translate canref sw sh)
    (c2d/set-color canref :red)
    (c2d/set-stroke canref 0.5)
    ;(c2d/crect canref 0 0 width width true)
    (c2d/set-stroke canref stroke)
    (c2d/set-color canref 0 (+ 50 (* node 30)) 0)
    ;(c2d/set-stroke canref stroke)
    (draw-glyph canref data)
    (c2d/translate canref out)
    (c2d/translate canref (* 0.5 width) 0)
    (if-not (empty? children)
      (doseq [[child tmatrix] (map vector children (:attach data))
              :let [[x y theta] tmatrix]]
        (c2d/push-matrix canref)
        (c2d/translate canref x y)
        (c2d/rotate canref theta)
        ;(c2d/line canref 0 0 0 100)
        ;(c2d/line canref 0 0 100 0)
        (draw-sigil canref sigil child)
        (c2d/pop-matrix canref)
        )
      )
    (when next
        (draw-sigil canref sigil next)
        )

    ))



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

(def lineno (atom 20))
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
  (reset! lineno 20)
  (let [
          hw (* 0.5 (:w canvas))
          hh (* 0.5 (:h canvas))
        ] 
    (c2d/with-canvas [canref canvas]
                     (c2d/translate canref (* 0.25 hw) hh)
                     (draw-sigil canref testsigil 0)
                     )
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
                       (c2d/set-background :black)
                       ) 

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
  (def testsigil 
    (s/size-sigil (-> (s/append-glyph [] :join-line)
      (s/append-glyph-at-line-end , 0 :one)
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 :one)
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/attach-child , 1 :zero)
      (s/attach-child , 3 :zero)
    ) 0))
  (def testsigil 
    (s/size-sigil (-> (s/append-glyph [] :join-line)
      (s/append-glyph-at-line-end , 0 :two)
      (s/attach-child , 1 :zero)
    ) 0))
  (def testsigil 
    (s/size-sigil (-> (s/append-glyph [] :join-line)
      (s/append-glyph-at-line-end , 0 :zero)
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 :one)
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 :two)
    ) 0))

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
