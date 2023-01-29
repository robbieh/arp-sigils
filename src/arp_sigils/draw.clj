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
                  :exception-timeout 0
                  :sigil nil
                  }))
(def sigils (atom {}))
(def errstate (atom {}))

(def stroke 3)

(defn reset-state []
  (reset! state {:mode :sigil
                :addr "94:c6:91:16:48:49"
                :exception nil
                :exception-timeout 0}))

(def canvas (c2d/canvas 800 400))

(defn negpoint [[x y]]
  [(- x) (- y)])

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

(defn line-pct 
  ([canref pct p1 p2]
   (let [[x1 y1] p1
         [x2 y2] p2]
     (line-pct canref pct x1 y1 x2 y2)))
  ([canref pct x1 y1 x2 y2]
   (let [pct   (* 0.1 pct)
         xdiff (* 0.1 (- x2 x1))
         ydiff (* 0.1 (- y2 y1))
         xnew  (+ x1 (* pct xdiff))
         ynew  (+ y1 (* pct ydiff))
         ]
     (c2d/line canref x1 y1 xnew ynew)))
  )

(defn arc-pct [canref pct x y w h start end]
  (let [pct    (* 0.01 pct)
        diff   (- end start)
        newend (+ start (* pct diff))
        ]
    (c2d/arc canref x y w h start newend)))

(defn draw-glyph-pct [canref glyph]
  (let [{:keys [parts width in out bbox]} glyph
        pct (get glyph :pct 0)
        [inx iny] in
        [outx outy] out ]
    (c2d/push-matrix canref)
    (c2d/set-stroke canref stroke)
    (doseq [part parts
            :let [kind (first part)
                  params (rest part)]]
      (case kind
        :line  (apply (partial line-pct canref pct) params)
        :point (do (c2d/set-stroke canref (* 3 stroke)) (apply (partial c2d/point canref) params) (c2d/set-stroke canref  stroke))
        :arc   (apply (partial arc-pct canref pct) params)
        ))
    (c2d/pop-matrix canref)
    (inc pct)
    ))

(defn draw-glyph [canref glyph]
  (let [{:keys [parts width in out bbox]} glyph
        [inx iny] in
        [outx outy] out ]
    (c2d/push-matrix canref)
    ;(c2d/translate canref (- (* 0.5 width)) 0)
    ;(c2d/translate canref  (* 0.5 width) 0)
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


(defn draw-sigil-pct [canref sigilkey node]
  (let [sigil  (get @sigils sigilkey)
        me     (get sigil node)
        {:keys [:next :children :data ]} me
        out    (:out data [20, 0])
        width  (:width data 20)  
        pct    (min (:pct data 0) 100)
        n      (get sigil next)
        in     (negpoint (get-in n [:data :in] [0 0]))
        ]
    (c2d/set-stroke canref stroke)
    (c2d/set-color canref :lime)
    (draw-glyph-pct canref data )
    (when (>= pct 100 ) 
      (if-not (empty? children)
        (doseq [[child tmatrix] (map vector children (:attach data))
                :let [[x y theta] tmatrix
                      chin (get-in sigil [child :data :in] )]]
          (c2d/push-matrix canref)
          (c2d/translate canref x y)
          (c2d/rotate canref theta)
          (c2d/translate canref (negpoint chin))
          (draw-sigil-pct canref sigilkey child)
          (c2d/pop-matrix canref)
          )))
    (c2d/translate canref out)
    (c2d/translate canref in)
    ;(c2d/translate canref (* 0.5 width) 0)
    (when (and (>= pct 100) next)
        (draw-sigil-pct canref sigilkey next))
    (swap! sigils assoc-in [sigilkey node :data :pct] (inc pct))
    ))

(defn draw-sigil [canref sigil node]
  (let [me     (get sigil node)
        {:keys [:next :children :data ]} me
        out    (:out data [20, 0])
        width  (:width data 20)  
        n      (get sigil next)
        in     (negpoint (get-in n [:data :in] [0 0]))
        ]
    (c2d/set-color canref :red)
    (c2d/set-stroke canref 0.75)
    ;(c2d/line canref 0 0 20 0)
    ;(c2d/line canref 0 0 0 20)
    ;(c2d/crect canref 0 0 width width true)
    (c2d/set-stroke canref stroke)
    ;(c2d/set-color canref 0 (+ 50 (* node 20)) 0)
    (c2d/set-color canref :lime)
    ;(c2d/set-stroke canref stroke)
    ;(c2d/translate canref (* -0.5 width) 0)
    (draw-glyph canref data)
    (if-not (empty? children)
      (doseq [[child tmatrix] (map vector children (:attach data))
              :let [[x y theta] tmatrix
                    chin (get-in sigil [child :data :in] )]]
        (c2d/push-matrix canref)
        (c2d/translate canref x y)
        (c2d/rotate canref theta)
        (c2d/translate canref (negpoint chin) )
        ;(c2d/line canref 0 0 0 100)
        ;(c2d/line canref 0 0 100 0)
        (draw-sigil canref sigil child)
        (c2d/pop-matrix canref)
        )
      )
    (c2d/translate canref out)
    (c2d/translate canref in)
    ;(c2d/translate canref (* 0.5 width) 0)
    (when next
        (draw-sigil canref sigil next)
        )

    ))



;(defn digit->glyphfn [d] (case d \0 (g/zero) \1 (g/one) \2 (g/two) \3 (g/three)))

(comment
  (def testsigil (arp->sigil "20:21:23"))
  (def testsigil (arp->sigil "00:21:30"))
  (def testsigil (arp->sigil "10:20:30:31:33"))

  (arp->sigil "03:03:03")
 (g/attach-glyphs-by-line (g/one) (g/two))
  )

;(defn arp->sigil [macstr]
;  (let [rev    (clojure.string/reverse macstr)
;        segs   (clojure.string/split rev #":")
;        gpairs (for [seg segs
;                     :let [a (first seg)
;                           b (second seg)]]
;                 (g/attach-glyph (digit->glyphfn b) (digit->glyphfn a)))
;        ;gpairs (reverse gpairs)
;        ;lined  (map #(g/attach-out-glyph (g/join-line) %) gpairs) 
;        ]
;    ;(reduce #(g/attach-out-glyph (g/attach-out-glyph  (g/join-line)) %2) lined)
;    (reduce #(g/attach-glyphs-by-line %2 %1) gpairs)))

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
                     (draw-sigil-pct canref :s1 0)
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
  (start false)
  (type @sigils)
  (keys @sigils)
  (identity testsigil)
  (identity @sigils)
  (swap! sigils assoc-in [:s1 0 :data :pct] 10)
  (swap! sigils assoc :s1 testsigil)
  (def testsigil 
    (s/size-sigil (-> (s/append-glyph [] :join-line)
      (s/append-glyph-at-line-end , 0 :three)
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 :eleven)
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 :ten)
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 :seven)
      (s/attach-child , 1 :three)
      (s/attach-child , 3 :nine)
      (s/attach-child , 5 :eleven)
      (s/attach-child , 7 :zero)
    ) 0))
  (def gset [:zero :one :two :three :four :five :six :seven :eight :nine :ten :eleven])
  (def testsigil 
    (s/size-sigil (-> (s/append-glyph [] :join-line)
      (s/append-glyph-at-line-end , 0 (rand-nth gset))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (rand-nth gset))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (rand-nth gset))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (rand-nth gset))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (rand-nth gset))
      (s/attach-child , 1 (rand-nth gset))
      (s/attach-child , 3 (rand-nth gset))
      (s/attach-child , 5 (rand-nth gset))
      (s/attach-child , 7 (rand-nth gset))
      (s/attach-child , 9 (rand-nth gset))
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

  (c2d/with-canvas [canref canvas]
                   (line-pct canref 50 0 0 1000 1000))
  
  )
