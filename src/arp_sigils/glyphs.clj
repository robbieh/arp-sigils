(ns arp-sigils.glyphs
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as fm]))

(defn scale [x]
  (* 10 x))

(defn empty-glyph [name] {:name name })



(defn attach-glyph [base addition]
  (let [acount (count (:attach base))]
    (assoc base :subglyphs (vec (repeat acount addition)))))

(defn attach-out-glyph [base addition]
  (assoc base :outglyph addition))

;(defn attach-glyphs-by-line [a b]
;  (let [join (join-line)]
;    (attach-out-glyph a (attach-out-glyph join b))))


(defn size-glyph [glyph]
  (let [theta     (:rotation glyph)
        [x y]     (:in glyph)
        bb        (:bbox glyph) 
        subglyphs (:subglyphs glyph)
        sizefn    (:sizefn glyph)]
    (if subglyphs 
      (let [sizedsubs (map size-glyph subglyphs)
            sized     (sizefn glyph sizedsubs)]
        (merge glyph sized {:subglyphs sizedsubs}))
      glyph
      )))

(defn rotate-shape-at [shape x y theta]
    (.createTransformedShape (java.awt.geom.AffineTransform/getRotateInstance  theta x y) 
    shape))
;(get-rotated-bounds 10 10 fm/QUARTER_PI)
;(get-rotated-bounds 10 10 fm/HALF_PI)
(defn get-rotated-bounds [w h theta]
  (let [bounds (.getBounds (rotate-shape-at (c2d/crect-shape 0 0 w h) 0 0 theta))]
    [(.getWidth bounds) (.getHeight bounds)]))

;(attach-rect2d-at (c2d/rect-shape -10 -10 20 20) 0 -10 -10 0 fm/-HALF_PI)
(defn attach-rect2d-at [rect inx iny ax ay theta]
  (let [tform (doto (java.awt.geom.AffineTransform.)  
                    (.translate (- inx) (- iny))
                    (.rotate theta)
                    (.translate ax ay)
                )
        path  (.createTransformedShape tform rect)
        ]
    (.getBounds path)
    )
    )

(defn bbox->wh [bbox]
  (let [[x1 y1 x2 y2] bbox] 
    [(- x2 x1) (- y2 y1)]))

(defn rect2d->corners [rect]
  [(.getX rect) 
   (.getY rect)
   (+ (.getX rect) (.getWidth rect))
   (+ (.getY rect) (.getHeight rect))
   ])

(defn union-bboxes [& bboxes]
  (->> bboxes
    (mapv (partial apply c2d/rect-shape) , )
    (reduce #(.createUnion %1 %2) ,)
    rect2d->corners))

(def child-count-map 
  {:join-line 0
   :zero 1
   :one 1
   :two 2
   :three 3})


(defn size-join-line [_]
  {:parts [[:line -20 0 20 0]]
   :width 40
   :in [-20 0]
   :out [20 0]
   :bbox (c2d/rect-shape -20 -20 20 20)
   })

(defn size-zero [children]
  (let [childbb (-> children first :bbox)
        w       (if childbb (.getWidth childbb) 20)
        h       (if childbb (.getHeight childbb) 20)
        myw     (max w h)
        hmyw    (* 0.5 myw)]
    {:parts [[:arc 0 0 myw myw 0.0 (* 2.0 Math/PI)]]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw myw)
     :attach [[0 hmyw (fm/radians 90)]] }))

(defn size-one [children]
  (let [childbb (-> children first :bbox)
        w       (if childbb (.getWidth childbb) 0)
        h       (if childbb (.getHeight childbb) 0)
        myw     20
        myh     40
        combyw  (max myw w)
        combyh  (+ myh h)
        hmyw    (* 0.5 myw)
        hmyh    (* 0.5 myh)
        ]
    {:parts [[:line (- hmyw) 0 0 0]
             [:line 0 (- hmyh) 0 hmyh] [:point 0 (- hmyh)]
             [:line 0 0 hmyw 0]
             ]
     :width combyw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  combyw combyh)
     :attach [[0 (- hmyh) fm/-HALF_PI]] }))

(defn size-two [children]
  (let [childbbs (map :bbox children)
        cws      (mapv #(.getWidth %) childbbs)
        chs      (mapv #(.getHeight %) childbbs)
        myw     20
        myh     40
        combyw  (apply max (conj cws myw))
        combyh  (apply + (conj chs myh))
        hmyw    (* 0.5 myw)
        hmyh    (* 0.5 myh)
        qmyw    (* 0.25 myw)
        qmyh    (* 0.25 myh)
        ]
    {:parts [[:line (- hmyw) 0 0 0]
             [:line 0 (- hmyh) 0 hmyh] [:point 0 (- hmyh)]
             [:point [qmyw qmyh]] [:point [(- qmyw) (- qmyh)]]
             [:line 0 0 hmyw 0]
             ]
     :width combyw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  combyw combyh)
     :attach [[0 (- hmyh) fm/-HALF_PI] [0 hmyh fm/HALF_PI]] }))    

;(size-three [(size-zero nil) (size-zero nil)])
(defn size-three [children]
  (println "three " children)
  (let [childbbs (map :bbox children)
        cws      (mapv #(.getWidth %) childbbs)
        chs      (mapv #(.getHeight %) childbbs)
        thetas   [(fm/radians 315) (fm/radians 225) (fm/radians 90)]
        newbb    (mapv #(get-rotated-bounds %1 %2 %3 ) cws chs thetas)
        newws    (mapv first newbb)
        newhs    (mapv second newbb)
        myw     (+ 20 (reduce + (take 2 newws)))
        myh     (+ 20 (reduce + (take-last 2 newhs)))
        ;combyw  (apply max (conj cws myw))
        ;combyh  (apply + (conj chs myh))
        hmyw    (* 0.5 myw)
        hmyh    (* 0.5 myh)
        ]
    {:parts [[:line (- hmyw) 0 0 0]
             [:line 0 0 0 hmyh] [:line 0 0 (- hmyw) (- hmyh)] [:line 0 0 hmyw (- hmyh)]
             [:point (- hmyw) (- hmyh)] [:point hmyw (- hmyh)] [:point 0 hmyh]
             [:line 0 0 hmyw 0]
             ]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw myh)
     :attach [
              [(- hmyw) (- hmyh) (fm/radians 225)]
              [hmyw (- hmyh) (fm/radians 315)]
              [0 hmyh (fm/radians 90)]
              ] }))


(def size-function-map
  {:join-line size-join-line
   :zero      size-zero
   :one       size-one
   :two       size-two
   :three     size-three
   })
;(defn join-line [sigil node]
;  (let [me (get sigil node)
;        outglyph (:outglyph me)]
;    (if (empty? outglyph)
;      (assoc sigil node (merge me join-line-map))
;      (let [child     (get sigil outglyph)
;            childfunc (:func child)
;            newsigil  (childfunc child)] 
;        (assoc newsigil node (merge me join-line-map)))) ))


;(defn zero []
;  (let [me (get sigil node)
;        outglyph (:outglyph me)
;        subglyphs (:subglyphs me)
;        ]
;
;    )
;  {:name :zero
;   :parts [[:arc 0 0 20 20 0.0 (* 2.0 Math/PI)]]
;   :subglyphs nil
;   :outglyph nil
;   :out [10 0]
;   :width 20
;   :bbox (c2d/rect-shape -10 -10 10 10)
;   :in [-10 0]
;   :attach [[0 0 0.0]]
;   :sizefn (fn [self bboxes] 
;             (let [bbox    (-> bboxes first :bbox)
;                   [xs ys] (rect2d->corners bbox)]
;               (merge self {:bbox bbox
;                            :parts [[:arc 0 0 xs ys 0.0 fm/TWO_PI]]})))})

;(defn one []
;  {:name :one
;   :parts  [[:line 0 -20 0 20] [:point 0 -20]]
;   :width 20
;   :bbox (c2d/rect-shape -10 -10 10 10)
;   :in [-10 0]
;   :out [10 0]
;   :attach [[0 20 fm/HALF_PI]]
;   :sizefn (fn [self bboxes]
;             (let [bbox    (-> bboxes first :bbox )
;                   [xs ys] (bbox->wh bbox)]
;               (merge self {:bbox bbox
;                            :parts [[:arc 0 0 xs ys 0.0 fm/TWO_PI]]})
;
;               )
;             )
;  })
;
;(defn two []
;  {:name :two
;   :parts  [[:line 0 -20 0 20] [:point 0 -20] [:point 0 20]]
;   :out [10 0]
;   :width 20
;   :bbox (c2d/rect-shape -10 -10 10 10)
;   :in [-10 0]
;   :attach [[0 20 fm/HALF_PI] [0 -20 fm/-HALF_PI]]
;   :sizefn (fn [bboxes])
;  })
;
;(defn three []
;  {:name :three
;   :parts [[:line 0 0 0 20] 
;           [:line 0 0 -10 -20] [:point -10 -20]
;           [:line 0 0 10 -20] [:point 10 -20]
;           ]
;   :width  20
;   :out [10 0]
;   :bbox (c2d/rect-shape -10 -10 10 10)
;   :in [-10 0]
;   :attach [[-10 -20 (* 1.3 fm/PI )]
;            [10 -20 (* 0.3 fm/-PI)]
;            ]
;   :sizefn (fn [boxes])
;   })
;
