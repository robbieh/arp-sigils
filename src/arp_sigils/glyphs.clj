(ns arp-sigils.glyphs
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as fm])
  )

(defn scale [x]
  (* 10 x))



(defn attach-glyph [base addition]
  (let [acount (count (:attach base))]
    (assoc base :subglyphs (vec (repeat acount addition)))))

(defn attach-out-glyph [base addition]
  (assoc base :outglyph addition))

(defn attach-glyphs-by-line [a b]
  (let [join (join-line)]
    (attach-out-glyph a (attach-out-glyph join b))))


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

(defn join-line []
  {:name :join
   :parts [[:line -20 0 20 0]]
   :width 40
   :in [-20 0]
   :out [20 0]})

(defn zero []
  {:name :zero
   :parts [[:arc 0 0 20 20 0.0 (* 2.0 Math/PI)]]
   :subglyphs nil
   :outglyph nil
   :out [10 0]
   :width 20
   :bbox (c2d/rect-shape -10 -10 10 10)
   :in [-10 0]
   :attach [[0 0 0.0]]
   :sizefn (fn [self bboxes] 
             (let [bbox    (-> bboxes first :bbox)
                   [xs ys] (rect2d->corners bbox)]
               (merge self {:bbox bbox
                            :parts [[:arc 0 0 xs ys 0.0 fm/TWO_PI]]})))})

(defn one []
  {:name :one
   :parts  [[:line 0 -20 0 20] [:point 0 -20]]
   :width 20
   :bbox (c2d/rect-shape -10 -10 10 10)
   :in [-10 0]
   :out [10 0]
   :attach [[0 20 fm/HALF_PI]]
   :sizefn (fn [self bboxes]
             (let [bbox    (-> bboxes first :bbox )
                   [xs ys] (bbox->wh bbox)]
               (merge self {:bbox bbox
                            :parts [[:arc 0 0 xs ys 0.0 fm/TWO_PI]]})

               )
             )
  })

(defn two []
  {:name :two
   :parts  [[:line 0 -20 0 20] [:point 0 -20] [:point 0 20]]
   :out [10 0]
   :width 20
   :bbox (c2d/rect-shape -10 -10 10 10)
   :in [-10 0]
   :attach [[0 20 fm/HALF_PI] [0 -20 fm/-HALF_PI]]
   :sizefn (fn [bboxes])
  })

(defn three []
  {:name :three
   :parts [[:line 0 0 0 20] 
           [:line 0 0 -10 -20] [:point -10 -20]
           [:line 0 0 10 -20] [:point 10 -20]
           ]
   :width  20
   :out [10 0]
   :bbox (c2d/rect-shape -10 -10 10 10)
   :in [-10 0]
   :attach [[-10 -20 (* 1.3 fm/PI )]
            [10 -20 (* 0.3 fm/-PI)]
            ]
   :sizefn (fn [boxes])
   })

;{:main [:line :one :line :zero :line]}

;"94:c6:91:16:48:49"
;{:main [:line :nine [:four] :line :c [:six] :line :nine [:one] ]}

