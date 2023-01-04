(ns arp-sigils.sigils
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as fm])
  )

(defn scale [x]
  (* 10 x))



(defn attach-sigil [base addition]
  (let [acount (count (:attach base))]
    (assoc base :subsigils (vec (repeat acount addition)))))

(defn size-sigil [sigil]
  (let [theta     (:rotation sigil)
        [x y]     (:in sigil)
        bb        (:bbox sigil) 
        subsigils (:subsigils sigil)
        sizefn    (:sizefn sigil)]
    (if subsigils 
      (let [sizedsubs (map size-sigil subsigils)
            sized     (sizefn sigil sizedsubs)]
        (merge sigil sized {:subsigils sizedsubs}))
      sigil
      )))

(defn rotate-shape-at [shape x y theta]
    (.createTransformedShape (java.awt.geom.AffineTransform/getRotateInstance  theta x y) 
    shape))


(attach-rect2d-at (c2d/rect-shape -10 -10 20 20) 0 -10 -10 0 fm/-HALF_PI)
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

(defn zero []
  {:parts [[:arc 0 0 20 20 0.0 (* 2.0 Math/PI)]]
   :width 20
   :bbox (c2d/rect-shape -10 -10 10 10)
   :in [-10 10]
   :rotation 0.0
   :attach [[0 0 0.0]]
   :sizefn (fn [self bboxes] 
             (let [bbox    (-> bboxes first :bbox)
                   [xs ys] (rect2d->corners bbox)]
               (merge self {:bbox bbox
                            :parts [[:arc 0 0 xs ys 0.0 fm/TWO_PI]]})

             ))
   })

(defn one []
  {:parts  [[:line 0 -20 0 20] [:point 0 -20]]
   :width 0
   :bbox (c2d/rect-shape -10 -10 10 10)
   :in [-10 10]
   :rotation 0.0
   :attach [[0 20 fm/-HALF_PI]]
   :sizefn (fn [self bboxes]
             (let [bbox    (-> bboxes first :bbox )
                   [xs ys] (bbox->wh bbox)]
               (merge self {:bbox bbox
                            :parts [[:arc 0 0 xs ys 0.0 fm/TWO_PI]]})

               )
             )
  })

(defn two []
  {:parts  [[:line 0 -20 0 20] [:point 0 -20] [:point 0 20]]
   :width 0
   :bbox [-10 -10 10 10]
   :in [-10 10]
   :rotation 0.0
   :attach [[0 20 fm/PI] [0 -20 fm/-HALF_PI]]
   :sizefn (fn [bboxes])
  })


;{:main [:line :one :line :zero :line]}

;"94:c6:91:16:48:49"
;{:main [:line :nine [:four] :line :c [:six] :line :nine [:one] ]}

