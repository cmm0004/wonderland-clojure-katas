(ns alphabet-cipher.coder)

(def get_alphabet (map char (range (int \a) (inc (int \z)))))
(def indexed-alphabet (zipmap get_alphabet (iterate inc 0)))

(defn chars-for-col [col]
	(take (count get_alphabet) 
		(drop (get indexed-alphabet col) (cycle get_alphabet)
			)
		)
)

(defn col->char [col row]
	(get (zipmap (chars-for-col col) get_alphabet) row)
)

(defn col->row [col charac]
	(get (zipmap get_alphabet (chars-for-col col)) charac)
)

(defn map-matrix [mapping-function keyword message]
	(let [cypher (vec (take (count message) (cycle keyword)))]
		(loop [iter 0 result []]
		(if (= iter (count message))
			(clojure.string/join result)
			(recur (inc iter) (conj result (mapping-function (get cypher iter) (get (vec message) iter))))
			)
		)
	)
)

(defn encode [keyword message]
  "encodeme"
  (map-matrix col->char keyword message)
)

(defn decode [keyword message]
  "decodeme"
  (map-matrix col->row keyword message)
)

(defn first-repetition [vectorofchars]
	"find the first copy of a repeated sequence of characters.
	sconessconesscon -> scones"
	(loop [iter 0 result []]
		(if (= vectorofchars (take (count vectorofchars) (cycle result)))
			result
			(recur (inc iter) (conj result (get vectorofchars iter)))
		)
	)
)

(defn decipher [cypher message]
  "decypherme"

  	(loop [iter 0 result []]
		(if (= iter (count message))
			(clojure.string/join (first-repetition result))
			(recur (inc iter) (conj result (col->char (get (vec cypher) iter) (get (vec message) iter))))
			)
		)
  	
  )



