(require '[clojure.string :as str])

(defn toggle [bit]
  (if (= bit \0) \1 \0)
)

(defn dragonize [bits]
  (str/join "0" [
    bits
    (apply str (map toggle (str/reverse bits)))
  ])
)

(assert (= (dragonize "1") "100"))
(assert (= (dragonize "0") "001"))
(assert (= (dragonize "11111") "11111000000"))
(assert (= (dragonize "111100001010") "1111000010100101011110000"))

(defn fill [bits length]
  (if (>= (count bits) length)
    (subs bits 0 length)
    (recur (dragonize bits) length)
  )
)

(assert (= (fill "10000" 20) "10000011110010000111"))

(defn check [bits]
  (if (= [\0 \0] bits) \1
    (if (= [\1 \1] bits) \1 \0)
  )
)

(defn checksum [bits]
  (let
    [sum
      (reduce
        (fn [acc p] (conj acc (check p)))
        []
        (partition 2 bits)
      )
    ]
    (if (= (mod (count sum) 2) 0)
      (recur sum)
      (apply str sum)
    )
  )
)

(assert (= (checksum (fill "10000" 20)) "01100"))

(def input "00111101111101000")

(do
  (print "part1: ")
  (println (checksum (fill input 272)))
)

(do
  (print "part2: ")
  (println (checksum (fill input 35651584)))
)
