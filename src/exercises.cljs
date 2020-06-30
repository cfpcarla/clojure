;; exercise01
;;Refactor the below code to use a single cond instead of multiple ifs.
; Also, consider re-ordering the conditionals to simplify the logic.

defn process-value
  [value]
  (cond (string? value) :a-string
        (not (number? value)) :something-else
        (zero? value) :zero
        (> value 10) :pretty-big
        (< value 0) :negative
        :else :small-number))


; Exercise02
; Write a function to determine if some 3 side lengths are sufficient to make a triangle.

; To check if 3 sides make a triangle, you need to check that every side is less than or equal to the sum of the other two sides.

(defn triangle? [a b c] (and (< a (+ b c)) (< b (+ a c)) (< c (+ b a))
                             
                             
; Exercise03
; Write a function classify-triangle, which, given 3 numbers, returns whether a triangle is equilateral (3 equal sides), isosceles (2 equal sides), scalene, or, not a triangle.

(defn triangle? [a b c] (and (< a (+ b c)) (< b (+ a c)) (< c (+ a b))))

(defn classify-triangle
  [a b c]
  (cond (not (triangle? a b c)) :invalid
        (= a b c) :equilateral
        (or (= a b) (= b c) (= c a)) :isosceles
        :else :scalene))
                             
; Exercise04
;  Write a function clamp to restrict a value to a given range:
; (defn clamp [x min max])

(defn clamp
  [x min max]
  (cond (<= min x max) x
        (< x min) min
        :else max))

;; Exercise 05
;; Write a function largest to find the largest number in a list, making use of max and either apply or reduce.

(defn largest [nums] (apply max nums))

(defn largest-reduce [nums] (reduce max nums))

; Exercise06
; Implement a function score that calculates the scrabble score for a given word.
; Use google to find out how much each letter is worth.

(def points
  {\a 1
   \b 3
   \c 3
   \d 2
   \e 1
   \f 4
   \g 2
   \h 4
   \i 1
   \j 8
   \k 5
   \l 1
   \m 3
   \n 1
   \o 1
   \p 3
   \q 10
   \r 1
   \s 1
   \t 1
   \u 1
   \v 4
   \w 4
   \x 8
   \y 4
   \z 10})

(defn score
  [s]
  (->> s
       (map points)
       (apply +)))

; Exercise07
; Write a function count-if that counts the number of items in a collection that pass a certain predicate.

(defn count-if [pred? coll] (count (filter pred? coll)))