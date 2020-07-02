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

; Exercise08
; Implement a function that lets someone convert their age on one planet to their age on another.
;; For example:
;; 30 years on Earth is about 1 year on Saturn (a year being a single revolution around the sun).
;; 10 years on Mars is 78 years on Mercury. You will need to look up the relevant data on planets yourself.

(def lookup
  {:mercury 0.240867
   :venus 0.61519726
   :earth 1
   :mars 1.8808158
   :jupiter 11.862615
   :saturn 29.447498
   :uranus 84.016846
   :neptune 164.79132})

(defn convert-space-age
  [age source-planet target-planet]
  (int (* age (/ (lookup source-planet) (lookup target-planet)))))

;; Exercise 09
;; Devise a data model to represent a game of tic-tac-toe. For example, describe a game whose board currently looks like the below:
;; _|X|O
;; _|_|X
;; O|_|_
;; In your model, include a history of the moves played. Write a function turns-played that returns how many turns have been played.

(def game-state
  {:current-player "X"
   :board [nil "X" "O" nil nil "X" "O" nil nil]
   :history [{:player "X"
              :location 1}
             {:player "O"
              :location 2}
             {:player "X"
              :location 5}
             {:player "O"
              :location 6}]})

(defn moves-played [state] (count (state :history)))

(defn moves-played-alt [state] (count (remove nil? (state :board))))

;; Exercise 10
;; Write a function get-and-set that makes a change to a collection at some location, and return the original value at that location and the changed collection.
;; get-and-set should take 3 arguments: a key, a value, and a collection (a vector or map).
;; It should return a vector of two things: (1) the value that was at that key in the original collection, and (2) the updated collection.


(defn get-and-set [k v col] [(get col k) (assoc col k v)])

;; exercise 11
;; Read in a text file (assuming it's in the current directory), reverse the file's contents, and write the result to a new file (the name now prefixed by "rev-") .

(require [clojure.string :as string])

(defn reverse-file
  [file-name]
  ->> (slurp file-name)
  string/reverse
  (spit (str "rev-" file-name))))

;; Exercise 12
;; Write a function that takes a map with this structure:

;; {:people {1 {:name "james"
;;              :points 1}
;;           2 {:name "rafd"
;;              :points 5}}}
;; as well as an id, a keyword, and some value. Return the same map but with the person with the given id having the given keyword & value added to their data (see tests for an example).

(defn add-info [info id key value] (assoc-in info [:people id key] value))

;; Exercise 13 Update deep
;; Write a function that takes a map with this structure:

;; {:people {1 {:name "james"
;;              :points 1}
;;           2 {:name "rafd"
;;              :points 5}}}
;; as well as an id and a number. Return the same map but with the person with the given id having their name converted to upper-case and the given number added to their points.

(require [clojure.string :as string])
(defn update-info
  [info id points]
  (-> info
      (update-in [:people id :name] string/upper-case)
      (update-in [:people id :points] + points)))