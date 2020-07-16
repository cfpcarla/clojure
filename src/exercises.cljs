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

;; Exercise 14 grouping thinks
;; Given a list of strings, categorize them by their starting character.
(defn categorize [strings] (group-by first strings))

;; Exericse 15 - map transform
;; Write a function that takes a map with the below format:
;; {:disabled? true
;;  :name "james"
;;  :points 0}
;; and returns a map with the :disabled? key removed, 1 added to the :points value, add a key called :activated added with the value "now"; all other keys should be left as-is.

(defn activate
  [person]
  (-> person
      (dissoc :disabled?)
      (assoc :activated "now")
      (update :points inc)))

;; exercise 16  - partial-map
;; Write a function that takes two functions and a list. Transform the given list by applying the first function to all the values in the list that return true for the second function; other values should remain as is.

defn partial-map
  [f pred? values]
  (map (fn [x]
         (if (pred? x)
          (f x)
          x))
       values))

;; Exercise 17 - phone number sanitization (filter)
;; Implement a function to clean-up phone numbers so that they can be stored in a standardized way: a string of 10 digits.
;; Make use of filter.

(defn sanitize
  [phonenumber]
  (->> phonenumber
       (filter #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
       (take-last 10)
       (apply str)))

;; Exercise 18 
;; Run-length-encoding is a form of lossless data compression. For example, the array [:a :a :b :b :b :c :a :a :d :d] could be represented as: [[:a 2] [:b 3] [:c 1] [:a 2] [:d 2]]. Write a function to decode a run-length encoded 

(defn run-length-decode
  [runs]
  (mapcat (fn [[val len]]
            (repeat len val))
          runs))

;; Exercise 19
;; Write a function to sum a list of numbers, using apply.

(def sumNumber [nums] (apply + nums))

;; Exercise 20
;; Write a function to sum a list of numbers, using an atom and doseq.

(defn sumListNumbers
  [nums] (let [total (atom 0)]
     (doseq [n nums]
       (swap! sums + n))
         @total))

;; Exercise 21
;; Write a function to sum a list of numbers, using reduce.
(defn summingList
  [values] (reduce + values))

;; Exercise 22
;; Given two sets of user interests, return sets indicating: (1) their common interests, (2) each of their unique interests, and (3) all of their interests.

(require '[clojure.set :as set])
(defn using-sets
  [y z]
  {:shared (set/intersection y z)
   :all (set/union y z)
   :unique-y (set/difference y z)
   :unique-z (set/difference z y))})

;; Exercise 23
;; Write a function that inserts a value into a vector at a given index (moving all subsequent values) .

;; (defn insert-at [v index value])

(defn insert-vector
  [v index value] (into (conj (subvec v 0 index) value) (subvec v index)))

;; Exercise 24 - Regex extraction
;; Given a string like "Lastname, Firstname (some title)", use a regular expression to extract out the first name, last name, and title.

(defn extract-info 
  [string] 
  (let [[_ lastn firstn title] (re-matches #"([^,]+), ([^(]+) \(([^)]+)\)"
                                           string)]
    {:first firstn
     :last lastn
     :title title})))


;; Exercise 25 - regex searching
;; Given a string of text, find all the substrings inside of it that are phone numbers.

(defn find-phone-numbers
  [text]
  (->> text
       (re-seq #"(?:\+?\d[- ])?(?:\(\d{3}\)|\d{3})[- ]\d{3}[- ]\d{4}")))

;; Exercise 26
;; Write a function that removes a value at a given index from a vector, moving everything else up.
;; (defn dissoc-at [v index])
;; 
(defn dissoc-at [v index]
(into (subvec v 0 index) (subvec v (inc index))))
                                  (dissoc-at[1 2 3 4]1)

;; Exercise 27
;; summing a list (safe recursion)
;; Mark as Finished
;; Write a function to sum a list of numbers. Use recursion, but in a way that doesn't blow the stack.

(defn summing-list 
  [values] (loop [list-of-numbers numbers total 0]
           (if (empty? list-of-number) total)
           (recur (rest list-of-numbers) (+ (first list-of-numbers) total))))
       (sum (1 2 3 4))


;; Exercise 28 - merging maps
;; Write a function that takes two lists of maps and combines the corresponding pairs.
(defn join-maps [col1 col2] (map merge col1 col2))
                                  
                                  
;; Exercise 29  - Make pmap
;; pmap is a built-in function that acts just like map, except it runs the functions over the collection in parallel.
;; Implement your own version of pmap and test that it works by using time to compare the performance of the normal map and your version of pmap. You can use Thread/sleep inside a function to simulate a long running process.      
 (defn my-pmap
         [f coll]
         (->> (map (fn [x]
                     (future (f x)))
                   coll)
              (map deref)))                           

;; Exercise 32
;; Given a list, repeat each element in the list multiple times (the number of times being equal to the item's position in the list) .
;; map-indexed fn flatten repeat

(defn incresing-repeat 
  [values] (->> (map-indexed (fn [index value]
                              (repeat (inc index) value))values) (flatten)))
(incresing-repeat [a b c d])

 

;;  Exercise
;;  Write a function that takes a matrix as a vector of vectors and inverts it.
 
 (defn transpose [data]
   (apply mapv vector data))