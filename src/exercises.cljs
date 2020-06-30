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