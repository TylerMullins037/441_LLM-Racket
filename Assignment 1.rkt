#lang racket

(define (count-frequencies lst)
  (define (count-helper lst freq-table)
    (cond
      [(empty? lst) freq-table]
      [(hash-has-key? freq-table (first lst))
       (count-helper (rest lst) (hash-update freq-table (first lst) add1))]
      [else (count-helper (rest lst) (hash-set freq-table (first lst) 1))])) ; Set frequency
  (count-helper lst (make-immutable-hash))) ; Use immutable hash table

(define (generate-sorted-list freq-table min-val max-val)
  (define (generate-helper current-val result)
    (cond
      [(> current-val max-val) result] ; Base case: finished
      [else
       (define count (hash-ref freq-table current-val 0)) ; Get count, default to 0
       (if (> count 0)  ; Only add if count is greater than zero
           (generate-helper (+ current-val 1) 
                            (append (make-list count current-val) result)) ; Construct directly
           (generate-helper (+ current-val 1) result))])) ; Skip adding if count is zero
  (reverse (generate-helper min-val '()))) ; Start with empty list

(define (counting-sort lst)
  (if (empty? lst)
      '()  ; Return empty if input is empty
      (let* ([freq-table (count-frequencies lst)]
             [min-val (apply min lst)]
             [max-val (apply max lst)])
        (generate-sorted-list freq-table min-val max-val))))  ; Generate the sorted list

; Function to read integers from a file and convert them to a list
(define (read-integers-from-file filename)
  (define file-content (file->string filename)) ; Read the file as a string
  (define str-integers (string-split file-content)) ; Split string by whitespace
  (map string->number str-integers)) ; Convert each string to a number

; Read integers from a file, sort them, and display the result
(define (sort-file-integers filename)
  (define integers (read-integers-from-file filename))
  (define sorted-integers (counting-sort integers))
  (display sorted-integers)) ; Output the sorted list

; Example usage:
(sort-file-integers "C:/Users/Tyler/OneDrive/Documents/Racket Data/Data-1.txt")
(sort-file-integers "C:/Users/Tyler/OneDrive/Documents/Racket Data/Data-2.txt")
(sort-file-integers "C:/Users/Tyler/OneDrive/Documents/Racket Data/Data-3.txt")
(sort-file-integers "C:/Users/Tyler/OneDrive/Documents/Racket Data/Data-4.txt")
(sort-file-integers "C:/Users/Tyler/OneDrive/Documents/Racket Data/Data-5.txt")
(sort-file-integers "C:/Users/Tyler/OneDrive/Documents/Racket Data/Data-6.txt")
(sort-file-integers "C:/Users/Tyler/OneDrive/Documents/Racket Data/Data-7.txt")
