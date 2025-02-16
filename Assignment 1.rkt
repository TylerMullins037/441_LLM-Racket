#lang racket
(define (count-frequencies lst)
  (define (count-helper lst freq-table)
    (cond
      [(empty? lst) freq-table]
      [(hash-has-key? freq-table (first lst))
       (count-helper (rest lst) (hash-update freq-table (first lst) add1))]
      [else (count-helper (rest lst) (hash-set freq-table (first lst) 1))]))
  ; Return a hash table with frequencies of each element
  (count-helper lst (make-immutable-hash)))

(define (generate-sorted-list freq-table)
  (define (generate-helper keys)
    (cond
      [(empty? keys) '()]
      [else
       (define key (first keys))
       (define count (hash-ref freq-table key))
       (append (make-list count key) (generate-helper (rest keys)))]))
  (generate-helper (sort (hash-keys freq-table) <))) ; Sort the keys manually

(define (counting-sort lst)
  (define freq-table (count-frequencies lst))
  (generate-sorted-list freq-table))

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