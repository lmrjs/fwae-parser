#lang plai

;; WHAT IS A PARSER?

;; p :: String -> (listof Result)
(define-type Parser
  [parser (p procedure?)])

(define-type Result
  [result (value any/c) (rest string?)])

;; parse :: Parser -> String -> (listof Result)
;; apply the function in parse to the string to be parsed
(define (parse p str)
  ((parser-p p) str))

;; Parse the first character of a string and save it
(define read-char
  (parser (lambda (cs)
            (if (string=? "" cs)
                empty
                (list (result (string (first (string->list cs)))
                              (list->string (rest (string->list cs)))))))))
                              
;; Parse the first character of a string but don't save it
(define skip-char
  (parser (lambda (cs)
            (if (string=? "" cs)
                empty
                (list (result ""
                              (list->string (rest (string->list cs)))))))))

;; Tests
(test (parse read-char "") empty)
(test (parse read-char "abc") (list (result "a" "bc")))
(test (parse skip-char "") empty)
(test (parse skip-char "abc") (list (result "" "bc")))


;; MOTIVATIONS FOR A MONADIC PARSER

(define (skip-a value)
  (if (string=? "a" value)
      skip-char
      read-char))

(define (return value)
  (parser (lambda (str) (list (result value str)))))

;; funnel :: Parser -> (String -> Parser) -> (listof Result)
;; apply p to str, getting a value and rest of string;
;; then, apply f to value to get p;
;; finally, apply p' to rest of string
(define (funnel p f str)
  (append* (map (lambda (res)
                  (parse (f (result-value res))
                         (result-rest res)))
                (parse p str))))

;; (>>=) :: Parser -> (String -> Parser) -> Parser
(define (>>= p f)
  (parser (lambda (str)
            (append* (map (lambda (res)
                            (parse (f (result-value res))
                                   (result-rest res)))
                            (parse p str))))))

;; Tests
(test (skip-a "") read-char)
(test (skip-a "a") skip-char)
(test (skip-a "b") read-char)

(test (funnel read-char skip-a "") empty)
(test (funnel read-char skip-a "abc") (list (result "" "c")))
(test (funnel read-char skip-a "bbc") (list (result "b" "c")))

(test (parse (>>= read-char skip-a) "") empty)
(test (parse (>>= read-char skip-a) "abc") (list (result "" "c")))
(test (parse (>>= read-char skip-a) "bbc") (list (result "b" "c")))

(test (parse (>>= (>>= read-char skip-a) skip-a) "abc") (list (result "c" "")))

(test (parse (>>= read-char return) "abc") (list (result "a" "bc")))


;; PARSER COMBINATORS

;; (>>) :: Parser -> Parser -> Parser
;; parse using p1; then, ignoring value, parse remaining input using p2
(define (>> p1 p2)
  (>>= p1 (lambda _ p2)))

;; (<>) :: Parser -> Parser -> Parser
(define (<> p1 p2)
  (parser (lambda (str)
            (append (parse p1 str) (parse p2 str)))))

(define parse-num
  (parser (lambda (str)
            (if (string=? "" str)
                empty
                (let* ([c   (string (first (string->list str)))]
                       [cs  (list->string (rest (string->list str)))]
                       [num (string->number c)])
                  (if num
                      (list (result num cs))
                      empty))))))

(test (parse (>> read-char read-char) "abc") (list (result "b" "c")))

(test (parse parse-num "") empty)
(test (parse parse-num "a") empty)
(test (parse parse-num "10") (list (result 1 "0")))

(test (parse (<> parse-num read-char) "") empty)
(test (parse (<> parse-num read-char) "a") (list (result "a" "")))
(test (parse (<> parse-num read-char) "10") (list (result 1 "0") (result "1" "0")))


;; DO SYNTAX

(parse (>>= p1 (lambda (v1)
       (>>= p2 (lambda (v2)
       (>>= p3 (lambda (v3)
       ...
       (f v1 v2 v3 ...)))))))
  str)
