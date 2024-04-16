#lang racket

;; Project 2: Implementing PageRank
;;
;; PageRank is a popular graph algorithm used for information
;; retrieval and was first popularized as an algorithm powering
;; the Google search engine. Details of the PageRank algorithm will be
;; discussed in class. Here, you will implement several functions that
;; implement the PageRank algorithm in Racket.
;;
;; Hints: 
;; 
;; - For this project, you may assume that no graph will include
;; any "self-links" (pages that link to themselves) and that each page
;; will link to at least one other page.
;;
;; - you can use the code in `testing-facilities.rkt` to help generate
;; test input graphs for the project. The test suite was generated
;; using those functions.
;;
;; - You may want to define "helper functions" to break up complicated
;; function definitions.

(provide graph?
         pagerank?
         num-pages
         num-links
         get-backlinks
         mk-initial-pagerank
         step-pagerank
         iterate-pagerank-until
         rank-pages)

;; This program accepts graphs as input. Graphs are represented as a
;; list of links, where each link is a list `(,src ,dst) that signals
;; page src links to page dst.
;; (-> any? boolean?)
(define (graph? glst)
  (and (list? glst)
       (andmap
        (lambda (element)
          (match element
                 [`(,(? symbol? src) ,(? symbol? dst)) #t]
                 [else #f]))
        glst)))

;; Our implementation takes input graphs and turns them into
;; PageRanks. A PageRank is a Racket hash-map that maps pages (each 
;; represented as a Racket symbol) to their corresponding weights,
;; where those weights must sum to 1 (over the whole map).
;; A PageRank encodes a discrete probability distribution over pages.
;;
;; The test graphs for this assignment adhere to several constraints:
;; + There are no "terminal" nodes. All nodes link to at least one
;; other node.
;; + There are no "self-edges," i.e., there will never be an edge `(n0
;; n0).
;; + To maintain consistenty with the last two facts, each graph will
;; have at least two nodes.
;; + There will be no "repeat" edges. I.e., if `(n0 n1) appears once
;; in the graph, it will not appear a second time.
;;
;; (-> any? boolean?)
(define (pagerank? pr)
  (and (hash? pr)
       (andmap symbol? (hash-keys pr))
       (andmap rational? (hash-values pr))
       ;; All the values in the PageRank must sum to 1. I.e., the
       ;; PageRank forms a probability distribution.
       (= 1 (foldl + 0 (hash-values pr)))))

;; Takes some input graph and computes the number of pages in the
;; graph. For example, the graph '((n0 n1) (n1 n2)) has 3 pages, n0,
;; n1, and n2.
;;
;; (-> graph? nonnegative-integer?)
(define (num-pages graph)
  (length (remove-duplicates (append (map first graph) (map second graph)))))

;; Takes some input graph and computes the number of links emanating
;; from page. For example, (num-links '((n0 n1) (n1 n0) (n0 n2)) 'n0)
;; should return 2, as 'n0 links to 'n1 and 'n2.
;;
;; (-> graph? symbol? nonnegative-integer?)
(define (num-links graph page)
  (length (filter (lambda (link) (eq? (first link) page)) graph)))

;; Calculates a set of pages that link to page within graph. For
;; example, (get-backlinks '((n0 n1) (n1 n2) (n0 n2)) n2) should
;; return (set 'n0 'n1).
;;
;; (-> graph? symbol? (set/c symbol?))
(define (get-backlinks graph page)
  (let ((backlinks (filter (lambda (link) (eq? (second link) page)) graph)))
    (apply set (map first backlinks))))

;; Generate an initial pagerank for the input graph g. The returned
;; PageRank must satisfy pagerank?, and each value of the hash must be
;; equal to (/ 1 N), where N is the number of pages in the given
;; graph.
;; (-> graph? pagerank?)
(define (mk-initial-pagerank graph)
  (let ((num-pages (num-pages graph)))
    (for/fold ([initial-pagerank (make-hash)])
              ([page (remove-duplicates (append (map first graph) (map second graph)))])
      (hash-set! initial-pagerank page (/ 1 num-pages)))
    initial-pagerank))

;; Perform one step of PageRank on the specified graph. Return a new
;; PageRank with updated values after running the PageRank
;; calculation. The next iteration's PageRank is calculated as
;;
;; NextPageRank(page-i) = (1 - d) / N + d * S
;;
;; Where:
;;  + d is a specified "dampening factor." in range [0,1]; e.g., 0.85
;;  + N is the number of pages in the graph
;;  + S is the sum of P(page-j) for all page-j.
;;  + P(page-j) is CurrentPageRank(page-j)/NumLinks(page-j)
;;  + NumLinks(page-j) is the number of outbound links of page-j
;;  (i.e., the number of pages to which page-j has links).
;;
;; (-> pagerank? rational? graph? pagerank?)
(define (step-pagerank pr d graph)
  (let* ((num-pages (num-pages graph))
         (next-pagerank (make-hash)))
    (for/fold ([next-pagerank next-pagerank])
              ([page (remove-duplicates (append (map first graph) (map second graph)))])
      (let* ((backlinks (get-backlinks graph page))
             (page-rank-sum (for/sum ([backlink backlinks])
                              (/ (hash-ref pr backlink) (num-links graph backlink)))))
        (hash-set! next-pagerank page
                   (+ (* (- 1 d) (/ 1 num-pages))
                      (* d page-rank-sum)))))
    next-pagerank))

;; Iterate PageRank until the largest change in any page's rank is
;; smaller than a specified delta.
;;
;; (-> pagerank? rational? graph? rational? pagerank?)
(define (iterate-pagerank-until pr d graph delta)
  (let loop ([current-pagerank pr]
             [next-pagerank (step-pagerank pr d graph)])
    (if (< (max-error current-pagerank next-pagerank) delta)
        next-pagerank
        (loop next-pagerank (step-pagerank next-pagerank d graph)))))

;; Helper function to calculate the maximum error between two pageranks
(define (max-error pr1 pr2)
  (apply max (for/list ([page (hash-keys pr1)])
               (abs (- (hash-ref pr1 page 0) (hash-ref pr2 page 0))))))

;; Given a PageRank, returns the list of pages it contains in ranked
;; order (from least-popular to most-popular) as a list. You may
;; assume that none of the pages in the pagerank have the same
;; value (i.e., there will be no ambiguity in ranking)
;;
;; (-> pagerank? (listof symbol?))
(define (rank-pages pr)
  (sort (hash-keys pr) (lambda (page1 page2) (< (hash-ref pr page1) (hash-ref pr page2)))))

;; Example usage and testing facilities (modify as needed)
(define links '((A B) (B C) (C A) (D A)))
(define damping-factor 0.85)
(define delta 0.001)

(if (graph? links)
    (begin
      (displayln "Valid graph.")

      ; Example usage
      (define initial-pagerank (mk-initial-pagerank links))
      (define final-pagerank (iterate-pagerank-until initial-pagerank damping-factor links delta))
      (displayln "PageRank:")
      (displayln final-pagerank)

      ; Example ranking
      (displayln "Ranked Pages:")
      (displayln (rank-pages final-pagerank)))
    (displayln "Invalid graph."))
