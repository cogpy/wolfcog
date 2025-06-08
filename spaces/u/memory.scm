;; User Space Memory - Interactive symbolic components
;; Part of WolfCog trinitized OS model

(define-module (wolfcog spaces u memory)
  #:export (init-user-memory
            user-memory-add
            user-memory-get
            user-memory-walk))

;; User space symbolic memory graph
(define *user-memory* '())

(define (init-user-memory)
  "Initialize user space symbolic memory"
  (display "👤 Initializing user space memory...\n")
  (set! *user-memory* '((type . user-space)
                        (context . interactive)
                        (memory . ()))))

(define (user-memory-add key value)
  "Add symbolic structure to user memory"
  (let ((memory-entry (assoc 'memory *user-memory*)))
    (assoc-set! *user-memory* 'memory 
                (acons key value (cdr memory-entry)))))

(define (user-memory-get key)
  "Retrieve symbolic structure from user memory"
  (let ((memory (assoc-ref *user-memory* 'memory)))
    (assoc-ref memory key)))

(define (user-memory-walk)
  "Walk through user memory structures"
  (display "🚶 Walking user memory...\n")
  (assoc-ref *user-memory* 'memory))