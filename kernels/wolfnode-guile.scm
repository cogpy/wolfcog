(use-modules (ice-9 readline)
             (ice-9 popen)
             (ice-9 rdelim))

(define (wolfnode-init)
  "Initialize WolfNode in Guile environment"
  (display "🐺 WolfNode Guile initialization...\n")
  (set! *wolfnode-state* (make-hash-table)))

(define *wolfnode-state* #f)

(define (wolfnode-eval expr)
  "Evaluate expression in WolfNode context"
  (display (string-append "🔮 WolfNode eval: " (object->string expr) "\n"))
  expr)

(define (connect-to-wolfcore)
  "Establish connection to wolfcore.lisp"
  (display "🔗 Connecting to WolfCore...\n")
  ;; TODO: Implement IPC connection
  #t)

(define (symbolic-bridge form)
  "Bridge between Guile and symbolic systems"
  (display "🌉 Symbolic bridge processing...\n")
  form)

;; Initialize WolfNode
(wolfnode-init)