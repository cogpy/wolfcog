(use-modules (ice-9 readline))

(define (launch-wolf-shell)
  (display "🐺 Launching WolfCog Bootstrap Shell...\n")
  (readline "wolf> "))

(launch-wolf-shell)