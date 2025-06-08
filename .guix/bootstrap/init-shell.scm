(use-modules (ice-9 readline))

(define (init-wolf-environment)
  (display "🌀 Initializing Wolf Shell Environment...\n")
  (setenv "WOLF_HOME" (getcwd))
  (setenv "WOLF_MODE" "bootstrap")
  (display "Environment ready.\n"))

(init-wolf-environment)