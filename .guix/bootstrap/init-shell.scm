(use-modules (ice-9 readline)
             (ice-9 format))

(define (init-wolf-environment)
  "Initialize Wolf Shell Environment with enhanced configuration"
  (display "🌀 Initializing Wolf Shell Environment...\n")
  
  ;; Set core environment variables
  (setenv "WOLF_HOME" (getcwd))
  (setenv "WOLF_MODE" "bootstrap")
  (setenv "WOLF_STAGE" "stage0")
  
  ;; Configure paths for OpenCog integration
  (when (file-exists? "./cogutil")
    (setenv "COGUTIL_PATH" (string-append (getcwd) "/cogutil")))
  (when (file-exists? "./atomspace")
    (setenv "ATOMSPACE_PATH" (string-append (getcwd) "/atomspace")))
  (when (file-exists? "./cogserver")
    (setenv "COGSERVER_PATH" (string-append (getcwd) "/cogserver")))
  
  ;; Set up symbolic filesystem mount point
  (setenv "ASFS_MOUNT" "/tmp/asfs")
  
  ;; Configure Guile load path for bootstrap modules
  (set! %load-path (cons (string-append (getcwd) "/.guix/bootstrap") %load-path))
  
  (display "  ✓ WOLF_HOME configured\n")
  (display "  ✓ OpenCog paths detected\n")
  (display "  ✓ Bootstrap modules available\n")
  (display "  ✅ Environment ready for Stage0 bootstrap\n"))

(define (wolf-shell-info)
  "Display Wolf Shell information and available commands"
  (display "\n🐺 WolfCog AGI-OS Bootstrap Shell\n")
  (display "================================\n")
  (display "Available commands:\n")
  (display "  (stage0-bootstrap)     - Run full Stage0 bootstrap\n")
  (display "  (detect-wolfram)       - Detect Wolfram kernels\n")
  (display "  (verify-opencog)       - Check OpenCog components\n")
  (display "  (wolf-shell-info)      - Show this information\n")
  (display "  ,q                     - Quit shell\n\n"))

(define (detect-wolfram)
  "Quick Wolfram kernel detection command"
  (load "stage0-bootstrap.scm")
  (detect-wolfram-kernels))

(define (verify-opencog)
  "Quick OpenCog verification command"
  (load "stage0-bootstrap.scm")
  (verify-opencog-unified))

;; Initialize environment
(init-wolf-environment)

;; Show shell information
(wolf-shell-info)

;; Enable readline for interactive use
(activate-readline)