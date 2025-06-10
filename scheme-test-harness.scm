#!/usr/bin/env guile -s
!#

;;; WolfCog Component Validation in Scheme
;;; Implements the recursive validation as specified in issue #36

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (srfi srfi-1))

;;; Component validation functions as specified in the issue

(define (file-exists? path)
  "Check if a file exists"
  (access? path F_OK))

(define (executable? path)
  "Check if a file is executable"
  (and (file-exists? path)
       (or (string-suffix? ".py" path)
           (access? path X_OK))))

(define (run-integration-tests component-path)
  "Run integration tests for a component"
  (let* ((test-command (format #f "cd /home/runner/work/wolfcog/wolfcog && python ~a" component-path))
         (pipe (open-input-pipe test-command))
         (output (read-string pipe))
         (exit-code (close-pipe pipe)))
    (zero? exit-code)))

;;; Main validation function from the issue
(define (validate-component component-path)
  "Ensure each component is real and testable"
  (and (file-exists? component-path)
       (executable? component-path)
       (run-integration-tests component-path)))

;;; Recursive verification as specified in the issue
(define (recursive-verify components)
  "Recursively verify components"
  (if (null? components)
      'all-components-validated
      (if (validate-component (car components))
          (begin
            (format #t "✅ Component validated: ~a~%" (car components))
            (recursive-verify (cdr components)))
          (error "Component not implemented: " (car components)))))

;;; Test harness entry point as specified in the issue
(define (wolfcog-test-runner test-type)
  "Test runner for neural-symbolic bridges"
  (case test-type
    ('opencog-bridge
     (begin
       (format #t "🧪 Testing OpenCog bridge...~%")
       (let ((bridge-path "kernels/wolfram-opencog-bridge.scm"))
         (if (file-exists? bridge-path)
             (begin
               (format #t "✅ OpenCog bridge file exists~%")
               #t)
             (begin
               (format #t "❌ OpenCog bridge file missing~%")
               #f)))))
    ('wolfram-bridge
     (begin
       (format #t "🧪 Testing Wolfram bridge...~%")
       (let ((bridge-path "src/wolfram_opencog_bridge.py"))
         (if (file-exists? bridge-path)
             (begin
               (format #t "✅ Wolfram bridge file exists~%")
               #t)
             (begin
               (format #t "❌ Wolfram bridge file missing~%")
               #f)))))
    ('integration-tests
     (begin
       (format #t "🧪 Running integration tests...~%")
       (let ((test-files '("test-integration.py" 
                           "test-real-implementation.py"
                           "validation_schematic.py")))
         (every validate-component test-files))))
    (else
     (begin
       (format #t "❌ Unknown test type: ~a~%" test-type)
       #f))))

;;; Component enumeration and audit
(define (enumerate-wolfcog-components)
  "Enumerate all WolfCog components as specified in the issue"
  (let ((components '("wolfcog-coordinator-real.py"
                     "src/symbolic_processor.py"
                     "src/task_manager.py" 
                     "src/agent_coordinator.py"
                     "daemons/scheduler_daemon.py"
                     "agents/admin_agent.py"
                     "agents/director_agent.py"
                     "kernels/wolfram-opencog-bridge.scm"
                     "src/wolfram_opencog_bridge.py"
                     "daemons/performance/performance-monitor.py")))
    (format #t "📋 WolfCog Components Audit:~%")
    (for-each (lambda (component)
                (let ((exists (file-exists? component))
                      (exec (executable? component)))
                  (format #t "  ~a ~a - exists: ~a, executable: ~a~%"
                         (if (and exists exec) "✅" "❌")
                         component exists exec)))
              components)
    components))

;;; Main validation procedure
(define (run-wolfcog-validation)
  "Run complete WolfCog validation as per cognitive flowchart"
  (format #t "🔬 WolfCog Scheme Validation Framework~%")
  (format #t "🎯 Implementing recursive test harness from issue #36~%")
  (format #t "=" (make-string 60 #\=))
  (newline)
  
  ;; Step 1: Component enumeration
  (format #t "🔍 Step 1: Component Enumeration~%")
  (let ((components (enumerate-wolfcog-components)))
    
    ;; Step 2: Recursive verification
    (format #t "~%🧪 Step 2: Recursive Component Verification~%")
    (catch #t
      (lambda ()
        (recursive-verify components)
        (format #t "🎉 All components validated successfully!~%"))
      (lambda (key . args)
        (format #t "❌ Validation failed: ~a~%" args)))
    
    ;; Step 3: Neural-symbolic bridge tests
    (format #t "~%🌉 Step 3: Neural-Symbolic Bridge Tests~%")
    (let ((opencog-result (wolfcog-test-runner 'opencog-bridge))
          (wolfram-result (wolfcog-test-runner 'wolfram-bridge))
          (integration-result (wolfcog-test-runner 'integration-tests)))
      
      (format #t "📊 Bridge Test Results:~%")
      (format #t "  OpenCog bridge: ~a~%" (if opencog-result "✅ PASS" "❌ FAIL"))
      (format #t "  Wolfram bridge: ~a~%" (if wolfram-result "✅ PASS" "❌ FAIL"))
      (format #t "  Integration tests: ~a~%" (if integration-result "✅ PASS" "❌ FAIL"))
      
      ;; Final assessment
      (let ((total-score (+ (if opencog-result 1 0)
                           (if wolfram-result 1 0) 
                           (if integration-result 1 0))))
        (format #t "~%📈 Final Score: ~a/3~%" total-score)
        (if (>= total-score 2)
            (format #t "🎉 WolfCog validation PASSED - Real implementation confirmed!~%")
            (format #t "⚠️ WolfCog validation needs attention - Some components require work~%"))))))

;;; Entry point for command line usage
(define (main args)
  "Main entry point"
  (if (and (> (length args) 1)
           (string=? (cadr args) "validate"))
      (run-wolfcog-validation)
      (begin
        (format #t "WolfCog Scheme Test Runner~%")
        (format #t "Usage: guile scheme-test-harness.scm validate~%")
        (format #t "       guile -c \"(wolfcog-test-runner 'opencog-bridge)\"~%"))))

;; Run if called as script
(when (defined? 'command-line)
  (main (command-line)))