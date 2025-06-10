#!/usr/bin/env guile
!#
;; WolfCog AGI-OS Stage0 Bootstrap Demonstration
;; This script demonstrates the Stage0 bootstrap functionality
;; Run with: guile demo-bootstrap.scm

(display "🐺🌟 WolfCog AGI-OS Stage0 Bootstrap Demo 🌟🐺\n")
(display "============================================\n\n")

;; Add the bootstrap directory to the load path
(set! %load-path (cons ".guix/bootstrap" %load-path))

;; Attempt to load the bootstrap module
(catch #t
  (lambda ()
    (display "📦 Loading Stage0 bootstrap module...\n")
    (load "stage0-bootstrap.scm")
    (display "✅ Bootstrap module loaded successfully!\n\n")
    
    ;; Demonstrate individual functions
    (display "🔍 Testing individual bootstrap functions:\n\n")
    
    (display "1. Detecting Wolfram kernels:\n")
    (let ((kernels (detect-wolfram-kernels)))
      (if (null? kernels)
          (display "   ⚠️ No Wolfram kernels found (fallback mode)\n")
          (for-each (lambda (kernel)
                      (format #t "   ✓ Found: ~a\n" kernel))
                    kernels)))
    
    (display "\n2. Verifying OpenCog components:\n")
    (let ((components (verify-opencog-unified)))
      (if (null? components)
          (display "   ⚠️ No OpenCog components found\n")
          (for-each (lambda (comp)
                      (format #t "   ✓ Found: ~a -> ~a\n" (car comp) (cdr comp)))
                    components)))
    
    (display "\n3. Testing bootloader configuration:\n")
    (let ((options `((:wolfram-pool . ,(detect-wolfram-kernels))
                    (:opencog-system . ,(verify-opencog-unified)))))
      (present-boot-options options))
    
    (display "\n🚀 Running full Stage0 bootstrap sequence:\n")
    (display "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "=" "\n")
    
    ;; Run the complete bootstrap
    (let ((result (stage0-bootstrap)))
      (display "\n✨ Stage0 Bootstrap Demo Complete! ✨\n")
      (display "📊 System initialized and ready for AGI operations\n")
      (format #t "🔄 Bootstrap result: ~a\n" (assoc-ref result 'bootstrap))
      result))
  
  (lambda (key . args)
    (display "❌ Error loading bootstrap module:\n")
    (format #t "   Key: ~a\n" key)
    (format #t "   Args: ~a\n" args)
    (display "\n💡 This is expected if Guile environment is not properly set up.\n")
    (display "   To run this demo properly:\n")
    (display "   1. Install Guix: curl https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh | sh\n")
    (display "   2. Enter environment: guix shell -m .guix/manifest.scm\n")
    (display "   3. Run demo: guile demo-bootstrap.scm\n")
    #f))

(display "\n🎯 Demo completed. Check above for results or errors.\n")
(display "📚 See docs/guix-bootstrap.md for full documentation.\n")