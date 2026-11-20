;;; Run every tests/test-*.scm file with SRFI-64 and exit non-zero on
;;; failure.  Invoked by `make check' with the load paths set up for
;;; the build tree.

(use-modules (srfi srfi-64)
             (ice-9 ftw)
             (ice-9 match))

(setlocale LC_ALL "")

(define tests-dir (dirname (car (command-line))))

(define test-files
  (sort (scandir tests-dir
                 (lambda (f)
                   (and (string-prefix? "test-" f)
                        (string-suffix? ".scm" f))))
        string<?))

(define failures 0)

(define (run-file f)
  (let ((runner (test-runner-simple)))
    (test-with-runner runner
      (load (string-append tests-dir "/" f)))
    (set! failures (+ failures
                      (test-runner-fail-count runner)
                      (test-runner-xpass-count runner)))))

(for-each run-file test-files)

(format #t "~%~a test file(s), ~a failure(s)~%" (length test-files) failures)
(exit (if (zero? failures) 0 1))
