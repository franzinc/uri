;; performance testing for URI module

(eval-when (compile eval load)
  (require :uri)
  (use-package :net.uri))

(in-package :user)

(eval-when (compile eval load)
(defparameter *test-uri-string-1*
    "http://www.franz.com/a/b;x;y;z/c/foo?bar=baz&xxx#fooxxxxxxxx")
(defparameter *test-uri-string-2*
    "https://layer:pwintheclear@www.franz.com:81/simple-form?text=%F3%D4%D2%CF")
(defparameter *test-uri-string-3*
    "urn:this-is-an-example-nid:nss-all-the-waaaaaaaaaaaay")
)

(eval-when (compile eval load)
(defparameter *test-uri-1* (parse-uri *test-uri-string-1*))
(defparameter *test-uri-2* (parse-uri *test-uri-string-2*))
(defparameter *test-uri-3* (parse-uri *test-uri-string-3*))
)

(defun uri-perf-parse ()
  (parse-uri #.*test-uri-string-1*)
  (parse-uri #.*test-uri-string-2*)
  (parse-uri #.*test-uri-string-3*))

(eval-when (compile eval)
(defmacro test-clear-cache (uri)
  ;; forces no caching of the printed representation:
  `(setf (net.uri::uri-string ,uri) nil))
)

(defun uri-perf-render-uri ()
  (test-clear-cache #.*test-uri-1*)
  (render-uri #.*test-uri-1* nil)
  (test-clear-cache #.*test-uri-2*)
  (render-uri #.*test-uri-2* nil)
  (test-clear-cache #.*test-uri-3*)
  (render-uri #.*test-uri-3* nil))

(defun uri-perf-format ()
  (test-clear-cache #.*test-uri-1*)
  (format nil "~a" #.*test-uri-1*)
  (test-clear-cache #.*test-uri-2*)
  (format nil "~a" #.*test-uri-2*)
  (test-clear-cache #.*test-uri-3*)
  (format nil "~a" #.*test-uri-3*))

;; make sure the perf tests are correct:
(dolist (s '(#.*test-uri-string-1* #.*test-uri-string-2* #.*test-uri-string-3*))
  (when (not (equal s (render-uri (parse-uri s) nil)))
    (error "verify failed for URI: ~a." s)))

(defun do-bm (n func)
  (format t "~%~%;; Testing ~s..~%" func)
  (dotimes (i 4) (gc))
  (gc t)
  (dotimes (i 3)
    (format t "~%")
    (do-bm-1 n func)))

(defun do-bm-1 (n func)
  (time (dotimes (i n) (funcall func))))

(defun run-bms ()
  ;; Number of iterations set so that each test takes ~10-12s on walter
  ;; (non-smp), for the original 10.1 implementation.
  (do-bm 1050000 #'uri-perf-parse)
  (do-bm 2400000 #'uri-perf-render-uri)
  (do-bm  700000 #'uri-perf-format))

(defun profile1 () (do-bm-1 1050000 #'uri-perf-parse))
(defun profile2 () (do-bm-1 2400000 #'uri-perf-render-uri))
(defun profile3 () (do-bm-1  700000 #'uri-perf-format))

(eval-when (compile)
  (defmacro do-profile (type func)
    `(progn
       (format t "~%~%;;;;;;; ~s ~s~%" ,type ,func)
       (prof:with-profiling (:type ,type) (funcall ,func))
       (prof:show-flat-profile)
       (prof:show-call-graph))))

(defun profile ()
  ;; Makes the output more useful...
  (push "new_standard_instance" prof:*hidden-functions*)
  (push "(lisp-heap)" prof:*hidden-functions*)
  (push "start" prof:*hidden-functions*)
  (push "(lisp-trampolines)" prof:*hidden-functions*)
  (push "(after-libnss_dns.so.2-lib)" prof:*hidden-functions*)
  (do-profile :space #'profile1)
  (do-profile :time #'profile1)
  (do-profile :space #'profile2)
  (do-profile :time #'profile2)
  (do-profile :space #'profile3)
  (do-profile :time #'profile3))
