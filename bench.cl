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

(defun uri-perf-parse (n)
  (dotimes (i n)
    (parse-uri #.*test-uri-string-1*)
    (parse-uri #.*test-uri-string-2*)
    (parse-uri #.*test-uri-string-3*)))

(eval-when (compile eval)
(defmacro test-clear-cache (uri)
  ;; forces no caching of the printed representation:
  `(setf (net.uri::uri-string ,uri) nil))
)

(defun uri-perf-render-uri (n)
  (dotimes (i n)
    (test-clear-cache #.*test-uri-1*)
    (render-uri #.*test-uri-1* nil)
    (test-clear-cache #.*test-uri-2*)
    (render-uri #.*test-uri-2* nil)
    (test-clear-cache #.*test-uri-3*)
    (render-uri #.*test-uri-3* nil)))

(defun uri-perf-format (n &aux (*print-pretty* nil))
  (dotimes (i n)
    (test-clear-cache #.*test-uri-1*)
    (format nil "~a" #.*test-uri-1*)
    (test-clear-cache #.*test-uri-2*)
    (format nil "~a" #.*test-uri-2*)
    (test-clear-cache #.*test-uri-3*)
    (format nil "~a" #.*test-uri-3*)))

(defun uri-perf-slot-access (n)
  (declare (optimize (speed 3)))
  (let ((u (excl::fast *test-uri-1*))
	a b c d e f g)
    (dotimes (i n)
      (setq a (uri-scheme u)
	    b (uri-userinfo u)
	    c (uri-port u)
	    d (uri-path u)
	    e (uri-query u)
	    f (uri-fragment u)
	    g (uri-plist u)
	    ;; uri-authority is not an accessor
	    ))
    ;; use the values so the compiler doesn't optimize away the slot
    ;; accesses
    (values a b c d e f g)))

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
    (time (funcall func n))))

;; Number of iterations set so that each test takes ~10-12s on walter
;; (non-smp), for the original 10.1 implementation.
(defun run-bm1 () (do-bm                 800000 #'uri-perf-parse))
(defun run-bm2 () (do-bm                2400000 #'uri-perf-render-uri))
(defun run-bm3 () (do-bm                1250000 #'uri-perf-format))
(defun run-bm4 () (do-bm #.most-positive-fixnum #'uri-perf-slot-access))

(defun run-bms ()
  (run-bm1)
  (run-bm2)
  (run-bm3)
  (run-bm4))

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
  (profile-bm1)
  (profile-bm2)
  (profile-bm3)
  (profile-bm4))

(defun profile-all ()
  ;; Makes the output more useful...
  (push "new_standard_instance" prof:*hidden-functions*)
  (push "(lisp-heap)" prof:*hidden-functions*)
  (push "start" prof:*hidden-functions*)
  (push "(lisp-trampolines)" prof:*hidden-functions*)
  (push "(after-libnss_dns.so.2-lib)" prof:*hidden-functions*)
  (profile-all-bm1)
  (profile-all-bm2)
  (profile-all-bm3)
  (profile-all-bm4))

(defun profile-bm1 ()
  (do-profile :time #'run-bm1))

(defun profile-all-bm1 ()
  (do-profile :space #'run-bm1)
  (do-profile :time #'run-bm1))

(defun profile-bm2 ()
  (do-profile :time #'run-bm2))

(defun profile-all-bm2 ()
  (do-profile :space #'run-bm2)
  (do-profile :time #'run-bm2))

(defun profile-bm3 ()
  (do-profile :time #'run-bm3))

(defun profile-all-bm3 ()
  (do-profile :space #'run-bm3)
  (do-profile :time #'run-bm3))

(defun profile-bm4 ()
  (do-profile :time #'run-bm4))

(defun profile-all-bm4 ()
  (do-profile :space #'run-bm4)
  (do-profile :time #'run-bm4))
