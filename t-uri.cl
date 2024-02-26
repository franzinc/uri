;; See the file LICENSE for the full license governing this code.

(eval-when (compile load eval)
  (require :tester)
  (require :uri)
  (require :aserve))

(in-package :util.test)

(eval-when (compile eval load)
  (use-package :util.test)
  (use-package :net.uri))

(defun test-uri ()
  (with-tests (:name "uri")
    (all-uri-tests)
    ;; IRI tests only work in a :char16 lisp
    #-(version>= 12 0)
    (ics-target-case
     (:+ics (all-iri-tests)))
    #+(version>= 12 0)
    (with-style-case :char
      (:char16 (all-iri-tests)))))

(defun all-iri-tests ()
  (let* ((path (format nil "/~c~c~c" (code-char #x926) (code-char #x941)
		       (code-char #x92a)))
	 (s (format nil "https://unicodelookup.com~a" path))
	 i)
    (test s (iri-to-string (setq i (string-to-iri s)))
	  :test #'string=)
    (test 'net.uri::iri (type-of i) :test #'eq)
    (test path (uri-path i) :test #'string=)))

(defun all-uri-tests ()
  ;; rfe16295
  (let ((s "https://web.archive.org/web/20131203162826/http://www.apl.jhu.edu/~hall/lisp.html"))
    (handler-case (parse-uri s)
      (uri-parse-error (c)
	(test s (uri-parse-error-string c) :test #'string=))
      (error (c)
	(error "wrong type of error signaled for parse: ~s ~s"
	       c s))))
  
  ;; bug26155
  (test "foo%25bar"
	(uri-path (parse-uri "foo%25bar"))
	:test #'string=)
  (test "/test/foo%25bar.lisp"
	(uri-to-string (string-to-uri "/test/foo%25bar.lisp"))
	:test #'string=)
  (test "/test/foo%25bar.lisp"
	(render-uri (parse-uri "/test/foo%25bar.lisp") nil)
	:test #'string=)
  
  ;; rfe15844
  (let ((s ;; uses +, = and & all encoded:
	 "http://franz.com/foo?val=a%2b%3d%26b+is+c"))
    (test s (render-uri (parse-uri s) nil) :test #'string=))
	 
  ;; bug25523
  (let ((s "https://foo.com/bar/"))
    (test s (let ((*current-case-mode* :case-insensitive-upper))
	      (net.uri:render-uri (net.uri:parse-uri s) nil))
	  :test #'string=))  

  ;; Test random URIs can be parsed and are the same when printed
  (dolist (s (list "http://localhost:42398/pptest#asdfasdf"
		   ;; IPvFuture:
		   "http://[v6.fe80::a_en1]"))
    (test s (render-uri (parse-uri s) nil)
	  :test #'string=))
  
  (dolist (xx ;; (list user-info ipaddr port)
	      '((nil "192.132.95.22" nil)
		(nil "192.132.95.22" 81)
		("layer" "192.132.95.22" nil)
		("layer" "192.132.95.22" 81)
		("layer:pass" "192.132.95.22" nil)
		("layer:pass" "192.132.95.22" 81)
		(nil "fe80::230:48ff:feb9:bbea" nil)
		(nil "fe80::230:48ff:feb9:bbea" 81)
		(nil "2001:470:1f05:548:230:48ff:feb9:bbea" nil)
		(nil "2001:470:1f05:548:230:48ff:feb9:bbea" 81)
		(nil "::1" nil)
		(nil "::1" 81)))
    (destructuring-bind (user-info host port) xx
      (let* ((h (if* (match-re ":" host :return nil)
		   then (format nil "[~a]" host)
		   else host))
	     (s (format nil "https://~@[~a@~]~a~@[:~d~]/foo.html"
			user-info h port))
	     (u (net.uri:parse-uri s)))
	(format t ";; ~a~%" s)
	(test s (princ-to-string u) :test #'string=)
	(test host (net.uri:uri-host u) :test #'string=)
	(when user-info
	  (test user-info (net.uri:uri-userinfo u) :test #'string=))
	(test port (net.uri:uri-port u)))))

  ;; make sure the IPv6 and zone ID are returned properly:
  (let* ((s "http://[2001:470:1f05:548:230:48ff:feb9:bbea%25en0]/foo.cl")
	 (u (parse-uri s)))
    (test "2001:470:1f05:548:230:48ff:feb9:bbea%en0"
	  (net.uri:uri-host u)
	  :test #'string=)
    (test "en0" (net.uri:uri-zone-id u) :test #'string=)
    (test s (net.uri:render-uri u nil) :test #'string=))
  
  (dolist (x (list "http://layer:pw@foo.franz.com:81/foo/bar.cl"
		   "http://layer:pw@foo.franz.com:81/foo/bar.cl"
		   "http://[::1%25en0]:81/foo/bar.cl"))
    ;; make sure u's slots don't end up in the final URI
    (let* ((u (parse-uri x))
	   (s "http://[2001:470:1f05:548:230:48ff:feb9:bbea%25en0]/foo.cl")
	   (u2 (parse-uri s))
	   (u3 (copy-uri u2 :place u)))
      (test "2001:470:1f05:548:230:48ff:feb9:bbea%en0"
	    (net.uri:uri-host u3)
	    :test #'string=)
      (test "en0" (net.uri:uri-zone-id u3) :test #'string=)
      (test s (net.uri:render-uri u3 nil) :test #'string=)))
  
  (test :mailto
	(net.uri:uri-scheme
	 (net.uri:parse-uri "mailto:support@franz.com"))
	:test #'eq)
  (test "support@franz.com"
	(net.uri:uri-path
	 (net.uri:parse-uri "mailto:support@franz.com"))
	:test #'string=)
  ;; bug25618
  (test "test" (net.uri:uri-host (net.uri:parse-uri "http://test#"))
	:test #'string=)
  ;; bug19680
  (test
   "tag:govshare.info,2005:rdf/census/"
   (princ-to-string
    (net.uri:parse-uri "tag:govshare.info,2005:rdf/census/"))
   :test #'string=)
  
  ;; bug11527/bug18546
  (test
   "/simple-form?text=%F3%D4%D2%CF"
   (princ-to-string
    (net.uri:parse-uri "/simple-form?text=%F3%D4%D2%CF"))
   :test #'string=)
  
  (let ((s "/simple-form?text=%F3%D4%D2%CF"))
    (test s (uri-to-string (string-to-uri s)) :test #'string=))
  
  ;; bug18582
  (test "foo:bar"
	(net.uri:uri-userinfo
	 (net.uri:parse-uri "http://foo:bar@localhost/"))
	:test #'string=)
  (test "foo"
	(net.uri:uri-userinfo
	 (net.uri:parse-uri "http://foo@localhost/"))
	:test #'string=)
  (test "foo:bar@localhost"
	(net.uri:uri-authority
	 (net.uri:parse-uri "http://foo:bar@localhost/"))
	:test #'string=)
  (test "foo@localhost"
	(net.uri:uri-authority
	 (net.uri:parse-uri "http://foo@localhost/"))
	:test #'string=)
  (test "http://user:fooa@host:800/foo/bar/foo"
   (princ-to-string
    (net.uri:parse-uri "http://user:foo%61@host:800/foo/bar/foo"))
   :test #'string=)
  (test "http://user:fooa@host:800/foo/bar/foo"
	(uri-to-string
	 (string-to-uri "http://user:foo%61@host:800/foo/bar/foo"))
	:test #'string=)
  (test
   "http://user@host:800/foo/bar/foo"
   (princ-to-string
    (net.uri:parse-uri "http://user@host:800/foo/bar/foo"))
   :test #'string=)
  (test
   "http://user:fooa@host/foo/bar/foo"
   (princ-to-string
    (net.uri:parse-uri "http://user:foo%61@host/foo/bar/foo"))
   :test #'string=)
  (test "http://user:fooa@host/foo/bar/foo"
	(uri-to-string (string-to-uri "http://user:foo%61@host/foo/bar/foo"))
	:test #'string=)
  (test
   "http://user@host/foo/bar/foo"
   (princ-to-string
    (net.uri:parse-uri "http://user@host/foo/bar/foo"))
   :test #'string=)
  
  ;; bug18546
  (test "http://localhost/?x=%0Ax%20&y=1"
	(princ-to-string
	 (net.uri:parse-uri
	  "http://localhost/?x=%0Ax%20&y=1"))
	:test #'string=)
  (test "http://localhost/?x=%0Ax%20&y=1"
	(uri-to-string (string-to-uri "http://localhost/?x=%0Ax%20&y=1"))
	:test #'string=)
  
  ;; bug18153
  (let* ((p (pathname "/foo/the/bar/foo the bar.cl"))
	 (u (net.uri:parse-uri "file:///foo/the/bar/foo%20the%20bar.cl"))
	 (u2 (net.uri:pathname-to-uri p)))
    (test u u2 :test #'uri=)
    (test (namestring p)
	  (namestring (net.uri:uri-to-pathname u2))
	  :test #'string=)
    (test (namestring p)
	  (namestring (net.uri:uri-to-pathname u))
	  :test #'string=))
  
  ;; bug17764
  #+mswindows
  (progn
    (test
     "file://C:/a/fruit/apple"
     (princ-to-string
      (net.uri:merge-uris "fruit/apple" "file://C:/a/sample-file.rdf"))
     :test #'string=)
    (test
     (net.uri:parse-uri "file://C:/a/fruit/apple")
     (net.uri:merge-uris "fruit/apple" "file://C:/a/sample-file.rdf")
     :test #'uri=))
   
  (test "file:///"
	(princ-to-string (net.uri:parse-uri "file:///"))
	:test #'string=)
  (test "file:///foo"
	(princ-to-string (net.uri:parse-uri "file:///foo"))
	:test #'string=)
  (test "file:///foo/bar"
	(princ-to-string (net.uri:parse-uri "file:///foo/bar"))
	:test #'string=)
  (test "hdfs:///"
	(princ-to-string (net.uri:parse-uri "hdfs:///"))
	:test #'string=)
  (test "hdfs:///foo"
	(princ-to-string (net.uri:parse-uri "hdfs:///foo"))
	:test #'string=)
  (test "hdfs:///foo/bar"
	(princ-to-string (net.uri:parse-uri "hdfs:///foo/bar"))
	:test #'string=)
  
  ;; bug17757
  #+mswindows
  (progn
    (test
     "file://c:/cygwin/blah/blarg/scratch/sample-file.rdf"
     (princ-to-string
      (net.uri:parse-uri
       "file://c:/cygwin/blah/blarg/scratch/sample-file.rdf"))
     :test #'string=)
    (test "file://c:/"
	  (princ-to-string (net.uri:parse-uri "file://c:/"))
	  :test #'string=))
  
  ;; bug17650
  (test
   (net.uri:parse-uri
    "http://en.wikipedia.org/wiki/100%25_Dynamite")
   (merge-uris (net.uri:parse-uri
		"http://en.wikipedia.org/wiki/100%25_Dynamite")
	       (net.uri:parse-uri "file:///tmp/test.nt"))
   :test #'uri=)
  
  (let ((net.uri::*render-include-slash-on-null-path* t))
    (dolist (s '("http://www.franz.com"
		 "http://www.franz.com/"
		 "http://www.franz.com:/"
		 "http://www.franz.com:80/"))
      (test "http://www.franz.com/"
	    (render-uri (net.uri:parse-uri s) nil)
	    :test #'string=)))
  
  (test
   "urn:bar:foo"
   (render-uri (net.uri:parse-uri "urn:bar:foo") nil)
   :test #'string=)
  
  ;; bug17407:
  (test "/"
	(net.uri:uri-path
	 (net.uri:parse-uri "http://example.org/#Andr"))
	:test #'string=)
  (test "http://example.org/#Andr"
	(net.uri::render-uri (net.uri:parse-uri
			      "http://example.org/#Andr")
			     nil)
	:test #'string=)
  
  ;; bug16660:
  (let ((u (net.uri:parse-uri "foo://bar:10/baz")))
    (test :foo (net.uri:uri-scheme u)
	  :test #'eq))
  
  ;; bug21361
  (let ((u1 (net.uri:parse-uri "http://www.exampe.com/path#"))
	(u2 (net.uri:parse-uri "http://www.exampe.com/path")))
    (test u1 u2 :test (lambda (a b)
			(not (uri= a b)))))

  (unintern-uri t)
    
;;;; urn examples from RFC 8141
  (let ((urn (parse-uri "urn:example:1/406/47452/2")))
    (test "example" (urn-nid urn) :test #'string=)
    (test "1/406/47452/2" (urn-nss urn) :test #'string=))
       
  (let ((urn (parse-uri "urn:example:foo-bar-baz-qux?+CCResolve:cc=uk")))
    (test "example" (urn-nid urn) :test #'string=)
    (test "foo-bar-baz-qux" (urn-nss urn) :test #'string=)
    (test "CCResolve:cc=uk" (urn-r-component urn) :test #'string=))
       
  (let* ((s "urn:example:weather?=op=map&lat=39.56&lon=-104.85&datetime=1969-07-21T02:56:15Z")
	 (urn (parse-uri s)))
    (test "example" (urn-nid urn) :test #'string=)
    (test "weather" (urn-nss urn) :test #'string=)
    (test "op=map&lat=39.56&lon=-104.85&datetime=1969-07-21T02:56:15Z"
	  (urn-q-component urn) :test #'string=)
    (test s (render-uri urn nil) :test #'string=))
       
  (let* ((s "urn:example:a123,z456")
	 (urn (parse-uri s)))
    (test "example" (urn-nid urn) :test #'string=)
    (test "a123,z456" (urn-nss urn) :test #'string=)
    (test s (render-uri urn nil) :test #'string=))

  (let ((urn (parse-uri "URN:example:a123,z456")))
    (test "example" (urn-nid urn) :test #'string=)
    (test "a123,z456" (urn-nss urn) :test #'string=))

  (let* ((s "urn:example:a12?+abc?=xyz")
	 (urn (parse-uri s)))
    (test "example" (urn-nid urn) :test #'string=)
    (test "a12" (urn-nss urn) :test #'string=)
    (test "abc" (urn-r-component urn) :test #'string=)
    (test "xyz" (urn-q-component urn) :test #'string=)
    (test s (render-uri urn nil) :test #'string=))
       
  (let* ((s "urn:example:a123?+abc?=xyz#foo")
	 (urn (parse-uri s)))
    (test "example" (urn-nid urn) :test #'string=)
    (test "a123" (urn-nss urn) :test #'string=)
    (test "abc" (urn-r-component urn) :test #'string=)
    (test "xyz" (urn-q-component urn) :test #'string=)
    (test "foo" (urn-f-component urn) :test #'string=)
    (test s (render-uri urn nil) :test #'string=))
       
  (test-error ;; can't end in -
   (parse-uri "urn:a2345678901234567890123456789-:foo")
   :condition-type 'uri-parse-error)
       
  (test-error ;; too short -- NID min is 2
   (parse-uri "urn:a:foo")
   :condition-type 'uri-parse-error)
       
  (test "ab" (urn-nid (parse-uri "urn:ab:foo"))
	:test #'string=)
  (test "a23456789012345678901234567890" ;; length=30
	(urn-nid
	 (parse-uri "urn:a23456789012345678901234567890:foo"))
	:test #'string=)
  (test "a234567890123456789012345678901" ;; length=31
	(urn-nid
	 (parse-uri "urn:a234567890123456789012345678901:foo"))
	:test #'string=)

  (test "a23456789012345678901234567890-2" ;; length=32
	(urn-nid
	 (parse-uri "urn:a23456789012345678901234567890-2:foo"))
	:test #'string=)
  (test-error ;; can't end in -
   (parse-uri "urn:a234567890123456789012345678901-:foo")
   :condition-type 'uri-parse-error)
       
  (test-error ;; length=33
   (parse-uri "urn:a23456789012345678901234567890123:foo")
   :condition-type 'uri-parse-error)

  (test-clear-computed-uri-slots-hack)
       
  (more-uri-tests))

(defun test-clear-computed-uri-slots-hack ()
  ;; Need to test setting these slots:
  ;;   scheme, userinfo, port, path, query and fragment
  ;; and testing whether the string and parsed-path slots have been
  ;; updated.
  (flet ((doit (generic &aux (u (parse-uri "http://foo.com/bar/")))
	   (format t "test-clear-computed-uri-slots-hack: generic=~s~%"
		   generic)
	   ;; compute the computed slots
	   (identity (format nil "~s" u))
	   (if* generic
	      then (setf (generic-uri-scheme u) :https)
	      else (setf (uri-scheme u) :https))
	   (test nil (net.uri::uri-string u))
	   (test nil (net.uri::uri-hashcode u))
	   (test t (not (null (net.uri::.uri-parsed-path u))))
	   (test "https://foo.com/bar/" (format nil "~a" u) :test #'string=)
    
	   (if* generic
	      then (setf (generic-uri-userinfo u) "joe")
	      else (setf (uri-userinfo u) "joe"))
	   (test nil (net.uri::uri-string u))
	   (test nil (net.uri::uri-hashcode u))
	   (test t (not (null (net.uri::.uri-parsed-path u))))
	   (test "https://joe@foo.com/bar/" (format nil "~a" u) :test #'string=)

	   (if* generic
	      then (setf (generic-uri-port u) 81)
	      else (setf (uri-port u) 81))
	   (test nil (net.uri::uri-string u))
	   (test nil (net.uri::uri-hashcode u))
	   (test t (not (null (net.uri::.uri-parsed-path u))))
	   (test "https://joe@foo.com:81/bar/"
		 (format nil "~a" u) :test #'string=)

	   (if* generic
	      then (setf (generic-uri-path u) "/newpath/")
	      else (setf (uri-path u) "/newpath/"))
	   (test nil (net.uri::uri-string u))
	   (test nil (net.uri::uri-hashcode u))
	   ;; only this one should be nil
	   (test nil (net.uri::.uri-parsed-path u))
	   (test "https://joe@foo.com:81/newpath/"
		 (format nil "~a" u) :test #'string=)

	   (if* generic
	      then (setf (generic-uri-query u) "foo=bar")
	      else (setf (uri-query u) "foo=bar"))
	   (test nil (net.uri::uri-string u))
	   (test nil (net.uri::uri-hashcode u))
	   (test t (not (null (net.uri::.uri-parsed-path u))))
	   (test "https://joe@foo.com:81/newpath/?foo=bar"
		 (format nil "~a" u) :test #'string=)

	   (if* generic
	      then (setf (generic-uri-fragment u) "frag")
	      else (setf (uri-fragment u) "frag"))
	   (test nil (net.uri::uri-string u))
	   (test nil (net.uri::uri-hashcode u))
	   (test t (not (null (net.uri::.uri-parsed-path u))))
	   (test "https://joe@foo.com:81/newpath/?foo=bar#frag"
		 (format nil "~a" u) :test #'string=)

	   ;; This failed before the CLOS fixed index change
	   ;; (already generic)
	   (setf (uri-host u) "foo2.com")
	   (test nil (net.uri::uri-string u))
	   (test nil (net.uri::uri-hashcode u))
	   (test t (not (null (net.uri::.uri-parsed-path u))))
	   (test "https://joe@foo2.com:81/newpath/?foo=bar#frag"
		 (format nil "~a" u) :test #'string=)))
    (doit nil)
    (doit t)))

(defun more-uri-tests ()
  ;; Tests only work if this is nil:
  (assert (null net.uri::*render-include-slash-on-null-path*))
  (let ((base-uri "http://a/b/c/d;p?q"))
    (dolist (x `(;; (relative-uri result base-uri compare-function)
;;;; RFC Appendix 5.4.1 (normal examples)
		 ("g:h"           "g:h" 		  ,base-uri)
		 ("g"             "http://a/b/c/g" 	  ,base-uri)
		 ("./g"           "http://a/b/c/g" 	  ,base-uri)
		 ("g/"            "http://a/b/c/g/" 	  ,base-uri)
		 ("/g"            "http://a/g" 		  ,base-uri) 
		 ("//g"           "http://g" 		  ,base-uri)
		 ("?y"            "http://a/b/c/d;p?y" 	  ,base-uri) 
		 ("g?y"           "http://a/b/c/g?y" 	  ,base-uri)
		 ("#s"            "http://a/b/c/d;p?q#s"  ,base-uri) 
		 ("g#s"           "http://a/b/c/g#s" 	  ,base-uri) 
		 ("g?y#s"         "http://a/b/c/g?y#s" 	  ,base-uri)
		 (";x"            "http://a/b/c/;x" 	  ,base-uri) 
		 ("g;x"           "http://a/b/c/g;x" 	  ,base-uri) 
		 ("g;x?y#s"       "http://a/b/c/g;x?y#s"  ,base-uri)
		 (""               "http://a/b/c/d;p?q"   ,base-uri)
		 ("."             "http://a/b/c/" 	  ,base-uri)
		 ("./"            "http://a/b/c/" 	  ,base-uri) 
		 (".."            "http://a/b/" 	  ,base-uri) 
		 ("../"           "http://a/b/" 	  ,base-uri)
		 ("../g"          "http://a/b/g" 	  ,base-uri) 
		 ("../.."         "http://a/" 		  ,base-uri) 
		 ("../../"        "http://a/" 		  ,base-uri)
		 ("../../g"       "http://a/g" 		  ,base-uri)

;;;; RFC Appendix 5.4.2 (abnormal examples)
		 ("../../../g"    "http://a/g"  	  ,base-uri)
		 ("../../../../g" "http://a/g" 	          ,base-uri)
		 ("/./g"          "http://a/g"   	  ,base-uri)
		 ("/../g"         "http://a/g"            ,base-uri)
		 ("g."            "http://a/b/c/g."       ,base-uri) 
		 (".g"            "http://a/b/c/.g"       ,base-uri) 
		 ("g.."           "http://a/b/c/g.."      ,base-uri)
		 ("..g"           "http://a/b/c/..g"      ,base-uri) 
		 
		 ("./../g"        "http://a/b/g"          ,base-uri) 
		 ("./g/."         "http://a/b/c/g/"       ,base-uri)
		 ("g/./h"         "http://a/b/c/g/h"      ,base-uri) 
		 ("g/../h"        "http://a/b/c/h"        ,base-uri) 
		 ("g;x=1/./y"     "http://a/b/c/g;x=1/y"  ,base-uri)
		 ("g;x=1/../y"    "http://a/b/c/y"        ,base-uri) 

		 ("g?y/./x"       "http://a/b/c/g?y/./x"  ,base-uri)
		 ("g?y/../x"      "http://a/b/c/g?y/../x" ,base-uri) 
		 ("g#s/./x"       "http://a/b/c/g#s/./x"  ,base-uri)
		 ("g#s/../x"      "http://a/b/c/g#s/../x" ,base-uri) 

		 ;; for backward compatibility this is allowed:
		 ;;   "http://a/b/c/g"
		 ;; but we have always returned the correct result.
		 ("http:g"        "http:g"                ,base-uri)

             ;;; added examples
		 ("f/b/baz.htm#foo"
		  "http://a/b/f/b/baz.htm#foo"
		  "http://a/b/c.htm")
		 ("f/b/baz.htm#foo"
		  "http://a/b/f/b/baz.htm#foo"
		  "http://a/b/")
		 ("f/b/baz.htm#foo"
		  "http://a/f/b/baz.htm#foo"
		  "http://a/b")
		 ("f/b;x;y/bam.htm"
		  "http://a/b/c/f/b;x;y/bam.htm"
		  "http://a/b/c/")))
      (format t ";; merge: ~15a ~18a = ~20a~%" (first x) (third x) (second x))
      (test (second x)
	    (render-uri (merge-uris (first x) (third x)) nil)
	    :test #'string=)
      (test (intern-uri (second x))
	    (intern-uri (merge-uris (intern-uri (first x))
				    (intern-uri (third x))))
	    :test 'uri=))

;;;; intern tests
    (dolist (x '(;; default port and specifying the default port are
		 ;; supposed to compare the same:
		 ("http://www.franz.com:80" "http://www.franz.com")
		 ("http://www.franz.com:80" "http://www.franz.com" eq)
		 ;; make sure they're `eq':
		 ("http://www.franz.com:80" "http://www.franz.com" eq)
		 ("http://www.franz.com" "http://www.franz.com" eq)
		 ("http://www.franz.com/foo" "http://www.franz.com/foo" eq)
		 ("http://www.franz.com/foo?bar"
		  "http://www.franz.com/foo?bar" eq)
		 ("http://www.franz.com/foo?bar#baz"
		  "http://www.franz.com/foo?bar#baz" eq)
		 ("http://WWW.FRANZ.COM" "http://www.franz.com" eq)
		 ("http://www.FRANZ.com" "http://www.franz.com" eq)
		 ("http://www.franz.com" "http://www.franz.com" eq)
		 (;; %72 is "r", %2f is "/", %3b is ";"
		  "http://www.franz.com/ba%72%2f%3b;x;y;z/baz/"
		  "http://www.franz.com/bar%2f%3b;x;y;z/baz/" eq)))
      (test (intern-uri (second x))
	    (intern-uri (first x))
	    :test (if* (third x)
		     then (third x)
		     else 'uri=)))

;;;; parsing and equivalence tests
    ;; %26=&, which should not be decoded
    (test (parse-uri "http://foo+bar?baz=b%26lob+bof")
	  (parse-uri "http://foo+bar?baz=b%26lob+bof")
	  :test 'uri=)
    (test (parse-uri "http://www.foo.com?")
	  (parse-uri (parse-uri "http://www.foo.com?")) ; allow ? at end
	  :test 'uri=)
    (test (parse-uri "http://www.foo.com/foo/bar?")
	  (parse-uri (parse-uri "http://www.foo.com/foo/bar?"))
	  :test 'uri=)
    ;; RFC 3986 requires the %26 to be converted to a &, but that is wrong.
    ;; The interpretation should be left to the HTTP spec.
    (test "baz=b%26lob+bof"
	  (uri-query (parse-uri "http://foo+bar?baz=b%26lob+bof"))
	  :test 'string=)
    ;; RFC 3986 requires the %26/%3d to be converted to &/=, but that is
    ;; wrong.
    (test "baz=b%26lob+bof%3d"
	  (uri-query (parse-uri "http://foo+bar?baz=b%26lob+bof%3d"))
	  :test 'string=)
    
    (test (parse-uri "xxx?%41") (parse-uri "xxx?A") :test 'uri=)
    (test "A" (uri-query (parse-uri "xxx?%41")) :test 'string=)

    (test-error (parse-uri " ") :condition-type 'uri-parse-error)
    (test-error (parse-uri "foo ") :condition-type 'uri-parse-error)
    (test-error (parse-uri " foo ") :condition-type 'uri-parse-error)
    (test-error (parse-uri "<foo") :condition-type 'uri-parse-error)
    (test-error (parse-uri "foo>") :condition-type 'uri-parse-error)
    (test-error (parse-uri "<foo>") :condition-type 'uri-parse-error)
    (test-error (parse-uri "%") :condition-type 'uri-parse-error)
    (test-error (parse-uri "foo%xyr") :condition-type 'uri-parse-error)
    (test-error (parse-uri "\"foo\"") :condition-type 'uri-parse-error)
    (test "%20" (format nil "~a" (parse-uri "%20")) :test 'string=)
    (test "&" (format nil "~a" (parse-uri "%26")) :test 'string=)
    (test "foo%23bar" (format nil "~a" (parse-uri "foo%23bar"))
	  :test 'string=)
    (test "foo%23bar#foobar"
	  (format nil "~a" (parse-uri "foo%23bar#foobar"))
	  :test 'string=)

    (let ((net.uri::*strict-parse* nil))
      (test "foo%23bar#foobar#baz"
	    (format nil "~a" (parse-uri "foo%23bar#foobar#baz"))
	    :test 'string=))
    
    (let ((net.uri::*strict-parse* nil))
      (test "http://foo.com/bar"
	    (format nil "~a" (parse-uri "http://foo.com//bar"))
	    :test 'string=))
       
    (let ((net.uri::*strict-parse* t))
      (test "foo%23bar#foobar%23baz"
	    (format nil "~a" (parse-uri "foo%23bar#foobar%23baz"))
	    :test 'string=))
    (let ((net.uri::*strict-parse* nil))
      (test "foo%23bar#foobar#baz"
	    (format nil "~a" (parse-uri "foo%23bar#foobar%23baz"))
	    :test 'string=))
    (test "foo%23bar#foobar/baz"
	  (format nil "~a" (parse-uri "foo%23bar#foobar%2fbaz"))
	  :test 'string=)

    (test "foobar??" (render-uri (parse-uri "foobar??") nil) :test #'string=)
    (test "foobar?foo?" (render-uri (parse-uri "foobar?foo?") nil)
	  :test #'string=)

    (test "http://foo/bAr;3/baz?baf=3"
	  (format nil "~a" (parse-uri "http://foo/b%41r;3/baz?baf=3"))
	  :test 'string=)
    (test '(:absolute ("/bAr" "3") "baz")
	  (uri-parsed-path (parse-uri "http://foo/%2fb%41r;3/baz?baf=3"))
	  :test 'equal)
    (test "/%2FbAr;3/baz"
	    (let ((u (parse-uri "http://foo/%2fb%41r;3/baz?baf=3")))
	      (setf (uri-parsed-path u) '(:absolute ("/bAr" "3") "baz"))
	      (uri-path u))
	    :test 'string=)
    ;; RFC 3986 requires the = sign to be converted, but that is wrong.
    (test "http://www.verada.com:8010/kapow?name=foo%3Dbar%25"
	    (format nil "~a"
		    (parse-uri
		     "http://www.verada.com:8010/kapow?name=foo%3Dbar%25"))
	    :test 'string=)
    (test "ftp://parcftp.xerox.com/pub/pcl/mop/"
	    (format nil "~a"
		    (parse-uri "ftp://parcftp.xerox.com:/pub/pcl/mop/"))
	    :test 'string=)

;;;; enough-uri tests
    (dolist (x `(("http://www.franz.com/foo/bar/baz.htm"
		  "http://www.franz.com/foo/bar/"
		  "baz.htm")
		 ("http://www.franz.com/foo/bar/baz.htm"
		  "http://www.franz.com/foo/bar"
		  "baz.htm")
		 ("http://www.franz.com:80/foo/bar/baz.htm"
		  "http://www.franz.com:80/foo/bar"
		  "baz.htm")
		 ("http://foo/bar/baz.htm" "http://foo/bar"  "baz.htm")
		 ("http://foo/bar/baz.htm" "http://foo/bar/" "baz.htm")
		 ("/foo/bar/baz.htm" "/foo/bar"  "baz.htm")
		 ("/foo/bar/baz.htm" "/foo/bar/" "baz.htm")
		 ("/foo/bar/baz.htm#foo" "/foo/bar/" "baz.htm#foo")
		 ("/foo/bar/baz.htm?bar#foo" "/foo/bar/" "baz.htm?bar#foo")
		 
		 ("http://www.dnai.com/~layer/foo.htm"
		  "http://www.known.net"
		  "http://www.dnai.com/~layer/foo.htm")
		 ("http://www.dnai.com/~layer/foo.htm"
		  "http://www.dnai.com:8000/~layer/"
		  "http://www.dnai.com/~layer/foo.htm")
		 ("http://www.dnai.com:8000/~layer/foo.htm"
		  "http://www.dnai.com/~layer/"
		  "http://www.dnai.com:8000/~layer/foo.htm")
		 ("http://www.franz.com"
		  "http://www.franz.com"
		  "/")))
      (test (parse-uri (third x))
	    (enough-uri (parse-uri (first x))
			(parse-uri (second x)))
	    :test 'uri=))
    
;;;; urn tests, ideas of which are from rfc2141
    (let ((urn "urn:com:foo-the-bar"))
      (test "com" (urn-nid (parse-uri urn)) :test #'string=)
      (test "foo-the-bar" (urn-nss (parse-uri urn)) :test #'string=))
    
    (test-error (parse-uri "urn:") :condition-type 'uri-parse-error)
    (test-error (parse-uri "urn:foo") :condition-type 'uri-parse-error)
    (test-error (parse-uri "urn:foo$") :condition-type 'uri-parse-error)
    (test-error (parse-uri "urn:foo_") :condition-type 'uri-parse-error)
    (test "urn:foo:foo&bar"
		 (format nil "~a" (parse-uri "urn:foo:foo&bar"))
		 :test #'string=)
    (test (parse-uri "URN:foo:a123,456")
		 (parse-uri "urn:foo:a123,456")
		 :test #'uri=)
    (test (parse-uri "URN:foo:a123,456")
		 (parse-uri "urn:FOO:a123,456")
		 :test #'uri=)
    (test (parse-uri "urn:foo:a123,456")
		 (parse-uri "urn:FOO:a123,456")
		 :test #'uri=)
    (test (parse-uri "URN:FOO:a123%2c456")
		 (parse-uri "urn:foo:a123%2C456")
		 :test #'uri=)
    (test nil
	    (uri= (parse-uri "urn:foo:A123,456")
		  (parse-uri "urn:FOO:a123,456")))
    (test nil
	    (uri= (parse-uri "urn:foo:A123,456")
		  (parse-uri "urn:foo:a123,456")))
    (test nil
	    (uri= (parse-uri "urn:foo:A123,456")
		  (parse-uri "URN:foo:a123,456")))
    (test nil
	    (uri= (parse-uri "urn:foo:a123%2C456")
		  (parse-uri "urn:FOO:a123,456")))
    (test nil
	    (uri= (parse-uri "urn:foo:a123%2C456")
		  (parse-uri "urn:foo:a123,456")))
    (test nil
	  (uri= (parse-uri "URN:FOO:a123%2c456")
		(parse-uri "urn:foo:a123,456")))
    (test nil
	    (uri= (parse-uri "urn:FOO:a123%2c456")
		  (parse-uri "urn:foo:a123,456")))
    (test nil
	    (uri= (parse-uri "urn:foo:a123%2c456")
		  (parse-uri "urn:foo:a123,456")))
    
    #+mswindows
    (test t (equal (pathname "d:/foo/bar.cl")
		   (uri-to-pathname (parse-uri "file:///d:/foo/bar.cl"))))

    #+mswindows
    (test t (uri= (parse-uri "file:///d:/foo/bar.cl")
		  (pathname-to-uri (pathname "d:/foo/bar.cl"))))
    
    (test t (equal (pathname "/foo/bar.cl")
		   (uri-to-pathname (parse-uri "file:///foo/bar.cl"))))

    (test t (uri= (parse-uri "file:///foo/bar.cl")
		  (pathname-to-uri (pathname "/foo/bar.cl"))))
    
    ;; not true anymore, due to bug21361
    (test nil (uri= (parse-uri "foo") (parse-uri "foo#")))

    (let ((net.uri::*strict-parse* nil))
      (test-no-error
       (net.uri:parse-uri
	"http://foo.com/bar?a=zip|zop")))
    (test-error
     (net.uri:parse-uri "http://foo.com/bar?a=zip|zop")
     :condition-type 'uri-parse-error)
    
    (let ((net.uri::*strict-parse* nil))
      (test-no-error
       (net.uri:parse-uri
	"http://arc3.msn.com/ADSAdClient31.dll?GetAd?PG=NBCSBU?SC=D2?AN=1.0586041")))
    (let ((net.uri::*strict-parse* nil))
      (test-no-error
       (net.uri:parse-uri
	"http://scbc.booksonline.com/cgi-bin/ndCGI.exe/Develop/pagClubHome.hrfTIOLI_onWebEvent(hrfTIOLI)?selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&selGetClubOffer.TB_OFFER_ID_ITEM=34487%2e0&selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&^CSpCommand.currRowNumber=5&hrfTIOLI=The+Visual+Basic+6+Programmer%27s+Toolkit&SPIDERSESSION=%3f%3f%3f%3f%3f%5f%3f%3f%3f%40%5b%3f%3f%3f%3fBOs%5cH%3f%3f%3f%3f%3f%3f%3f%3f%3fMMpXO%5f%40JG%7d%40%5c%5f%3f%3f%3fECK%5dt%3fLDT%3fTBD%3fDDTxPEToBS%40%5f%5dBDgXVoKBSDs%7cDT%3fK%3fd%3fTIb%7ceHbkeMfh%60LRpO%5cact%5eUC%7bMu%5fyWUGzLUhP%5ebpdWRka%5dFO%3f%5dBopW%3f%40HMrxbMRd%60LOpuMVga%3fv%3fTS%3fpODT%40O&%5euniqueValue=977933764843")))
    (test-error
     (net.uri:parse-uri
      "http://scbc.booksonline.com/cgi-bin/ndCGI.exe/Develop/pagClubHome.hrfTIOLI_onWebEvent(hrfTIOLI)?selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&selGetClubOffer.TB_OFFER_ID_ITEM=34487%2e0&selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&^CSpCommand.currRowNumber=5&hrfTIOLI=The+Visual+Basic+6+Programmer%27s+Toolkit&SPIDERSESSION=%3f%3f%3f%3f%3f%5f%3f%3f%3f%40%5b%3f%3f%3f%3fBOs%5cH%3f%3f%3f%3f%3f%3f%3f%3f%3fMMpXO%5f%40JG%7d%40%5c%5f%3f%3f%3fECK%5dt%3fLDT%3fTBD%3fDDTxPEToBS%40%5f%5dBDgXVoKBSDs%7cDT%3fK%3fd%3fTIb%7ceHbkeMfh%60LRpO%5cact%5eUC%7bMu%5fyWUGzLUhP%5ebpdWRka%5dFO%3f%5dBopW%3f%40HMrxbMRd%60LOpuMVga%3fv%3fTS%3fpODT%40O&%5euniqueValue=977933764843")
     :condition-type 'uri-parse-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-uri)
