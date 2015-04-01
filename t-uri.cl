;; copyright (c) 1999-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2015 Franz Inc, Oakland, CA - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the
;; preamble found here:
;;     http://opensource.franz.com/preamble.html
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.

(eval-when (compile load eval)
  (require :test)
  (require :uri)
  (require :aserve))

(in-package :test)

(eval-when (compile eval load) (use-package :net.uri))

(test-set uri
  (format t ";;;;;;; *render-include-slash-on-null-path*=t~%")
  (let ((net.uri::*render-include-slash-on-null-path* t))
    (all-uri-tests))
  (format t ";;;;;;; *render-include-slash-on-null-path*=nil~%")
  (let ((net.uri::*render-include-slash-on-null-path* nil))
    (all-uri-tests)))

(defun all-uri-tests ()
  (util.test:test :mailto
		  (net.uri:uri-scheme
		   (net.uri:parse-uri "mailto:support@franz.com"))
		  :test #'eq)
  (util.test:test "support@franz.com"
		  (net.uri:uri-path
		   (net.uri:parse-uri "mailto:support@franz.com"))
		  :test #'string=)
  ;; bug19680
  (util.test:test
   "tag:govshare.info,2005:rdf/census/"
   (princ-to-string
    (net.uri:parse-uri "tag:govshare.info,2005:rdf/census/"))
   :test #'string=)
  
  ;; bug11527/bug18546
  (util.test:test
   "/simple-form?text=%F3%D4%D2%CF"
   (princ-to-string
    (net.uri:parse-uri "/simple-form?text=%F3%D4%D2%CF"))
   :test #'string=)
  
  ;; bug18582
  (util.test:test "foo:bar"
		  (net.uri:uri-userinfo
		   (net.uri:parse-uri "http://foo:bar@localhost/"))
		  :test #'string=)
  (util.test:test "foo"
		  (net.uri:uri-userinfo
		   (net.uri:parse-uri "http://foo@localhost/"))
		  :test #'string=)
  (util.test:test "foo:bar@localhost"
		  (net.uri:uri-authority
		   (net.uri:parse-uri "http://foo:bar@localhost/"))
		  :test #'string=)
  (util.test:test "foo@localhost"
		  (net.uri:uri-authority
		   (net.uri:parse-uri "http://foo@localhost/"))
		  :test #'string=)
  (util.test:test
   "http://user:fooa@host:800/foo/bar/foo"
   (princ-to-string
    (net.uri:parse-uri "http://user:foo%61@host:800/foo/bar/foo"))
   :test #'string=)
  (util.test:test
   "http://user@host:800/foo/bar/foo"
   (princ-to-string
    (net.uri:parse-uri "http://user@host:800/foo/bar/foo"))
   :test #'string=)
  (util.test:test
   "http://user:fooa@host/foo/bar/foo"
   (princ-to-string
    (net.uri:parse-uri "http://user:foo%61@host/foo/bar/foo"))
   :test #'string=)
  (util.test:test
   "http://user@host/foo/bar/foo"
   (princ-to-string
    (net.uri:parse-uri "http://user@host/foo/bar/foo"))
   :test #'string=)
  
  ;; bug18546
  (util.test:test "http://localhost/?x=%0Ax%20&y=1"
		  (princ-to-string
		   (net.uri:parse-uri
		    "http://localhost/?x=%0Ax%20&y=1"))
		  :test #'string=)
  
  ;; bug18153
  (let* ((p (pathname "/foo/the/bar/foo the bar.cl"))
	 (u (net.uri:parse-uri "file:///foo/the/bar/foo%20the%20bar.cl"))
	 (u2 (net.uri:pathname-to-uri p)))
    (util.test:test u u2 :test #'uri=)
    (util.test:test (namestring p)
		    (namestring (net.uri:uri-to-pathname u2))
		    :test #'string=)
    (util.test:test (namestring p)
		    (namestring (net.uri:uri-to-pathname u))
		    :test #'string=))
  
  ;; bug17764
  #+mswindows
  (progn
    (util.test:test
     "file://C:/a/fruit/apple"
     (princ-to-string
      (net.uri:merge-uris "fruit/apple" "file://C:/a/sample-file.rdf"))
     :test #'string=)
    (util.test:test
     (net.uri:parse-uri "file://C:/a/fruit/apple")
     (net.uri:merge-uris "fruit/apple" "file://C:/a/sample-file.rdf")
     :test #'uri=))
  
  ;; bug17757
  #+mswindows
  (progn
    (util.test:test
     "file://c:/cygwin/blah/blarg/scratch/sample-file.rdf"
     (princ-to-string
      (net.uri:parse-uri
       "file://c:/cygwin/blah/blarg/scratch/sample-file.rdf"))
     :test #'string=)
    (util.test:test "file://c:/"
		    (princ-to-string (net.uri:parse-uri "file://c:/"))
		    :test #'string=)
    (util.test:test "file:///"
		    (princ-to-string (net.uri:parse-uri "file:///"))
		    :test #'string=)
    
    ;; For now, only Windows tests, but they should probably be run on all
    ;; platforms
    (util.test:test-error (net.uri:parse-uri "file://c")
			  :condition-type 'parse-error)
    (util.test:test-error (net.uri:parse-uri "file://c/")
			  :condition-type 'parse-error)
    (util.test:test-error (net.uri:parse-uri "file://c:")
			  :condition-type 'parse-error))
  
  ;; bug17650
  (util.test:test
   (net.uri:parse-uri
    "http://en.wikipedia.org/wiki/100%25_Dynamite")
   (merge-uris (net.uri:parse-uri
		"http://en.wikipedia.org/wiki/100%25_Dynamite")
	       (net.uri:parse-uri "file:///tmp/test.nt"))
   :test #'uri=)
  
  ;; bug17656
  (dolist (s '("http://www.franz.com"
	       "http://www.franz.com/"
	       "http://www.franz.com:/"
	       "http://www.franz.com:80/"))
    (util.test:test
     (if net.uri::*render-include-slash-on-null-path*
	 "http://www.franz.com/"
       "http://www.franz.com")
     (render-uri (net.uri:parse-uri s) nil)
     :test #'string=))
  
  (util.test:test
   "urn:bar:foo"
   (render-uri (net.uri:parse-uri "urn:bar:foo") nil)
   :test #'string=)
  
  ;; bug17407:
  (util.test:test "/"
		  (net.uri:uri-path
		   (net.uri:parse-uri "http://example.org/#Andr"))
		  :test #'string=)
  (util.test:test "http://example.org/#Andr"
		  (net.uri::render-uri (net.uri:parse-uri
					"http://example.org/#Andr")
				       nil)
		  :test #'string=)
  
  ;; bug16660:
  (let ((u (net.uri:parse-uri "foo://bar:10/baz")))
    (util.test:test :foo (net.uri:uri-scheme u)
		    :test #'eq))
  
  ;; bug21361
  (let ((u1 (net.uri:parse-uri "http://www.exampe.com/path#"))
	(u2 (net.uri:parse-uri "http://www.exampe.com/path")))
    (util.test:test u1 u2 :test (lambda (a b)
				  (not (uri= a b)))))

  ;; rfe13043
  (dotimes (i 127)
    (when (= 1 (sbit net.uri::*reserved-characters* i))
      (let* ((ch (code-char i))
	     (u1 (net.uri:parse-uri
		  (concatenate 'simple-string "http://www.example.com/path/with/"
			       (net.aserve:uriencode-string (format nil "special~cchar" ch))))))
	(util.test:test-no-error (net.uri:uri-parsed-path u1)))))

  (unintern-uri t)

  #.
  (let ((res '())
	(base-uri "http://a/b/c/d;p?q"))

    (dolist (x `(;; (relative-uri result base-uri compare-function)
;;;; RFC Appendix C.1 (normal examples)
		 ("g:h"           "g:h" 		  ,base-uri)
		 ("g"             "http://a/b/c/g" 	  ,base-uri)
		 ("./g"           "http://a/b/c/g" 	  ,base-uri)
		 ("g/"            "http://a/b/c/g/" 	  ,base-uri)
		 ("/g"            "http://a/g" 		  ,base-uri) 
		 ("//g"           "http://g" 		  ,base-uri)
;;; The following test was changed and is different the corresponding
;;; example in appendix C of RFC 2396 because of the clarification of this
;;; example given here:
;;; http://www.apache.org/~fielding/uri/rev-2002/issues.html#003-relative-query
;;; NOTE: RFC 3986 fixed the example.
		 ("?y"            "http://a/b/c/d;p?y" 	  ,base-uri) 
		 ("g?y"           "http://a/b/c/g?y" 	  ,base-uri)
		 ("#s"            "http://a/b/c/d;p?q#s"  ,base-uri) 
		 ("g#s"           "http://a/b/c/g#s" 	  ,base-uri) 
		 ("g?y#s"         "http://a/b/c/g?y#s" 	  ,base-uri)
		 (";x"            "http://a/b/c/;x" 	  ,base-uri) 
		 ("g;x"           "http://a/b/c/g;x" 	  ,base-uri) 
		 ("g;x?y#s"       "http://a/b/c/g;x?y#s"  ,base-uri)
		 ("."             "http://a/b/c/" 	  ,base-uri) 
		 ("./"            "http://a/b/c/" 	  ,base-uri) 
		 (".."            "http://a/b/" 	  ,base-uri) 
		 ("../"           "http://a/b/" 	  ,base-uri)
		 ("../g"          "http://a/b/g" 	  ,base-uri) 
		 ("../.."         "http://a/" 		  ,base-uri) 
		 ("../../"        "http://a/" 		  ,base-uri)
		 ("../../g"       "http://a/g" 		  ,base-uri)
;;;; RFC Appendix C.2 (abnormal examples)
		 (""              "http://a/b/c/d;p?q" 	  ,base-uri) 
;;;; RFC 2396 and 3986 differ on this block of 4:
		 ;; these two return the RFC 3986 answer:
		 ("../../../g"    "http://a/g"  	  ,base-uri)
		 ("../../../../g" "http://a/g" 	          ,base-uri)
		 ;; these two return the RFC 2396 answer:
		 ("/./g"          "http://a/./g"   	  ,base-uri)
		 ("/../g"         "http://a/../g"         ,base-uri)
;;;; ...end differing examples
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

		 ("http:g"        "http:g"                ,base-uri)

		 ("foo/bar/baz.htm#foo"
		  "http://a/b/foo/bar/baz.htm#foo"
		  "http://a/b/c.htm")
		 ("foo/bar/baz.htm#foo"
		  "http://a/b/foo/bar/baz.htm#foo"
		  "http://a/b/")
		 ("foo/bar/baz.htm#foo"
		  "http://a/foo/bar/baz.htm#foo"
		  "http://a/b")
		 ("foo/bar;x;y/bam.htm"
		  "http://a/b/c/foo/bar;x;y/bam.htm"
		  "http://a/b/c/")))
      (push `(util.test:test (intern-uri ,(second x))
			     (intern-uri (merge-uris (intern-uri ,(first x))
						     (intern-uri ,(third x))))
			     :test 'uri=)
	    res))

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
		 ("http://www.franz.com" "http://www.franz.com/" eq)
		 (;; %72 is "r", %2f is "/", %3b is ";"
		  "http://www.franz.com/ba%72%2f%3b;x;y;z/baz/"
		  "http://www.franz.com/bar%2f%3b;x;y;z/baz/" eq)))
      (push `(util.test:test (intern-uri ,(second x))
			     (intern-uri ,(first x))
			     :test ',(if* (third x)
					then (third x)
					else 'uri=))
	    res))

;;;; parsing and equivalence tests
    (push `(util.test:test
	    (parse-uri "http://foo+bar?baz=b%26lob+bof")
	    (parse-uri (parse-uri "http://foo+bar?baz=b%26lob+bof"))
	    :test 'uri=)
	  res)
    (push '(util.test:test
	    (parse-uri "http://www.foo.com")
	    (parse-uri (parse-uri "http://www.foo.com?")) ; allow ? at end
	    :test 'uri=)
	  res)
    (push `(util.test:test
	    "baz=b%26lob+bof"
	    (uri-query (parse-uri "http://foo+bar?baz=b%26lob+bof"))
	    :test 'string=)
	  res)
    (push `(util.test:test
	    "baz=b%26lob+bof%3d"
	    (uri-query (parse-uri "http://foo+bar?baz=b%26lob+bof%3d"))
	    :test 'string=)
	  res)
    (push
     `(util.test:test (parse-uri "xxx?%41") (parse-uri "xxx?A") :test 'uri=)
     res)
    (push
     `(util.test:test "A" (uri-query (parse-uri "xxx?%41")) :test 'string=)
     res)

    (push `(util.test:test-error (parse-uri " ")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri "foo ")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri " foo ")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri "<foo")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri "foo>")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri "<foo>")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri "%")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri "foo%xyr")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri "\"foo\"")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test "%20" (format nil "~a" (parse-uri "%20"))
			   :test 'string=)
	  res)
    (push `(util.test:test "&" (format nil "~a" (parse-uri "%26"))
			   :test 'string=)
	  res)
    (push
     `(util.test:test "foo%23bar" (format nil "~a" (parse-uri "foo%23bar"))
		      :test 'string=)
     res)
    (push
     `(util.test:test "foo%23bar#foobar"
		      (format nil "~a" (parse-uri "foo%23bar#foobar"))
		      :test 'string=)
     res)
    (push
     `(util.test:test "foo%23bar#foobar#baz"
		      (format nil "~a" (parse-uri "foo%23bar#foobar#baz"))
		      :test 'string=)
     res)
    (push
     `(util.test:test "foo%23bar#foobar#baz"
		      (format nil "~a" (parse-uri "foo%23bar#foobar%23baz"))
		      :test 'string=)
     res)
    (push
     `(util.test:test "foo%23bar#foobar/baz"
		      (format nil "~a" (parse-uri "foo%23bar#foobar%2fbaz"))
		      :test 'string=)
     res)
    (push `(util.test:test-error (parse-uri "foobar??")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri "foobar?foo?")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test "foobar?%3f"
			   (format nil "~a" (parse-uri "foobar?%3f"))
			   :test 'string=)
	  res)
    (push `(util.test:test
	    "http://foo/bAr;3/baz?baf=3"
	    (format nil "~a" (parse-uri "http://foo/b%41r;3/baz?baf=3"))
	    :test 'string=)
	  res)
    (push `(util.test:test
	    '(:absolute ("/bAr" "3") "baz")
	    (uri-parsed-path (parse-uri "http://foo/%2fb%41r;3/baz?baf=3"))
	    :test 'equal)
	  res)
    (push `(util.test:test
	    "/%2fbAr;3/baz"
	    (let ((u (parse-uri "http://foo/%2fb%41r;3/baz?baf=3")))
	      (setf (uri-parsed-path u) '(:absolute ("/bAr" "3") "baz"))
	      (uri-path u))
	    :test 'string=)
	  res)
    (push `(util.test:test
	    "http://www.verada.com:8010/kapow?name=foo%3Dbar%25"
	    (format nil "~a"
		    (parse-uri
		     "http://www.verada.com:8010/kapow?name=foo%3Dbar%25"))
	    :test 'string=)
	  res)
    (push `(util.test:test
	    "ftp://parcftp.xerox.com/pub/pcl/mop/"
	    (format nil "~a"
		    (parse-uri "ftp://parcftp.xerox.com:/pub/pcl/mop/"))
	    :test 'string=)
	  res)

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
		 ("http:/foo/bar/baz.htm" "http:/foo/bar"  "baz.htm")
		 ("http:/foo/bar/baz.htm" "http:/foo/bar/" "baz.htm")
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
      (push `(util.test:test (parse-uri ,(third x))
			     (enough-uri (parse-uri ,(first x))
					 (parse-uri ,(second x)))
			     :test 'uri=)
	    res))
    
;;;; urn tests, ideas of which are from rfc2141
    (let ((urn "urn:com:foo-the-bar"))
      (push `(util.test:test "com" (urn-nid (parse-uri ,urn))
			     :test #'string=)
	    res)
      (push `(util.test:test "foo-the-bar" (urn-nss (parse-uri ,urn))
			     :test #'string=)
	    res))
    (push `(util.test:test-error (parse-uri "urn:")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri "urn:foo")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri "urn:foo$")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri "urn:foo_")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test-error (parse-uri "urn:foo:foo&bar")
				 :condition-type 'parse-error)
	  res)
    (push `(util.test:test (parse-uri "URN:foo:a123,456")
			   (parse-uri "urn:foo:a123,456")
			   :test #'uri=)
	  res)
    (push `(util.test:test (parse-uri "URN:foo:a123,456")
			   (parse-uri "urn:FOO:a123,456")
			   :test #'uri=)
	  res)
    (push `(util.test:test (parse-uri "urn:foo:a123,456")
			   (parse-uri "urn:FOO:a123,456")
			   :test #'uri=)
	  res)
    (push `(util.test:test (parse-uri "URN:FOO:a123%2c456")
			   (parse-uri "urn:foo:a123%2C456")
			   :test #'uri=)
	  res)
    (push `(util.test:test
	    nil
	    (uri= (parse-uri "urn:foo:A123,456")
		  (parse-uri "urn:FOO:a123,456")))
	  res)
    (push `(util.test:test
	    nil
	    (uri= (parse-uri "urn:foo:A123,456")
		  (parse-uri "urn:foo:a123,456")))
	  res)
    (push `(util.test:test
	    nil
	    (uri= (parse-uri "urn:foo:A123,456")
		  (parse-uri "URN:foo:a123,456")))
	  res)
    (push `(util.test:test
	    nil
	    (uri= (parse-uri "urn:foo:a123%2C456")
		  (parse-uri "urn:FOO:a123,456")))
	  res)
    (push `(util.test:test
	    nil
	    (uri= (parse-uri "urn:foo:a123%2C456")
		  (parse-uri "urn:foo:a123,456")))
	  res)
    (push `(util.test:test
	    nil
	    (uri= (parse-uri "URN:FOO:a123%2c456")
		  (parse-uri "urn:foo:a123,456")))
	  res)
    (push `(util.test:test
	    nil
	    (uri= (parse-uri "urn:FOO:a123%2c456")
		  (parse-uri "urn:foo:a123,456")))
	  res)
    (push `(util.test:test
	    nil
	    (uri= (parse-uri "urn:foo:a123%2c456")
		  (parse-uri "urn:foo:a123,456")))
	  res)
    
    #+mswindows
    (push `(util.test:test
	    t
	    (equal (pathname "d:/foo/bar.cl")
		   (uri-to-pathname (parse-uri "file:///d:/foo/bar.cl"))))
	  res)

    #+mswindows
    (push `(util.test:test
	    t
	    (uri= (parse-uri "file:///d:/foo/bar.cl")
		  (pathname-to-uri (pathname "d:/foo/bar.cl"))))
	  res)
    
    (push `(util.test:test
	    t
	    (equal (pathname "/foo/bar.cl")
		   (uri-to-pathname (parse-uri "file:///foo/bar.cl"))))
	  res)

    (push `(util.test:test
	    t
	    (uri= (parse-uri "file:///foo/bar.cl")
		  (pathname-to-uri (pathname "/foo/bar.cl"))))
	  res)
    
    ;; not true anymore, due to bug21361
    (push `(util.test:test nil
			   (uri= (parse-uri "foo") (parse-uri "foo#")))
	  res)
    
    (push
     '(let ((net.uri::*strict-parse* nil))
       (util.test:test-no-error
	(net.uri:parse-uri
	 "http://foo.com/bar?a=zip|zop")))
     res)
    (push
     '(util.test:test-error
       (net.uri:parse-uri "http://foo.com/bar?a=zip|zop")
       :condition-type 'parse-error)
     res)
    
    (push
     '(let ((net.uri::*strict-parse* nil))
       (util.test:test-no-error
	(net.uri:parse-uri
	 "http://arc3.msn.com/ADSAdClient31.dll?GetAd?PG=NBCSBU?SC=D2?AN=1.0586041")))
     res)
    (push
     '(util.test:test-error
       (net.uri:parse-uri
	"http://arc3.msn.com/ADSAdClient31.dll?GetAd?PG=NBCSBU?SC=D2?AN=1.0586041")
       :condition-type 'parse-error)
     res)
    
    (push
     '(let ((net.uri::*strict-parse* nil))
       (util.test:test-no-error
	(net.uri:parse-uri
	 "http://scbc.booksonline.com/cgi-bin/ndCGI.exe/Develop/pagClubHome.hrfTIOLI_onWebEvent(hrfTIOLI)?selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&selGetClubOffer.TB_OFFER_ID_ITEM=34487%2e0&selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&^CSpCommand.currRowNumber=5&hrfTIOLI=The+Visual+Basic+6+Programmer%27s+Toolkit&SPIDERSESSION=%3f%3f%3f%3f%3f%5f%3f%3f%3f%40%5b%3f%3f%3f%3fBOs%5cH%3f%3f%3f%3f%3f%3f%3f%3f%3fMMpXO%5f%40JG%7d%40%5c%5f%3f%3f%3fECK%5dt%3fLDT%3fTBD%3fDDTxPEToBS%40%5f%5dBDgXVoKBSDs%7cDT%3fK%3fd%3fTIb%7ceHbkeMfh%60LRpO%5cact%5eUC%7bMu%5fyWUGzLUhP%5ebpdWRka%5dFO%3f%5dBopW%3f%40HMrxbMRd%60LOpuMVga%3fv%3fTS%3fpODT%40O&%5euniqueValue=977933764843")))
     res)
    (push
     '(util.test:test-error
       (net.uri:parse-uri
	"http://scbc.booksonline.com/cgi-bin/ndCGI.exe/Develop/pagClubHome.hrfTIOLI_onWebEvent(hrfTIOLI)?selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&selGetClubOffer.TB_OFFER_ID_ITEM=34487%2e0&selGetClubOffer.TB_OFFER_ID_OFFER=344879%2e0&^CSpCommand.currRowNumber=5&hrfTIOLI=The+Visual+Basic+6+Programmer%27s+Toolkit&SPIDERSESSION=%3f%3f%3f%3f%3f%5f%3f%3f%3f%40%5b%3f%3f%3f%3fBOs%5cH%3f%3f%3f%3f%3f%3f%3f%3f%3fMMpXO%5f%40JG%7d%40%5c%5f%3f%3f%3fECK%5dt%3fLDT%3fTBD%3fDDTxPEToBS%40%5f%5dBDgXVoKBSDs%7cDT%3fK%3fd%3fTIb%7ceHbkeMfh%60LRpO%5cact%5eUC%7bMu%5fyWUGzLUhP%5ebpdWRka%5dFO%3f%5dBopW%3f%40HMrxbMRd%60LOpuMVga%3fv%3fTS%3fpODT%40O&%5euniqueValue=977933764843")
       :condition-type 'parse-error)
     res)
    
    `(progn ,@(nreverse res)))
  )

(when *do-test* (do-test "uri" #'test-uri))
