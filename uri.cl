#+(version= 8 1)
(sys:defpatch "uri" 10
  "v1: don't canonicalize away path of / if there are a query or fragment;
v2: handle merging of urns;
v3: handle further case of merging urns and uris;
v4: fix merge-uris escaping; render-uri now a generic function;
v5: add escape keyword to parse-uri;
v6: Fix file://c:/ parsing on Windows;
v7: Fixes to merge-uris of file://c:/.../ pathnames on Windows;
v8: pathname-to-uri/uri-to-pathname handle chars needing escaping;
v9: add newline and linefeed to list of escaped chars;
v10: handle userinfo in authority, fix escaping issues."
  :type :system
  :post-loadable t)

#+(version= 8 2)
(sys:defpatch "uri" 3
  "v1: make canonicalization of / optional for schemes;
v2: handle opaque part parsing (e.g., tag:franz.com,2005:rdf/something/);
v3: don't normalize away a null fragment, on merge remove leading `.' and `..'."
  :type :system
  :post-loadable t)

#+(version= 9 0)
(sys:defpatch "uri" 1
  "v1: don't normalize away a null fragment, on merge remove leading `.' and `..'."
  :type :system
  :post-loadable t)

;; -*- mode: common-lisp; package: net.uri -*-
;; Support for URIs in Allegro.
;; For general URI information see RFC2396.
;;
;; copyright (c) 1999-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2012 Franz Inc, Oakland, CA - All rights reserved.
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

(defpackage :net.uri
  (:use :common-lisp :excl)
  (:export
   #:uri				; the type and a function
   #:uri-p
   #:copy-uri

   #:uri-scheme				; and slots
   #:uri-host
   #:uri-userinfo
   #:uri-port
   #:uri-path
   #:uri-query
   #:uri-fragment
   #:uri-plist
   #:uri-authority			; pseudo-slot accessor

   #:urn				; class
   #:urn-nid
   #:urn-nss
   
   #:*strict-parse*
   #:parse-uri
   #:merge-uris
   #:enough-uri
   #:uri-parsed-path
   #:render-uri

   #:make-uri-space			; interning...
   #:uri-space
   #:uri=
   #:intern-uri
   #:unintern-uri
   #:do-all-uris
   
   #:uri-to-pathname
   #:pathname-to-uri))

(in-package :net.uri)

(eval-when (compile) (declaim (optimize (speed 3))))

(defclass uri ()
  (
;;;; external:
   (scheme :initarg :scheme :initform nil :accessor uri-scheme)
   (host :initarg :host :initform nil :accessor uri-host)
   (userinfo :initarg :userinfo :initform nil :accessor uri-userinfo)
   (port :initarg :port :initform nil :accessor uri-port)
   (path :initarg :path :initform nil :accessor uri-path)
   (query :initarg :query :initform nil :accessor uri-query)
   (fragment :initarg :fragment :initform nil :accessor uri-fragment)
   (plist :initarg :plist :initform nil :accessor uri-plist)

;;;; internal:
   (escaped
    ;; used to prevent unnessary work, looking for chars to escape and
    ;; unescape.
    :initarg :escaped :initform nil :accessor uri-escaped)
   (string
    ;; the cached printable representation of the URI.  It *might* be
    ;; different than the original string, though, because the user might
    ;; have escaped non-reserved chars--they won't be escaped when the URI
    ;; is printed.
    :initarg :string :initform nil :accessor uri-string)
   (parsed-path
    ;; the cached parsed representation of the URI path.
    :initarg :parsed-path
    :initform nil
    :accessor .uri-parsed-path)
   (hashcode
    ;; cached sxhash, so we don't have to compute it more than once.
    :initarg :hashcode :initform nil :accessor uri-hashcode)))

(defclass urn (uri)
  ((nid :initarg :nid :initform nil :accessor urn-nid)
   (nss :initarg :nss :initform nil :accessor urn-nss)))

(eval-when (compile eval)
  (defmacro clear-caching-on-slot-change (name)
    `(defmethod (setf ,name) :around (new-value (self uri))
       (declare (ignore new-value))
       (prog1 (call-next-method)
	 (setf (uri-string self) nil)
	 ,@(when (eq name 'uri-path) `((setf (.uri-parsed-path self) nil)))
	 (setf (uri-hashcode self) nil))))
  )

(clear-caching-on-slot-change uri-scheme)
(clear-caching-on-slot-change uri-host)
(clear-caching-on-slot-change uri-userinfo)
(clear-caching-on-slot-change uri-port)
(clear-caching-on-slot-change uri-path)
(clear-caching-on-slot-change uri-query)
(clear-caching-on-slot-change uri-fragment)


(defmethod make-load-form ((self uri) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-name (class-of self))
     :scheme ,(uri-scheme self)
     :host ,(uri-host self)
     :userinfo ,(uri-userinfo self)
     :port ,(uri-port self)
     :path ',(uri-path self)
     :query ,(uri-query self)
     :fragment ,(uri-fragment self)
     :plist ',(uri-plist self)
     :string ,(uri-string self)
     :parsed-path ',(.uri-parsed-path self)))

(defmethod uri-p ((thing uri)) t)
(defmethod uri-p ((thing t)) nil)

(defun copy-uri (uri
		 &key place
		      (scheme (when uri (uri-scheme uri)))
		      (host (when uri (uri-host uri)))
		      (userinfo (when uri (uri-userinfo uri)))
		      (port (when uri (uri-port uri)))
		      (path (when uri (uri-path uri)))
		      (parsed-path
		       (when uri (copy-list (.uri-parsed-path uri))))
		      (query (when uri (uri-query uri)))
		      (fragment (when uri (uri-fragment uri)))
		      (plist (when uri (copy-list (uri-plist uri))))
		      (class (when uri (class-of uri)))
		 &aux (escaped (when uri (uri-escaped uri))))
  (if* place
     then (setf (uri-scheme place) scheme)
	  (setf (uri-host place) host)
	  (setf (uri-userinfo place) userinfo)
	  (setf (uri-port place) port)
	  (setf (uri-path place) path)
	  (setf (.uri-parsed-path place) parsed-path)
	  (setf (uri-query place) query)
	  (setf (uri-fragment place) fragment)
	  (setf (uri-plist place) plist)
	  (setf (uri-escaped place) escaped)
	  (setf (uri-string place) nil)
	  (setf (uri-hashcode place) nil)
	  place
   elseif (eq 'uri class)
     then ;; allow the compiler to optimize the call to make-instance:
	  (make-instance 'uri
	    :scheme scheme :host host :userinfo userinfo :port port
	    :path path :parsed-path parsed-path
	    :query query :fragment fragment :plist plist
	    :escaped escaped :string nil :hashcode nil)
     else (make-instance class
	    :scheme scheme :host host :userinfo userinfo :port port
	    :path path :parsed-path parsed-path
	    :query query :fragment fragment :plist plist
	    :escaped escaped :string nil :hashcode nil)))

(defmethod uri-parsed-path ((uri uri))
  (when (uri-path uri)
    (when (null (.uri-parsed-path uri))
      (setf (.uri-parsed-path uri)
	(parse-path (uri-path uri) (uri-escaped uri))))
    (.uri-parsed-path uri)))

(defmethod (setf uri-parsed-path) (path-list (uri uri))
  (assert (and (consp path-list)
	       (or (member (car path-list) '(:absolute :relative)
			   :test #'eq))))
  (setf (uri-path uri) (render-parsed-path path-list t))
  (setf (.uri-parsed-path uri) path-list)
  path-list)

(defun uri-authority (uri)
  (when (uri-host uri)
    (let ((*print-pretty* nil))
      (format nil "~@[~a@~]~a~@[:~a~]" (uri-userinfo uri)
	      (uri-host uri) (uri-port uri)))))

(defun uri-nid (uri)
  (if* (equalp "urn" (uri-scheme uri))
     then (uri-host uri)
     else (error "URI is not a URN: ~s." uri)))

(defun uri-nss (uri)
  (if* (equalp "urn" (uri-scheme uri))
     then (uri-path uri)
     else (error "URI is not a URN: ~s." uri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing

(defparameter *excluded-characters*
    '(;; `delims' (except #\%, because it's handled specially):
      #\< #\> #\" #\space #\#
      ;; `unwise':
      #\{ #\} #\| #\\ #\^ #\[ #\] #\`
      ;; common sense:
      #\newline #\linefeed
      ))

(defun reserved-char-vector (chars &key except)
  (do* ((a (make-array 127 :element-type 'bit :initial-element 0))
	(chars chars (cdr chars))
	(c (car chars) (car chars)))
      ((null chars) a)
    (if* (and except (member c except :test #'char=))
       thenret
       else (setf (sbit a (char-int c)) 1))))

(defparameter *reserved-characters*
    (reserved-char-vector
     (append *excluded-characters*
	     '(#\; #\/ #\? #\: #\@ #\& #\= #\+ #\$ #\, #\%))))
(defparameter *reserved-authority-characters*
    (reserved-char-vector
     (append *excluded-characters* '(#\; #\/ #\? #\: #\@))))
(defparameter *reserved-path-characters*
    (reserved-char-vector
     (append *excluded-characters*
	     '(#\;
;;;;The rfc says this should be here, but it doesn't make sense.
	       ;; #\=
	       #\/ #\?))))

(defparameter *reserved-path-conv-characters*
    (reserved-char-vector
     (append *excluded-characters*
	     '(#\; #\? #\: #\@ #\& #\= #\+ #\$ #\, #\%))))

(defparameter *reserved-fragment-characters*
    (reserved-char-vector (remove #\# *excluded-characters*)))

(eval-when (compile eval)
(defun gen-char-range-list (start end)
  (do* ((res '())
	(endcode (1+ (char-int end)))
	(chcode (char-int start)
		(1+ chcode))
	(hyphen nil))
      ((= chcode endcode)
       ;; - has to be first, otherwise it signifies a range!
       (if* hyphen
	  then (setq res (nreverse res))
	       (push #\- res)
	       res
	  else (nreverse res)))
    (if* (= #.(char-int #\-) chcode)
       then (setq hyphen t)
       else (push (code-char chcode) res))))
)

(defparameter *valid-nid-characters*
    (reserved-char-vector
     '#.(nconc (gen-char-range-list #\a #\z)
	       (gen-char-range-list #\A #\Z)
	       (gen-char-range-list #\0 #\9)
	       '(#\- #\. #\+))))
(defparameter *reserved-nss-characters*
    (reserved-char-vector
     (append *excluded-characters* '(#\& #\~ #\/ #\?))))

(defparameter *illegal-characters*
    (reserved-char-vector (remove #\# *excluded-characters*)))
(defparameter *strict-illegal-query-characters*
    (reserved-char-vector (append '(#\?) (remove #\# *excluded-characters*))))
(defparameter *illegal-query-characters*
    (reserved-char-vector
     *excluded-characters* :except '(#\^ #\| #\#)))


(defun parse-uri (thing &key (class 'uri)
			     (escape nil escape-supplied)
                             (canonicalize-schemes '(:http :https :ftp)))
  (when (uri-p thing) (return-from parse-uri thing))
  
  (when (not escape-supplied) (setq escape (escape-p thing)))

  (multiple-value-bind (scheme host userinfo port path query fragment)
      (parse-uri-string thing)
    (when scheme
      (setq scheme
	(intern (funcall
		 (case *current-case-mode*
		   ((:case-insensitive-upper :case-sensitive-upper)
		    #'string-upcase)
		   ((:case-insensitive-lower :case-sensitive-lower)
		    #'string-downcase))
		 (decode-escaped-encoding scheme escape))
		(find-package :keyword))))
    
    (when (and scheme (eq :urn scheme))
      (return-from parse-uri
	(make-instance 'urn :scheme scheme :nid host :nss path)))
    
    (when host (setq host (decode-escaped-encoding host escape)))
    (when userinfo (setq userinfo (decode-escaped-encoding userinfo escape)))
    (when port
      (when (not (numberp port)) (error "port is not a number: ~s." port))
      (when (not (plusp port))
	(error "port is not a positive integer: ~d." port))
      ;; Use `eql' instead of `=' so that scheme's other than the small set
      ;; below are possible.
      (when (eql port (case scheme
			(:http 80)
			(:https 443)
			(:ftp 21)
			(:telnet 23)))
	(setq port nil)))
    (when (or (string= "" path)
	      (and ;; we canonicalize away a reference to just /:
	       scheme
	       (member scheme canonicalize-schemes :test #'eq)
	       (string= "/" path)
	       ;; but only if the rest of the parse didn't see anything
	       (null query)
	       (null fragment)))
      (setq path nil))
    (when path
      (setq path
	(decode-escaped-encoding path escape *reserved-path-characters*)))
    (when query (setq query (decode-escaped-encoding query escape)))
    (when fragment
      (setq fragment
	(decode-escaped-encoding fragment escape
				 *reserved-fragment-characters*)))
    (if* (eq 'uri class)
       then ;; allow the compiler to optimize the make-instance call:
	    (make-instance 'uri
	      :scheme scheme
	      :host host
	      :userinfo userinfo
	      :port port
	      :path path
	      :query query
	      :fragment fragment
	      :escaped escape)
       else ;; do it the slow way:
	    (make-instance class
	      :scheme scheme
	      :host host
	      :userinfo userinfo
	      :port port
	      :path path
	      :query query
	      :fragment fragment
	      :escaped escape))))

(defmethod uri ((thing uri))
  thing)

(defmethod uri ((thing string))
  (parse-uri thing))

(defmethod uri ((thing t))
  (error "Cannot coerce ~s to a uri." thing))

(defvar *strict-parse* t)

;;(eval-when (compile) (pushnew :debug-uri-parse *features*))

(defun parse-uri-string (string &aux (illegal-chars *illegal-characters*))
  (declare (optimize (speed 3)))
  ;; Speed is important, so use a specialized state machine instead of
  ;; regular expressions for parsing the URI string. The regexp we are
  ;; simulating:
  ;;  ^(([^:/?#]+):)?
  ;;   (//([^/?#]*))?
  ;;   ([^?#]*)
  ;;   (\?([^#]*))?
  ;;   (#(.*))?
  (let* ((state 0)
	 (start 0)
	 (end (length string))
	 (tokval nil)
	 (scheme nil)
	 (host nil)
	 (userinfo nil)
	 (port nil)
	 (path-components '())
	 (query nil)
	 (fragment nil)
	 ;; namespace identifier, for urn parsing only:
	 (nid nil))
    (declare (fixnum state start end))
    (labels
	(#+debug-uri-parse
	 (.debug ()
	   (format t "~&state: ~a, tokval=~s~%" state tokval))
	 (read-token (kind &optional legal-chars)
	   #+debug-uri-parse (.debug)
	   (setq tokval nil)
	   (if* (>= start end)
	      then :end
	      else (let ((sindex start)
			 (res nil)
			 c)
		     (declare (fixnum sindex))
		     (setq res
		       (loop
			 (when (>= start end) (return nil))
			 (setq c (schar string start))
			 (let ((ci (char-int c)))
			   (if* legal-chars
			      then (if* (and (eq :colon kind) (eq c #\:))
				      then (return :colon)
				    elseif (= 0 (sbit legal-chars ci))
				      then (excl::.parse-error
					    "~
URI ~s contains illegal character ~s at position ~d."
					    string c start))
			    elseif (and (< ci 128)
					*strict-parse*
					(= 1 (sbit illegal-chars ci)))
			      then (excl::.parse-error "~
URI ~s contains illegal character ~s at position ~d."
						       string c start)))
			 (case kind
			   (:path (case c
				    (#\? (return :question))
				    (#\# (return :hash))))
			   (:query (case c (#\# (return :hash))))
			   (:rest)
			   (:authority
			    (case c
				(#\@ (return :atsign))
				(#\: (return :colon))
				(#\? (return :question))
				(#\# (return :hash))
				(#\/ (return :slash))))
			   (t (case c
				(#\: (return :colon))
				(#\? (return :question))
				(#\# (return :hash))
				(#\/ (return :slash)))))
			 (incf start)))
		     (if* (> start sindex)
			then ;; we found some chars
			     ;; before we stopped the parse
			     (setq tokval (subseq string sindex start))
			     :string
			else ;; immediately stopped at a special char
			     (incf start)
			     res))))
	 (failure (&optional why)
	   (excl::.parse-error "illegal URI: ~s [~d]~@[: ~a~]"
			       string state why))
	 (impossible ()
	   (excl::.parse-error "impossible state: ~d [~s]" state string)))
      (loop
	(case state
	  (0 ;; starting to parse
	   (ecase (read-token t)
	     (:colon (failure))
	     (:question (setq state 7))
	     (:hash (setq state 8))
	     (:slash (setq state 3))
	     (:string (setq state 1))
	     (:end (setq state 9))))
	  (1 ;; seen <token><special char>
	   (let ((token tokval))
	     (ecase (read-token t)
	       (:colon (setq scheme token)
		       (if* (equalp "urn" scheme)
			  then (setq state 15)
			  else (setq state 2)))
	       (:question (push token path-components)
			  (setq state 7))
	       (:hash (push token path-components)
		      (setq state 8))
	       (:slash (push token path-components)
		       (push "/" path-components)
		       (setq state 6))
	       (:string (failure))
	       (:end (push token path-components)
		     (setq state 9)))))
	  (2 ;; seen <scheme>:
	   (ecase (read-token t)
	     (:colon (failure))
	     (:question (setq state 7))
	     (:hash (setq state 8))
	     (:slash (setq state 3))
	     (:string (setq state 10))
	     (:end (setq state 9))))
	  (10 ;; seen <scheme>:<token>
	   (let ((token tokval))
	     (ecase (read-token t)
	       (:colon
		;; what follows the first colon is an opaque-part; we
		;; treat it as a path component.
                (return (values scheme
                                nil
                                nil
                                nil
                                (concatenate 'simple-string
				  token
				  ":"
				  (progn
				    (setq illegal-chars *illegal-characters*)
				    (read-token :rest)
				    tokval))
                                nil)))
	       (:question (push token path-components)
			  (setq state 7))
	       (:hash (push token path-components)
		      (setq state 8))
	       (:slash (push token path-components)
		       (setq state 6))
	       (:string (failure))
	       (:end (push token path-components)
		     (setq state 9)))))
	  (3 ;; seen / or <scheme>:/
	   (ecase (read-token t)
	     (:colon (failure))
	     (:question (push "/" path-components)
			(setq state 7))
	     (:hash (push "/" path-components)
		    (setq state 8))
	     (:slash (setq state 4))
	     (:string (push "/" path-components)
		      (push tokval path-components)
		      (setq state 6))
	     (:end (push "/" path-components)
		   (setq state 9))))
	  (4 ;; seen [<scheme>:]//
	   (ecase (read-token :authority)
	     (:colon (failure))
	     (:question (failure))
	     (:hash (failure))
	     (:slash
	      (if* (and (equalp "file" scheme)
			(null host))
		 then ;; file:///...
		      (push "/" path-components)
		      (setq state 6)
		 else (failure)))
	     (:string
	      ;; Need to look for c:/...
	      (let ((on-windows #+mswindows t #-mswindows nil)
		    c)
		(if* (and on-windows 
			  (equalp "file" scheme)
			  (= 1 (length tokval))
			  (or (<= #.(char-code #\a)
				  (setq c (char-code (schar tokval 0)))
				  #.(char-code #\z))
			      (<= #.(char-code #\A) c #.(char-code #\Z))))
		   then ;; seen file://x
			(push tokval path-components)
			(setq state 18)
		   else (setq host tokval)
			(setq state 11))))
	     (:end (failure))))
	  #+mswindows
	  (18 ;; seen file://x
	   (ecase (read-token t)
	     (:colon (setq state 19))
	     (:question (failure))
	     (:hash (failure))
	     (:slash (failure))
	     (:string (failure))
	     (:end (failure))))
	  #+mswindows
	  (19 ;; seen file://x:
	   (ecase (read-token t)
	     (:colon (failure))
	     (:question (failure))
	     (:hash (failure))
	     (:slash ;; file://x:/
	      (push ":/" path-components)
	      (setq state 6))
	     (:string (failure))
	     (:end (failure))))
	  (11 ;; seen [<scheme>:]//<something>
	   ;; Hack alert.  When I added handling of `userinfo', I did so
	   ;; with a minimum of changes.  Instead of a <host> we now assume
	   ;; that at this point we have either a `host' or `userinfo'.  It
	   ;; would be better to change the FSM to look for the items
	   ;; allowed by the RFC(2396) at this stage.  In state 12, if we
	   ;; see an `@', then we'll go back to state 4 to get the host,
	   ;; however some valid `userinfo' values will be flagged as
	   ;; errors (one that starts with a colon, for example).
	   (ecase (read-token :authority)
	     (:atsign
	      (if* userinfo
		 then ;; already read host:port once
		      (failure)
		 else ;; haven't read host:port, but user:passwd
		      (setq userinfo host)
		      (setq host nil)
		      (setq state 4)))
	     (:colon (setq state 5))
	     (:question (setq state 7))
	     (:hash (setq state 8))
	     (:slash (push "/" path-components)
		     (setq state 6))
	     (:string (impossible))
	     (:end (setq state 9))))
	  (5 ;; seen [<scheme>:]//<host-or-userinfo>:
	   (ecase (read-token :authority)
	     (:colon (failure))
	     (:question (failure))
	     (:hash (failure))
	     (:slash (push "/" path-components)
		     (setq state 6))
	     (:string (setq port tokval)
		      (setq state 12))
	     (:end (failure))))
	  (12 ;; seen [<scheme>:]//<host-or-userinfo>:[<port-or-password>]
	   (ecase (read-token :authority)
	     (:atsign
	      (if* userinfo
		 then ;; already read host:port once
		      (failure)
		 else ;; haven't read host:port, but user:passwd
		      (setq userinfo (concatenate 'simple-string host ":" port))
		      (setq host nil port nil)
		      (setq state 4)))
	     (:colon (failure))
	     (:question (setq state 7))
	     (:hash (setq state 8))
	     (:slash (push "/" path-components)
		     (setq state 6))
	     (:string (impossible))
	     (:end (setq state 9))))
	  (6 ;; seen /
	   (ecase (read-token :path)
	     (:question (setq state 7))
	     (:hash (setq state 8))
	     (:string (push tokval path-components)
		      (setq state 13))
	     (:end (setq state 9))))
	  (13 ;; seen path
	   (ecase (read-token :path)
	     (:question (setq state 7))
	     (:hash (setq state 8))
	     (:string (impossible))
	     (:end (setq state 9))))
	  (7 ;; seen ?
	   (setq illegal-chars
	     (if* *strict-parse*
		then *strict-illegal-query-characters*
		else *illegal-query-characters*))
	   (ecase (prog1 (read-token :query)
		    (setq illegal-chars *illegal-characters*))
	     (:hash (setq state 8))
	     (:string (setq query tokval)
		      (setq state 14))
	     (:end (setq state 9))))
	  (14 ;; query
	   (ecase (read-token :query)
	     (:hash (setq state 8))
	     (:string (impossible))
	     (:end (setq state 9))))
	  (8 ;; seen #
	   (ecase (read-token :rest)
	     (:string (setq fragment tokval)
		      (setq state 9))
	     (:end (setq fragment "" ;; rfe11851
			 state 9))))
	  (9 ;; done
	   (when port
	     (multiple-value-bind (int length)
		 (parse-integer port :junk-allowed t :radix 10)
	       (when (and int (= length (length port)))
		 (setq port int))))
	   (return
	     (values
	      scheme host userinfo port
	      (apply #'concatenate 'simple-string (nreverse path-components))
	      query fragment)))
	  ;; URN parsing:
	  (15 ;; seen urn:, read nid now
	   (case (read-token :colon *valid-nid-characters*)
	     (:string (setq nid tokval)
		      (setq state 16))
	     (t (failure "missing namespace identifier"))))
	  (16 ;; seen urn:<nid>
	   (case (read-token t)
	     (:colon (setq state 17))
	     (t (failure "missing namespace specific string"))))
	  (17 ;; seen urn:<nid>:, rest is nss
	   (return (values scheme
			   nid
			   nil
			   nil
			   (progn
			     (setq illegal-chars *reserved-nss-characters*)
			     (read-token :rest)
			     tokval))))
	  (t (excl::.parse-error
	      "internal error in parse engine, wrong state: ~s." state)))))))

(defun escape-p (string)
  (declare (optimize (speed 3)))
  (do* ((i 0 (1+ i))
	(max (the fixnum (length string))))
      ((= i max) nil)
    (declare (fixnum i max))
    (when (char= #\% (schar string i))
      (return t))))

(defun parse-path (path-string escape)
  (do* ((xpath-list (delimited-string-to-list path-string #\/))
	(path-list
	 (let (#+mswindows temp #+mswindows c)
	   (cond ((string= "" (car xpath-list))
		  (setf (car xpath-list) :absolute))
		 #+mswindows
		 ((and (= 2 (length (setq temp (car xpath-list))))
		       (char= #\: (char temp 1))
		       (or (<= #.(char-code #\a)
			       (setq c (char-code (char temp 0)))
			       #.(char-code #\z))
			   (<= #.(char-code #\A) c #.(char-code #\Z))))
		  ;; Starts with Windows drive letter
		  (setf (car xpath-list)
		    (intern (make-string 1 :initial-element (char temp 0))
			    (load-time-value (find-package :keyword))))
		  (push :absolute xpath-list))
		 (t (push :relative xpath-list)))
	   xpath-list))
	(pl (cdr path-list) (cdr pl))
	segments)
      ((null pl) path-list)
    
    (if* (symbolp (car pl))
       then ;; Only happens on Windows when we see a path with a drive
	    ;; letter.  The lack of #+mswindows doesn't matter here.
	    nil
     elseif (cdr (setq segments
		   (if* (string= "" (car pl))
		      then '("")
		      else (delimited-string-to-list (car pl) #\;))))
       then ;; there is a param
	    (setf (car pl)
	      (mapcar #'(lambda (s)
			  (decode-escaped-encoding s escape
						   ;; decode all %xx:
						   nil))
		      segments))
       else ;; no param
	    (setf (car pl)
	      (decode-escaped-encoding (car segments) escape
				       ;; decode all %xx:
				       nil)))))

(defun decode-escaped-encoding (string escape
				&optional (reserved-chars
					   *reserved-characters*))
  ;; Return a string with the real characters.
  (when (null escape) (return-from decode-escaped-encoding string))
  (do* ((i 0 (1+ i))
	(max (length string))
	(new-string (copy-seq string))
	(new-i 0 (1+ new-i))
	ch ch2 chc chc2)
      ((= i max)
       (excl::.primcall 'sys::shrink-svector new-string new-i)
       new-string)
    (if* (char= #\% (setq ch (schar string i)))
       then (when (> (+ i 3) max)
	      (excl::.parse-error
	       "Unsyntactic escaped encoding in ~s." string))
	    (setq ch (schar string (incf i)))
	    (setq ch2 (schar string (incf i)))
	    (when (not (and (setq chc (digit-char-p ch 16))
			    (setq chc2 (digit-char-p ch2 16))))
	      (excl::.parse-error
	       "Non-hexidecimal digits after %: %c%c." ch ch2))
	    (let ((ci (+ (* 16 chc) chc2)))
	      (if* (or (null reserved-chars)
		       (and (<= ci 127)	; bug11527
                            (= 0 (sbit reserved-chars ci))))
		 then ;; ok as is
		      (setf (schar new-string new-i)
			(code-char ci))
		 else (setf (schar new-string new-i) #\%)
		      (setf (schar new-string (incf new-i)) ch)
		      (setf (schar new-string (incf new-i)) ch2)))
       else (setf (schar new-string new-i) ch))))

#+ignore
(defun decode (string)
  (decode-escaped-encoding string t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Printing

;; bootstrapping, for change to make render-uri a generic function:
#-(version>= 8 2)
(eval-when (compile eval load)
  (when (fboundp 'render-uri) (fmakunbound 'render-uri)))

(defvar *render-include-slash-on-null-path* nil) ;; rfe11850

(defmethod render-uri ((uri uri) stream
		       &aux (escape (uri-escaped uri))
			    (*print-pretty* nil))
  (when (null (uri-string uri))
    (setf (uri-string uri)
      (let ((scheme (uri-scheme uri))
	    (host (uri-host uri))
	    (userinfo (uri-userinfo uri))
	    (port (uri-port uri))
	    (path (uri-path uri))
	    (query (uri-query uri))
	    (fragment (uri-fragment uri)))
	(concatenate 'simple-string
	  (when scheme
	    (encode-escaped-encoding
	     (string-downcase ;; for upper case lisps
	      (symbol-name scheme))
	     *reserved-characters* escape))
	  (when scheme ":")
	  (when (or host (eq :file scheme)) "//")
	  (when userinfo userinfo)
	  (when userinfo "@")
	  (when host
	    (encode-escaped-encoding
	     host *reserved-authority-characters* escape))
	  (when port ":")
	  (when port
	    (with-output-to-string (s) (excl::maybe-print-fast s port)))
	  (if* path
	     then (encode-escaped-encoding path
					   nil
					   ;;*reserved-path-characters*
					   escape)
	   elseif (and *render-include-slash-on-null-path*
		       #|no path but:|# scheme host)
	     then "/")
	  (when query "?")
	  (when query (encode-escaped-encoding query nil escape))
	  (when fragment "#")
	  (when fragment (encode-escaped-encoding fragment nil escape))))))
  (if* stream
     then (princ (uri-string uri) stream)
     else (uri-string uri)))

(defmethod render-uri ((urn urn) stream
		       &aux (*print-pretty* nil))
  (when (null (uri-string urn))
    (setf (uri-string urn)
      (let ((nid (urn-nid urn))
	    (nss (urn-nss urn)))
	(concatenate 'simple-string "urn:" nid ":" nss))))
  (if* stream
     then (write-string (uri-string urn) stream)
     else (uri-string urn)))

(defun render-parsed-path (path-list escape)
  (do* ((res '())
	(first (car path-list))
	(pl (cdr path-list) (cdr pl))
	(pe (car pl) (car pl)))
      ((null pl)
       (when res (apply #'concatenate 'simple-string (nreverse res))))
    (when (or (null first)
	      (prog1 (and (eq :absolute first)
			  ;; Only happens on Windows, in the case of a path
			  ;; with a drive letter in it.  The drive letter
			  ;; element is a keyword naming the drive.
			  (not (keywordp pe)))
		(setq first nil)))
      (push "/" res))
    (if* (symbolp pe)
       then ;; Only happens on Windows.  It's a keyword corresponding to
	    ;; the drive letter.
	    (push (format nil "~a:" pe) res)
     elseif (atom pe)
       then (push
	     (encode-escaped-encoding pe *reserved-path-characters* escape)
	     res)
       else ;; contains params
	    (push (encode-escaped-encoding
		   (car pe) *reserved-path-characters* escape)
		  res)
	    (dolist (item (cdr pe))
	      (push ";" res)
	      (push (encode-escaped-encoding
		     item *reserved-path-characters* escape)
		    res)))))

(defparameter *escaped-encoding*
    (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))

(defun encode-escaped-encoding (string reserved-chars escape)
  (when (null escape) (return-from encode-escaped-encoding string))
  ;; Make a string as big as it possibly needs to be (3 times the original
  ;; size), and truncate it at the end.
  (do* ((max (length string))
	(new-max (* 3 max)) ;; worst case new size
	(new-string (make-string new-max))
	(i 0 (1+ i))
	(new-i -1)
	c ci)
      ((= i max)
       (excl::.primcall 'sys::shrink-svector new-string (incf new-i))
       new-string)
    (setq ci (char-int (setq c (schar string i))))
    (if* (or (null reserved-chars)
	     (and (<= ci 127) (= 0 (sbit reserved-chars ci))))
       then ;; ok as is
	    (incf new-i)
	    (setf (schar new-string new-i) c)
       else ;; need to escape it
	    (multiple-value-bind (q r) (truncate ci 16)
	      (setf (schar new-string (incf new-i)) #\%)
	      (setf (schar new-string (incf new-i)) (elt *escaped-encoding* q))
	      (setf (schar new-string (incf new-i))
		(elt *escaped-encoding* r))))))

(defmethod print-object ((uri uri) stream)
  (if* *print-escape*
     then (format stream "#<~a ~a>"
		  (class-name (class-of uri))
		  (render-uri uri nil))
     else (render-uri uri stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; merging and unmerging

(defmethod merge-uris ((uri string) (base string) &optional place)
  (merge-uris (parse-uri uri) (parse-uri base) place))

(defmethod merge-uris ((uri uri) (base string) &optional place)
  (merge-uris uri (parse-uri base) place))

(defmethod merge-uris ((uri string) (base uri) &optional place)
  (merge-uris (parse-uri uri) base place))

(defmethod merge-uris ((uri uri) (base uri) &optional place)
  ;; See ../doc/rfc2396.txt for info on the algorithm we use to merge
  ;; URIs.
  ;;
  (tagbody
;;;; step 2
    (when (and (null (uri-path uri))
	       (null (uri-scheme uri))
	       (null (uri-host uri))
	       (null (uri-userinfo uri))
	       (null (uri-port uri))
	       (null (uri-query uri)))
      (return-from merge-uris
	(let ((new (copy-uri base :place place)))
	  (when (uri-query uri)
	    (setf (uri-query new) (uri-query uri)))
	  (when (uri-fragment uri)
	    (setf (uri-fragment new) (uri-fragment uri)))
	  new)))
    
    (setq uri (copy-uri uri :place place))

;;;; step 3
    (when (uri-scheme uri)
      (return-from merge-uris uri))
    (setf (uri-scheme uri) (uri-scheme base))
  
;;;; step 4
    (when (uri-host uri) (go :done))
    (setf (uri-host uri) (uri-host base))
    (setf (uri-userinfo uri) (uri-userinfo base))
    (setf (uri-port uri) (uri-port base))
    
;;;; step 5
    (let ((p (uri-parsed-path uri)))
      
      ;; bug13133:
      ;; The following form causes our implementation to be at odds with
      ;; RFC 2396, however this is apparently what was intended by the
      ;; authors of the RFC.  Specifically, (merge-uris "?y" "/foo")
      ;; should return #<uri /foo?y> instead of #<uri ?y>, according to
      ;; this:
;;; http://www.apache.org/~fielding/uri/rev-2002/issues.html#003-relative-query
      (when (null p)
	(setf (uri-path uri) (uri-path base))
	(go :done))
      
      (when (and p (eq :absolute (car p)))
	(when (equal '(:absolute "") p)
	  ;; Canonicalize the way parsing does:
	  (setf (uri-path uri) nil))
	(go :done)))
    
;;;; step 6
    (let* ((base-path
	    (or (uri-parsed-path base)
		;; needed because we canonicalize away a path of just `/':
		'(:absolute "")))
	   (path (uri-parsed-path uri))
	   new-path-list)
      (when (not (eq :absolute (car base-path)))
	(error "Cannot merge ~a and ~a, since the latter is not absolute."
	       uri base))

      ;; steps 6a and 6b:
      (setq new-path-list
	(append (butlast base-path)
		(if* path then (cdr path) else '(""))))

      ;; steps 6c and 6d:
      (let ((last (last new-path-list)))
	(if* (atom (car last))
	   then (when (string= "." (car last))
		  (setf (car last) ""))
	   else (when (string= "." (caar last))
		  (setf (caar last) ""))))
      (setq new-path-list
	(delete "." new-path-list :test #'(lambda (a b)
					    (if* (atom b)
					       then (string= a b)
					       else nil))))

      ;; steps 6e and 6f:
      (let ((npl (cdr new-path-list))
	    index tmp fix-tail)
	(setq fix-tail
	  (string= ".." (let ((l (car (last npl))))
			  (if* (atom l)
			     then l
			     else (car l)))))
	(loop
	  (setq index
	    (position ".." npl
		      :test #'(lambda (a b)
				(string= a
					 (if* (atom b)
					    then b
					    else (car b))))))
	  (when (null index) (return))
	  
	  (if* (= 0 index)
	     then ;; rfe11852: RFC 3986, in section 5.4.2 (Abnormal
		  ;; Examples) says parsers; must be careful in handling
		  ;; cases where there are more ".." segments in a
		  ;; relative-path reference than there are in the base
		  ;; URI's path.  The examples, between the two RFC's were
		  ;; changed to show the additional, leading ..'s to be
		  ;; removed. So, we'll do that now.
		  (setq npl (cdr npl))
	   elseif (= 1 index)
	     then (setq npl (cddr npl))
	     else (setq tmp npl)
		  (dotimes (x (- index 2)) (setq tmp (cdr tmp)))
		  (setf (cdr tmp) (cdddr tmp))))
	(setf (cdr new-path-list) npl)
	(when fix-tail (setq new-path-list (nconc new-path-list '("")))))

      ;; step 6g:
      ;; don't complain if new-path-list starts with `..'.  See comment
      ;; above about this step.

      ;; step 6h:
      (when (or (equal '(:absolute "") new-path-list)
		(equal '(:absolute) new-path-list))
	(setq new-path-list nil))
      (setf (uri-path uri)
	(render-parsed-path new-path-list
			    ;; don't know, so have to assume:
			    t)))

;;;; step 7
   :done
    (return-from merge-uris uri)))

(defmethod merge-uris ((urn urn) (base urn) &optional place)
  (if* place
     then (setf (urn-nid place) (urn-nid urn))
	  (setf (urn-nss place) (urn-nss urn))
	  place
     else urn))

(defmethod merge-uris ((urn urn) (base uri) &optional place)
  (if* place
     then (setf (urn-nid place) (urn-nid urn))
	  (setf (urn-nss place) (urn-nss urn))
	  place
     else urn))

(defmethod merge-uris ((uri uri) (base urn) &optional place)
  (copy-uri uri :place place))

(defmethod enough-uri ((uri string) (base string) &optional place)
  (enough-uri (parse-uri uri) (parse-uri base) place))

(defmethod enough-uri ((uri uri) (base string) &optional place)
  (enough-uri uri (parse-uri base) place))

(defmethod enough-uri ((uri string) (base uri) &optional place)
  (enough-uri (parse-uri uri) base place))

(defmethod enough-uri ((uri uri) (base uri) &optional place)
  (let ((new-scheme nil)
	(new-host nil)
	(new-userinfo nil)
	(new-port nil)
	(new-parsed-path nil))

    (when (or (and (uri-scheme uri)
		   (not (equalp (uri-scheme uri) (uri-scheme base))))
	      (and (uri-host uri)
		   (not (equalp (uri-host uri) (uri-host base))))
	      (not (equalp (uri-userinfo uri) (uri-userinfo base)))
	      (not (equalp (uri-port uri) (uri-port base))))
      (return-from enough-uri uri))

    (when (null (uri-host uri)) (setq new-host (uri-host base)))
    (when (null (uri-userinfo uri)) (setq new-userinfo (uri-userinfo base)))
    (when (null (uri-port uri)) (setq new-port (uri-port base)))
    
    (when (null (uri-scheme uri))
      (setq new-scheme (uri-scheme base)))

    ;; Now, for the hard one, path.
    ;; We essentially do here what enough-namestring does.
    (do* ((base-path (uri-parsed-path base))
	  (path (uri-parsed-path uri))
	  (bp base-path (cdr bp))
	  (p path (cdr p)))
	((or (null bp) (null p))
	 ;; If p is nil, that means we have something like
	 ;; (enough-uri "/foo/bar" "/foo/bar/baz.htm"), so
	 ;; new-parsed-path will be nil.
	 (when (null bp)
	   (setq new-parsed-path (copy-list p))
	   (when (not (symbolp (car new-parsed-path)))
	     (push :relative new-parsed-path))))
      (if* (equal (car bp) (car p))
	 thenret ;; skip it
	 else (setq new-parsed-path (copy-list p))
	      (when (not (symbolp (car new-parsed-path)))
		(push :relative new-parsed-path))
	      (return)))

    (let ((new-path 
	   (when new-parsed-path
	     (render-parsed-path new-parsed-path
				 ;; don't know, so have to assume:
				 t)))
	  (new-query (uri-query uri))
	  (new-fragment (uri-fragment uri))
	  (new-plist (copy-list (uri-plist uri))))
      (if* (and (null new-scheme)
		(null new-host)
		(null new-userinfo)
		(null new-port)
		(null new-path)
		(null new-parsed-path)
		(null new-query)
		(null new-fragment))
	 then ;; can't have a completely empty uri!
	      (copy-uri nil
			:class (class-of uri)
			:place place
			:path "/"
			:plist new-plist)
	 else (copy-uri nil
			:class (class-of uri)
			:place place
			:scheme new-scheme
			:host new-host
			:userinfo new-userinfo
			:port new-port
			:path new-path
			:parsed-path new-parsed-path
			:query new-query
			:fragment new-fragment
			:plist new-plist)))))

(defmethod enough-uri ((urn urn) (base urn) &optional place)
  (if* place
     then (setf (urn-nid place) (urn-nid urn))
	  (setf (urn-nss place) (urn-nss urn))
	  place
     else urn))

(defmethod enough-uri ((urn urn) (base uri) &optional place)
  (declare (ignore place))
  (error "enough-uri of a URN (~a) and URI (~a)." urn base))

(defmethod enough-uri ((uri uri) (base urn) &optional place)
  (declare (ignore place))
  (error "enough-uri of a URI (~a) and URN (~a)." uri base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uri-to-pathname (uri)
  ;; This is gross.  On Windows, we take URIs like
  ;;   file:///d:/foo/bar.cl
  ;; and
  ;;   file:///d|/foo/bar.cl
  ;; and turn them into #p"d:/foo/bar.cl".  On UNIX, something like this
  ;;   file:///foo/bar.cl
  ;; turns into #p"/foo/bar.cl".  The drive letter nonsense goes away, in
  ;; other words.
  (when (not (eq :file (uri-scheme uri)))
    (error "Only file: URIs can be converted to pathnames: ~s." uri))
  (when (null (uri-path uri)) (error "URI has no path: ~s." uri))
  (flet (#+mswindows
	 (frob-uri-file-string (path)
	   ;; /x:foo/  turns into x:foo/
	   ;; /x|foo/  turns into x:foo/
	   ;; /x:/foo/ turns into x:/foo/
	   ;; /x|/foo/ turns into x:/foo/
	   (when (and (>= (length path) 3)
		      (char= #\/ (schar path 0))
		      (alpha-char-p (schar path 1))
		      (or (char= #\| (schar path 2))
			  (char= #\: (schar path 2))))
	     (setq path (subseq path 1))
	     (setf (schar path 1) #\:))
	   path))
    (pathname
     (decode-escaped-encoding
      #+mswindows (frob-uri-file-string (uri-path uri))
      #-mswindows (uri-path uri)
      t
      nil))))

(defun pathname-to-uri (pathname)
  (when (not (#-(version>= 6 2 pre-beta 5) uri-absolute-pathname-p
	      #+(version>= 6 2 pre-beta 5) excl::absolute-pathname-p
	      pathname t))
    (error "A relative pathname cannot be converted to a URI: ~s." pathname))
  (parse-uri
   (let ((s
	  (encode-escaped-encoding
	   #+mswindows (substitute #\/ #\\ (namestring pathname))
	   #-mswindows (namestring pathname)
	   *reserved-path-conv-characters*
	   t)))
     #-mswindows (format nil "file://~a" s)
     #+mswindows
     (if* (pathname-device pathname)
	then (format nil "file:///~a" s)
	else (format nil "file://~a" s)))))

#-(version>= 6 2 pre-beta 5)
(defun uri-absolute-pathname-p (pathname &optional ignore-drive)
  #-mswindows (declare (ignore ignore-drive))
  (setq pathname (pathname pathname))
  (let (x)
    (and (setq x (pathname-directory pathname))
	 x
	 (consp x)
	 (eq :absolute (car x))
	 #+mswindows
	 (or ignore-drive
	     (stringp (pathname-device pathname))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for interning URIs

(defun make-uri-space (&rest keys &key (size 777) &allow-other-keys)
  (apply #'make-hash-table :size size :hash-function 'uri-hash
	 :test 'uri= :values nil keys))

(defun uri-hash (uri)
  (if* (uri-hashcode uri)
     thenret
     else (setf (uri-hashcode uri) (sxhash (render-uri uri nil)))))

(defvar *uris* (make-uri-space))

(defun uri-space () *uris*)

(defun (setf uri-space) (new-val)
  (setq *uris* new-val))

;; bootstrapping (uri= changed from function to method):
(when (fboundp 'uri=) (fmakunbound 'uri=))

(defmethod uri= ((uri1 uri) (uri2 uri))
  (when (not (eq (uri-scheme uri1) (uri-scheme uri2)))
    (return-from uri= nil))
  ;; RFC2396 says: a URL with an explicit ":port", where the port is
  ;; the default for the scheme, is the equivalent to one where the
  ;; port is elided.  Hmmmm.  This means that this function has to be
  ;; scheme dependent.  Grrrr.
  (let ((default-port (case (uri-scheme uri1)
			(:http 80)
			(:https 443)
			(:ftp 21)
			(:telnet 23))))
    (and (equalp (uri-host uri1) (uri-host uri2))
	 (equalp (uri-userinfo uri1) (uri-userinfo uri2))
	 (eql (or (uri-port uri1) default-port)
	      (or (uri-port uri2) default-port))
	 (string= (uri-path uri1) (uri-path uri2))
	 (string= (uri-query uri1) (uri-query uri2))
	 (string= (uri-fragment uri1) (uri-fragment uri2)))))

(defmethod uri= ((urn1 urn) (urn2 urn))
  (when (not (eq (uri-scheme urn1) (uri-scheme urn2)))
    (return-from uri= nil))
  (and (equalp (urn-nid urn1) (urn-nid urn2))
       (urn-nss-equal (urn-nss urn1) (urn-nss urn2))))

(defun urn-nss-equal (nss1 nss2 &aux len)
  ;; Return t iff the nss values are the same.
  ;; %2c and %2C are equivalent.
  (when (or (null nss1) (null nss2)
	    (not (= (setq len (length nss1))
		    (length nss2))))
    (return-from urn-nss-equal nil))
  (do* ((i 0 (1+ i))
	(state :char)
	c1 c2)
      ((= i len) t)
    (setq c1 (schar nss1 i))
    (setq c2 (schar nss2 i))
    (ecase state
      (:char
       (if* (and (char= #\% c1) (char= #\% c2))
	  then (setq state :percent+1)
	elseif (char/= c1 c2)
	  then (return nil)))
      (:percent+1
       (when (char-not-equal c1 c2) (return nil))
       (setq state :percent+2))
      (:percent+2
       (when (char-not-equal c1 c2) (return nil))
       (setq state :char)))))

(defmethod intern-uri ((xuri uri) &optional (uri-space *uris*))
  (let ((uri (gethash xuri uri-space)))
    (if* uri
       thenret
       else (excl:puthash-key xuri uri-space))))

(defmethod intern-uri ((uri string) &optional (uri-space *uris*))
  (intern-uri (parse-uri uri) uri-space))

(defun unintern-uri (uri &optional (uri-space *uris*))
  (if* (eq t uri)
     then (clrhash uri-space)
   elseif (uri-p uri)
     then (remhash uri uri-space)
     else (error "bad uri: ~s." uri)))

(defmacro do-all-uris ((var &optional uri-space result-form)
		       &rest forms
		       &environment env)
  "do-all-uris (var [[uri-space] result-form])
  		    {declaration}* {tag | statement}*
Executes the forms once for each uri with var bound to the current uri"
  (let ((f (gensym))
	(g-ignore (gensym))
	(g-uri-space (gensym))
	(body (third (excl::parse-body forms env))))
    `(let ((,g-uri-space (or ,uri-space *uris*)))
       (prog nil
	 (flet ((,f (,var &optional ,g-ignore)
		  (declare (ignorable ,var ,g-ignore))
		  (tagbody ,@body)))
	   (maphash #',f ,g-uri-space))
	 (return ,result-form)))))

;; moved to reader.cl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :uri)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timings
;; (don't run under emacs with M-x fi:common-lisp)

#+ignore
(defun time-uri-module ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((uri "http://www.franz.com/a/b;x;y;z/c/foo?bar=baz&xxx#foo")
	(uri2 "http://www.franz.com/a/b;x;y;z/c/%2ffoo?bar=baz&xxx#foo"))
    (gc t) (gc :tenure) (gc :tenure) (gc :tenure)
    (format t "~&;;; starting timing testing 1...~%")
    (time (dotimes (i 100000) (parse-uri uri)))
    
    (gc t) (gc :tenure) (gc :tenure) (gc :tenure)
    (format t "~&;;; starting timing testing 2...~%")
    (let ((uri (parse-uri uri)))
      (time (dotimes (i 100000)
	      ;; forces no caching of the printed representation:
	      (setf (uri-string uri) nil)
	      (format nil "~a" uri))))
    
    (gc t) (gc :tenure) (gc :tenure) (gc :tenure)
    (format t "~&;;; starting timing testing 3...~%")
    (time
     (progn
       (dotimes (i 100000) (parse-uri uri2))
       (let ((uri (parse-uri uri)))
	 (dotimes (i 100000)
	   ;; forces no caching of the printed representation:
	   (setf (uri-string uri) nil)
	   (format nil "~a" uri)))))))

;;******** reference output (ultra, modified 5.0.1):
;;; starting timing testing 1...
; cpu time (non-gc) 13,710 msec user, 0 msec system
; cpu time (gc)     600 msec user, 10 msec system
; cpu time (total)  14,310 msec user, 10 msec system
; real time  14,465 msec
; space allocation:
;  1,804,261 cons cells, 7 symbols, 41,628,832 other bytes, 0 static bytes
;;; starting timing testing 2...
; cpu time (non-gc) 27,500 msec user, 0 msec system
; cpu time (gc)     280 msec user, 20 msec system
; cpu time (total)  27,780 msec user, 20 msec system
; real time  27,897 msec
; space allocation:
;  1,900,463 cons cells, 0 symbols, 17,693,712 other bytes, 0 static bytes
;;; starting timing testing 3...
; cpu time (non-gc) 52,290 msec user, 10 msec system
; cpu time (gc)     1,290 msec user, 30 msec system
; cpu time (total)  53,580 msec user, 40 msec system
; real time  54,062 msec
; space allocation:
;  7,800,205 cons cells, 0 symbols, 81,697,496 other bytes, 0 static bytes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; after improving decode-escaped-encoding/encode-escaped-encoding:

;;; starting timing testing 1...
; cpu time (non-gc) 14,520 msec user, 0 msec system
; cpu time (gc)     400 msec user, 0 msec system
; cpu time (total)  14,920 msec user, 0 msec system
; real time  15,082 msec
; space allocation:
;  1,800,270 cons cells, 0 symbols, 41,600,160 other bytes, 0 static bytes
;;; starting timing testing 2...
; cpu time (non-gc) 27,490 msec user, 10 msec system
; cpu time (gc)     300 msec user, 0 msec system
; cpu time (total)  27,790 msec user, 10 msec system
; real time  28,025 msec
; space allocation:
;  1,900,436 cons cells, 0 symbols, 17,693,712 other bytes, 0 static bytes
;;; starting timing testing 3...
; cpu time (non-gc) 47,900 msec user, 20 msec system
; cpu time (gc)     920 msec user, 10 msec system
; cpu time (total)  48,820 msec user, 30 msec system
; real time  49,188 msec
; space allocation:
;  3,700,215 cons cells, 0 symbols, 81,707,144 other bytes, 0 static bytes
