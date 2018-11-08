;; -*- mode: common-lisp; package: net.uri -*-
;; Support for URIs (including URNs) in Allegro.
;; For general URI information see RFC 3986.
;; For general URN information see RFC 8141.
;; For IPv6 changes see RFC 6874.
;;
;; See the file LICENSE for the full license governing this code.

#+(version= 9 0)
(sys:defpatch "uri" 9
  "v9: fix misc parser issues;
v8: string-to-uri/uri-to-string, parse-uri query pct encoding of +/=/&;
v7: fixes for non-strict mode parsing, merge-uris, render-uri and parse-uri;
v6: fixes for non-strict mode parsing;
v5: bring up to spec with RFCs 3986, 6874 and 8141;
v4: handle no-authority URIs with `hdfs' scheme the same as `file';
v3: Fix handling of #\% chars in path component of uri;
v2: speed up parse-uri;
v1: don't normalize away a null fragment, on merge remove leading `.' and `..'."
  :type :system
  :post-loadable t)

#+(version= 10 0)
(sys:defpatch "uri" 7
  "v7: fix misc parser issues;
v6: string-to-uri/uri-to-string, parse-uri query pct encoding of +/=/&;
v5: fixes for non-strict mode parsing, merge-uris, render-uri and parse-uri;
v4: fixes for non-strict mode parsing;
v3: bring up to spec with RFCs 3986, 6874 and 8141;
v2: allow null query;
v1: handle no-authority URIs with `hdfs' scheme the same as `file'."
  :type :system
  :post-loadable t)

#+(version= 10 1)
(sys:defpatch "uri" 5
  "v5: fix misc parser issues;
v4: string-to-uri/uri-to-string, parse-uri query pct encoding of +/=/&;
v3: fixes for non-strict mode parsing, merge-uris, render-uri and parse-uri;
v2: fixes for non-strict mode parsing;
v1: bring up to spec with RFCs 3986, 6874 and 8141."
  :type :system
  :post-loadable t)

(eval-when (compile eval load) (require :util-string))

(defpackage :net.uri
  (:use #:common-lisp #:excl #:util.string)
  (:export
   #:uri				; the type and a function
   #:uri-p
   #:copy-uri

   #:uri-scheme				; and slots
   #:uri-host
   #:uri-ipv6
   #:uri-zone-id
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
   #:urn-q-component			; RFC 8141
   #:urn-f-component			; RFC 8141
   #:urn-r-component			; RFC 8141
   
   #:*strict-parse*
   #:parse-uri
   #:merge-uris
   #:enough-uri
   #:uri-parsed-path
   #:render-uri
   #:string-to-uri
   #:uri-to-string

   #:make-uri-space			; interning...
   #:uri-space
   #:uri=
   #:intern-uri
   #:unintern-uri
   #:do-all-uris
   
   #:uri-to-pathname
   #:pathname-to-uri))

(in-package :net.uri)

;; This does not persist past the end of compile-file
(eval-when (compile) (declaim (optimize (speed 3))))

(defvar *strict-parse* t)

(defclass uri ()
  (
;;;; external:
   (scheme :initarg :scheme :initform nil :accessor uri-scheme)
   ;; uri-host is computed and cached.  See copious comments below.
   ;; uri-ipv6 and uri-zone-id are read-only by users, so they are in the
   ;;   internal section below.
   (userinfo :initarg :userinfo :initform nil :accessor uri-userinfo)
   (port :initarg :port :initform nil :accessor uri-port)
   (path :initarg :path :initform nil :accessor uri-path)
   (query :initarg :query :initform nil :accessor uri-query)
   (fragment :initarg :fragment :initform nil :accessor uri-fragment)
   (plist :initarg :plist :initform nil :accessor uri-plist)

;;;; internal:
   (.host ;; where part of the value for uri-host is stored
    ;; The values stored here are for URIs with names or IPv4 addresses.
    ;; IPv6 addresses are stored in the .ipv6 and .zone-id slots.
    ;; NOTE:
    ;;  I'm conflicted over the fact that .host is both computed and NOT
    ;;  computed.  It is computed for IPv6, but it holds the actual values
    ;;  from the parse for names or IPv4 addresses.  It might be a tiny bit
    ;;  more clear to have a separate slot for the computed value, but
    ;;  would that extra clarity be worth the extra space at runtime?
    :initarg :host :initform nil :accessor .uri-host)
   (.ipv6 ;; the pure IPv6 portion of the uri-host, nil otherwise
    ;; This value is the actual IPv6 address that would be suitable for use
    ;; in networking functions.  It does NOT include the zone-id or the
    ;; URI [] syntax.
    :initarg :ipv6 :initform nil :accessor .uri-ipv6)
   (.zone-id ;; used if IPv6 has a zone ID
    :initarg :zone-id :initform nil :accessor .uri-zone-id)
   (escaped ;; non-nil if parsed input contained pct encoded characters
    :initarg :escaped :initform nil :accessor uri-escaped)
   (string ;; the cached printable representation of the URI
    ;; It might be different than the original string, because of percent
    ;; encoding.  Use of slot setf methods may reset this slot to nil,
    ;; causing it to be recomputed when needed.
    :initarg :string :initform nil :accessor uri-string)
   (parsed-path ;; the cached parsed representation of the URI path
    :initarg :parsed-path
    :initform nil
    :accessor .uri-parsed-path)
   (hashcode ;; cached sxhash, so we don't have to compute it more than once
    :initarg :hashcode :initform nil :accessor uri-hashcode)))

(defmethod uri-host ((uri uri))
  ;; Return the computed host for URI.  It is the value which could be used
  ;; by networking functions or programs to perform communication with the
  ;; resource designated by URI.
  (let ((host (.uri-host uri))
	ipv6 zone-id)
    ;; If HOST has a value, then use that.  Otherwise, if IPV6 has a value,
    ;; then return the IPv6 address, which will include the zone-id, if
    ;; non-nil.  Otherwise, return nil.
    (if* host
       thenret
     elseif (setq ipv6 (.uri-ipv6 uri))
       then ;; This setf clears the cached printed value (string slot)
	    (setf (.uri-host uri)
	      (if* (setq zone-id (.uri-zone-id uri))
		 then (string+ ipv6 "%" zone-id)
		 else ipv6)))))

;; It is by design there are no public setf methods for these
(defmethod uri-ipv6    ((uri uri)) (.uri-ipv6    uri))
(defmethod uri-zone-id ((uri uri)) (.uri-zone-id uri))

;; The .HOST slot is computed, for IPv6, or the actual name or IPv4
;; address.  To ensure all three slots are kept consistent, define a
;; function to set them.
(defun set-host (uri name-or-ipv4 ipv6 zone-id)
  (when (and name-or-ipv4 ipv6)
    (error "Both the IPv4/name and IPv6 values cannot be non-nil: ~s, ~s."
	   name-or-ipv4 ipv6))
  (setf (.uri-host    uri) name-or-ipv4
	(.uri-ipv6    uri) ipv6
	(.uri-zone-id uri) zone-id))

(defmethod (setf uri-host) (v (uri uri))
  (if* (null v)
     then (set-host uri nil nil nil)
   elseif (stringp v)
     then (multiple-value-bind (found whole ipv6 zone-id)
	      ;; This embodies knowledge of the URI IPv6 syntax
	      (match-re "^(.*:.*?)(%.*)?$" v)
	    (declare (ignore whole))
	    (if* found
	       then (set-host uri nil ipv6 zone-id)
	       else (set-host uri v nil nil))
	    v)
     else (error "host value must be a string: ~s." v)))

(defclass urn (uri)
  ;; NOTE: the q-component is stored in the `query' slot and the
  ;;       f-component is stored in the `fragment' slot of the of the
  ;;       parent class (uri).
  ;; The slots below have no place in the parent class.
  ((nid :initarg :nid :initform nil :accessor urn-nid)
   (nss :initarg :nss :initform nil :accessor urn-nss)
   ;; q-component is stored in the `query'
   ;; f-component is stored in the `fragment'
   (r-component ;; ignored in comparisons
    :initarg :r-component :initform nil :accessor urn-r-component)))

(eval-when (compile eval)
  (defmacro clear-computed-uri-slots (name)
    `(defmethod (setf ,name) :around (new-value (self uri))
       (declare (ignore new-value))
       (prog1 (call-next-method)
	 (setf (uri-string self) nil)
	 ,@(when (eq name 'uri-path)    `((setf (.uri-parsed-path self) nil)))
	 (setf (uri-hashcode self) nil))))
  )

(clear-computed-uri-slots uri-scheme)
;; Do not add the following slots, because they are handled specially.
;; See set-host above.
;;  .host
;;  .ipv6
;;  .zone-id
(clear-computed-uri-slots uri-userinfo)
(clear-computed-uri-slots uri-port)
(clear-computed-uri-slots uri-path)
(clear-computed-uri-slots uri-query)
(clear-computed-uri-slots uri-fragment)

(defmethod make-load-form ((self uri) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-name (class-of self))
     :scheme ,(uri-scheme self)
     :host ,(.uri-host self)
     :ipv6 ,(.uri-ipv6 self)
     :zone-id ,(.uri-zone-id self)
     :userinfo ,(uri-userinfo self)
     :port ,(uri-port self)
     :path ',(uri-path self)
     :query ,(uri-query self)
     :fragment ,(uri-fragment self)
     :plist ',(uri-plist self)
     :string ,(uri-string self)
     :parsed-path ',(.uri-parsed-path self)))

(defmethod make-load-form ((self urn) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-name (class-of self))
     :scheme ,(uri-scheme self)
     :host ,(.uri-host self)
     :ipv6 ,(.uri-ipv6 self)
     :zone-id ,(.uri-zone-id self)
     :userinfo ,(uri-userinfo self)
     :port ,(uri-port self)
     :path ',(uri-path self)
     :query ,(uri-query self)		; q-component
     :fragment ,(uri-fragment self)	; f-component
     :plist ',(uri-plist self)
     :string ,(uri-string self)
     :parsed-path ',(.uri-parsed-path self)
   ;;; URN-specific:
     :nid ,(urn-nid self)
     :nss ,(urn-nss self)
     :r-component ,(urn-r-component self)))

(defmethod uri-p ((thing uri)) t)
(defmethod uri-p ((thing t)) nil)

(defun copy-uri (uri
		 &key place
		      (scheme (when uri (uri-scheme uri)))
		      (host (when uri (.uri-host uri)))
		      (ipv6 (when uri (.uri-ipv6 uri)))
		      (zone-id (when uri (.uri-zone-id uri)))
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
	  (set-host place host ipv6 zone-id)
	  (setf (uri-userinfo place) userinfo)
	  (setf (uri-port place) port)
	  (setf (uri-path place) path)
	  (setf (.uri-parsed-path place) parsed-path)
	  (setf (uri-query place) query)
	  (setf (uri-fragment place) fragment)
	  (setf (uri-plist place) plist)
	  (setf (uri-escaped place) escaped)
	  (setf (uri-hashcode place) nil)
	  place
   elseif (eq 'uri class)
     then ;; allow the compiler to optimize the call to make-instance:
	  (make-instance 'uri
	    :scheme scheme :host host :ipv6 ipv6 :zone-id zone-id
	    :userinfo userinfo :port port
	    :path path :parsed-path parsed-path
	    :query query :fragment fragment :plist plist
	    :escaped escaped :string nil :hashcode nil)
     else (make-instance class
	    :scheme scheme :host host :ipv6 ipv6 :zone-id zone-id
	    :userinfo userinfo :port port
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
  (if* (null path-list)
     then (setf (uri-path uri) nil)
	  (setf (.uri-parsed-path uri) nil)
	  path-list
     else (when (not (and (consp path-list)
			  (or (member (car path-list) '(:absolute :relative)
				      :test #'eq))))
	    (error "internal error: path-list is ~s." path-list))
	  (setf (uri-path uri) (render-parsed-path path-list t))
	  (setf (.uri-parsed-path uri) path-list)
	  path-list))

(defun uri-authority (uri)
  (when (uri-host uri)
    (let ((*print-pretty* nil))
      (format nil "~@[~a@~]~a~@[:~a~]" (uri-userinfo uri)
	      (uri-host uri) (uri-port uri)))))

(defun uri-nid (uri)
  (if* (equalp "urn" (uri-scheme uri))
     then ;; Intentionally did not use .uri-host:
	  (uri-host uri)
     else (error "URI is not a URN: ~s." uri)))

(defun uri-nss (uri)
  (if* (equalp "urn" (uri-scheme uri))
     then (uri-path uri)
     else (error "URI is not a URN: ~s." uri)))

(defmethod urn-q-component ((urn urn)) (uri-query urn))
(defmethod urn-f-component ((urn urn)) (uri-fragment urn))

(defmethod uri ((thing uri))     thing)
(defmethod uri ((thing string)) (parse-uri thing))
(defmethod uri ((thing t))      (error "Cannot coerce ~s to a uri." thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To match sets of characters, the parser uses bit vectors constructed
;; from lists of characters.

(eval-when (compile eval)
  ;; The size of bit vectors are defined to check for characters in the
  ;; range 0 to one less than this value.
  (defparameter +uri-bit-vector-size+ 127))

(eval-when (compile eval)

  (defun generate-character-list (char-start char-end)
  ;; Generate a list of characters between char-start and char-end,
  ;; inclusive of the start and end characters.
  (when (>= (char-code char-start) (char-code char-end))
    (error "char-start (~s) must come before char-end (~s)."
	   char-start char-end))
  ;; Make sure it doesn't index off the end of the array:
  (when (>= (char-code char-end) #.+uri-bit-vector-size+)
    (error "Illegal char-code (>= ~d)." #.+uri-bit-vector-size+))
  (do* ((stop-code (1- (char-code char-start)))
	(c (char-code char-end) (1- c))
	(res '()))
      ((= c stop-code) res)
    (push (code-char c) res)))

(defmacro char-included-p (bit-vector char-code)
  `(= 1 (sbit ,bit-vector ,char-code)))

(defmacro safe-char-included-p (bit-vector char-code)
  (let ((g-bv (gensym))
	(g-cc (gensym)))
    `(let* ((,g-bv ,bit-vector)
	    (,g-cc ,char-code))
       (or (null ,g-bv)
	   (and (< ,g-cc #.+uri-bit-vector-size+)
		(char-included-p ,g-bv ,g-cc))))))

)

(eval-when (compile eval load)
(defun make-char-bitvector (chars &key except)
  ;; Return a bitvector which has a 1 for each character represented in
  ;; CHARS, where the index is the char-code of the character.  If EXCEPT
  ;; is non-nil, it should be a list of characters to exclude.
  (do* ((a (make-array #.+uri-bit-vector-size+
		       :element-type 'bit :initial-element 0))
	(chars chars (cdr chars))
	(c (car chars) (car chars)))
      ((null chars) a)
    (if* (and except (member c except :test #'eq))
       thenret
       else (setf (sbit a (char-code c)) 1))))
)

;; Lists of characters used to make the bit vectors.  These lists are
;; pretty much straight out of the grammars.

(eval-when (compile eval load)
(defparameter *alpha-chars*
    '#.(append (generate-character-list #\A #\Z)
	       (generate-character-list #\a #\z)))

(defparameter *digit-chars* '#.(generate-character-list #\0 #\9))

(defparameter *hexdig-chars*
    (append *digit-chars*
	    '#.(generate-character-list #\A #\F)
	    '#.(generate-character-list #\a #\f)))

(defparameter *alphanum-chars*  (append *alpha-chars* *digit-chars*))
(defparameter *alphanum+-chars* (append *alphanum-chars* '(#\-)))

(defparameter *sub-delims-chars* '(#\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))

(defparameter *unreserved-chars*
    (append *alpha-chars* *digit-chars* '(#\- #\. #\_ #\~)))

(defparameter *pchar-chars*
    (append *unreserved-chars* *sub-delims-chars* '(#\: #\@)))

;; used in pathname to URI conversion:
(defparameter *pchar/-chars*  (append *pchar-chars* '(#\/)))

(defparameter *urn-nss-chars* (append *pchar-chars* '(#\/)))

(defparameter *segment-nz-nc-chars* ;; pchar w/o #\:
    (append *unreserved-chars* *sub-delims-chars* '(#\@)))

(defparameter *query-strict-chars*    (append *pchar-chars* '(#\/ #\?)))
(defparameter *urn-query-chars*       (append *pchar-chars* '(#\/)))
(defparameter *fragment-strict-chars* (append *pchar-chars* '(#\/ #\?)))

(defparameter *ipvfuture-chars*
    (append *unreserved-chars* *sub-delims-chars* '(#\:)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *alpha-bitvector*      (make-char-bitvector *alpha-chars*))
(defparameter *digit-bitvector*      (make-char-bitvector *digit-chars*))
(defparameter *alphanum-bitvector*   (make-char-bitvector *alphanum-chars*))
(defparameter *alphanum+-bitvector*  (make-char-bitvector *alphanum+-chars*))
(defparameter *hexdig-bitvector*     (make-char-bitvector *hexdig-chars*))
(defparameter *pchar-bitvector*      (make-char-bitvector *pchar-chars*))
(defparameter *urn-nss-bitvector*    (make-char-bitvector *urn-nss-chars*))
(defparameter *unreserved-bitvector* (make-char-bitvector *unreserved-chars*))

;; used in pathname to URI conversion:
(defparameter *pchar/-bitvector*    (make-char-bitvector *pchar/-chars*))

(defparameter *userinfo-bitvector*
    (make-char-bitvector
     (append *unreserved-chars* *sub-delims-chars* '(#\:))))

(defparameter *reg-name-bitvector*
    (make-char-bitvector (append *unreserved-chars* *sub-delims-chars*)))

(defparameter *scheme-bitvector*
    (make-char-bitvector
     (append *alpha-chars* *digit-chars* '(#\+ #\- #\.))))

(defparameter *query-bitvector-strict*
    (make-char-bitvector *query-strict-chars*))

(defparameter *query-bitvector-non-strict*
    (make-char-bitvector
     (append *query-strict-chars*
	     '(#\| #\^
	       ;; Too many websites/tools use this in URLs
	       #\space))))

;;;;;;;;; HACK
;; See discussion in rfe15844.  Decoding the query should not touch percent
;; encodings of #\+, #\= and #\&, because those are interpreted by
;; another specification (HTTP).

(defparameter *decode-query-strict-chars*
  (append *unreserved-chars*
	  ;; Instead of *sub-delims-chars*, this (which is just like
	  ;; *sub-delims-chars*, except for the commented out characters):
	  '(#\! #\$ #\' #\( #\) #\* #\, #\;
	    ;;#\& #\+ #\=
	    )
	  '(#\: #\@)))

(defparameter *decode-query-bitvector-strict*
    (make-char-bitvector *decode-query-strict-chars*))

(defparameter *decode-query-bitvector-non-strict*
    (make-char-bitvector
     (append *decode-query-strict-chars*
	     '(#\| #\^
	       ;; Too many websites/tools use this in URLs
	       #\space))))
;;;;;;;;; ...HACK

(defparameter *fragment-bitvector-strict*
    (make-char-bitvector *fragment-strict-chars*))

(defparameter *fragment-bitvector-non-strict*
    (make-char-bitvector
     (append *fragment-strict-chars*
	     '(#\#
	       ;; Too many websites/tools use these in URLs
	       #\space #\|))))

(defparameter *segment-nz-nc-bitvector*
    (make-char-bitvector *segment-nz-nc-chars*))

(defparameter *urn-query-bitvector* (make-char-bitvector *urn-query-chars*))

(defparameter *ipvfuture-bitvector* (make-char-bitvector *ipvfuture-chars*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The part of a URI that can have percent encoding:
;; - userinfo
;; - host
;; - path
;; - query
;; - fragment

(defun percent-decode-string (string allowed-bitvector)
  ;; Return a new string based on STRING which has all percent encoded
  ;; pairs (%xx) turned into real characters.  If ALLOWED-BITVECTOR is
  ;; non-nil, only characters that `match' this bitvector are converted.
  (do* ((i 0 (1+ i))
	(max (length string))
	(new-string (make-string max))
	(new-i 0 (1+ new-i))
	ch ch2 chc chc2)
      ((= i max)
       (excl::.primcall 'sys::shrink-svector new-string new-i)
       new-string)
    (declare (fixnum i max new-i))
    (if* (char= #\% (setq ch (schar string i)))
       then (when (> (+ i 3) max)
	      (excl::.parse-error
	       "Unsyntactic percent encoding at ~d in ~s." i string))
	    (setq ch (schar string (incf i)))
	    (setq ch2 (schar string (incf i)))
	    (when (not (and (setq chc (digit-char-p ch 16))
			    (setq chc2 (digit-char-p ch2 16))))
	      (excl::.parse-error
	       "Non-hexidecimal digits after % at ~d in ~s."
	       (- i 2) string))
	    (let ((ci (the fixnum
			(+ (the fixnum (* 16 (the fixnum chc)))
			   (the fixnum chc2)))))
	      (declare (fixnum ci))
	      (if* (safe-char-included-p allowed-bitvector ci)
		 then ;; OK to convert
		      (setf (schar new-string new-i)
			(code-char ci))
		 else ;; leave percent encoded
		      (setf (schar new-string new-i) #\%)
		      (setf (schar new-string (incf new-i)) ch)
		      (setf (schar new-string (incf new-i)) ch2)))
       else (setf (schar new-string new-i) ch))))

(defun percent-encode-string (string allowed-bitvector)
  ;; Return a new string based on STRING which has all characters which do
  ;; not match ALLOWED-BITVECTOR converted into percent encoded pairs (%xx).
  ;; Percent-encoded pairs in the string are skipped over, as it is assumed
  ;; they were required to be encoded.
  ;;
  ;; Make a string as big as it possibly needs to be (3 times the original
  ;; size), and truncate it at the end.
  ;;(declare (optimize (safety 1))) 
  ;;(declare (:explain :calls :types))
  (do* ((hexchars ;; RFC 3986 section 2.1 says use upper case:
	 "0123456789ABCDEF")
	(pct (char-code #\%))
	(max (length string))
	(new-max (* 3 max)) ;; worst case new size
	(new-string (make-string new-max))
	(i 0 (1+ i))
	(new-i -1)
	(ci ;; so the fixnum decl is true:
	 0)
	c)
      ((= i max)
       (excl::.primcall 'sys::shrink-svector new-string (incf new-i))
       new-string)
    (declare (fixnum pct max new-max i new-i ci))
    (setq ci (char-code (setq c (schar string i))))
    (if* (or (= ci pct) ;; skip %'s
	     (safe-char-included-p allowed-bitvector ci))
       then ;; ok as is
	    (incf new-i)
	    (setf (schar new-string new-i) c)
       else ;; need to escape it
	    (let ((d1 (ash ci -4))
		  (d2 (logand ci #xf)))
	      (declare (fixnum d1 d2))
	      (setf (schar new-string (incf new-i)) #\%)
	      (setf (schar new-string (incf new-i)) (schar hexchars d1))
	      (setf (schar new-string (incf new-i)) (schar hexchars d2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For efficiency, we do as few subseq's as possible.  To achieve this, we
;; return, from various parser functions, the start/end pair encoded into a
;; fixnum.   This means the limit for a URI string is limited to 16384 on a
;; 32-bit Lisp.  It appears from searches that this is well above the
;; accepted maximum for URI strings.

(eval-when (compile eval)
  ;; The max array index is 1/2 of the available fixnum range.
  (defparameter +uri-max-string-length+
      #.(expt 2 (truncate (integer-length most-positive-fixnum) 2)))
  (defparameter +uri-pack-shift+
      #.(truncate (integer-length most-positive-fixnum) 2))
  (defparameter +uri-unpack-shift+
      #.(- (truncate (integer-length most-positive-fixnum) 2)))
  (defparameter +uri-unpack-mask+
      #.(1- (ash 1 (truncate (integer-length most-positive-fixnum) 2))))

  ;; This is used as a marker for the null string.  It must be a fixnum
  ;; that can't be returned as an index into a string.
  (defparameter *uri-null-marker* -1)
  )

(defun check-uri-string (string)
  ;; Make sure that:
  ;; 1. STRING is a simple string, and
  ;; 2. Two indices into STRING can packed into a single fixnum.
  ;;    This is what xsubseq/val do.
  (or (stringp string)
      (error "string must be a simple string."))
  (or (< (length string) #.+uri-max-string-length+)
      (error "string is larger than ~d characters."
	     #.+uri-max-string-length+)))

(defun xsubseq (start end)
  ;; Encode START and END into a fixnum.
  (declare (fixnum start end) (optimize (safety 0)))
  (the fixnum
    (+ start (the fixnum
	       (ash end #.+uri-pack-shift+)))))
       
(defun val (string i)
  ;; Return the subsequence of STRING given by I, which was encoded with
  ;; XSUBSEQ.
  (declare (fixnum i) (optimize (safety 0)))
  (when i
    (cond
     ((= i #.*uri-null-marker*) "")
     (t (let ((start (the fixnum (logand i #.+uri-unpack-mask+)))
	      (end (the fixnum
		     (ash i #.+uri-unpack-shift+))))
	  (declare (fixnum start end))
	  (subseq string start end))))))

(eval-when (compile eval)
(defmacro at-end-p (i end)
  ;; return T if index I is beyond the END of the string
  `(>= ,i ,end))
)

;; This macro is very specialized and not hygenic.  It is built for pure
;; speed.
(eval-when (compile eval)
(defmacro .looking-at (simple thing string index end char-equal)
  ;; INDEX and END are declared FIXNUM by our caller.
  ;; SIMPLE-STRING-P and SCHAR are much faster than STRINGP and CHAR.
  (let ((stringp (if simple 'simple-string-p 'stringp))
	(schar (if simple 'schar 'char))
	(len (gensym))
	(i (gensym))
	(j (gensym))
	(x (gensym)))
    `(let ((,len 0))
       (declare (fixnum ,len))
       (if* (at-end-p ,index ,end)
	  then nil
	elseif (characterp ,thing)
	  then ;; In this case, we ignore CHAR-EQUAL and always do the
	       ;; character comparison with CHAR= (case sensitively).
	       (when (char= ,thing (,schar ,string ,index))
		 (the fixnum (1+ ,index)))
	elseif (,stringp ,thing)
	  then (when (not (at-end-p (+ ,index
				       (setq ,len (the fixnum (length ,thing))))
				    ,end))
		 (do* ((,i ,index (the fixnum (1+ ,i)))
		       (,j 0 (the fixnum (1+ ,j)))
		       (,x ,len (the fixnum (1- ,x))))
		     ((= 0 ,x) (+ ,index ,len))
		   (declare (fixnum ,i ,j ,x))
		   (if* ,char-equal
		      then (when (not (char-equal (,schar ,string ,i)
						  (,schar ,thing  ,j)))
			     (return nil))
		      else (when (not (char= (,schar ,string ,i)
					     (,schar ,thing  ,j)))
			     (return nil)))))
	elseif (simple-bit-vector-p ,thing) ;; a LOT faster than bit-vector-p
	  then (when (char-included-p ,thing
				      (char-code (,schar ,string ,index)))
		 (the fixnum (1+ ,index)))
	  else (error "bad object: ~s." ,thing)))))
)

;; Future optimization from rfr:
;;   If THING is going to be a string very often,
;;   then you might get a useful speed improvement by splitting this
;;   again based on char-equal true/false. As it is, you're generating
;;   code in .looking-at that checks the char-equal argument on every
;;   character.
(defun looking-at (thing string index end
		   ;; optional because it is rarely given
		   &optional char-equal)
  ;; Return a new index into the parse buffer (STRING), if
  ;; an object equivalent to THING exists at index INDEX.
  ;; THING can be a:
  ;;  - bit vector: if a bit vector, then check that at character
  ;;    code index for it, there is a `1'
  ;;  - string: check that the string is in STRING starting at INDEX
  ;;  - character: check that the character is in STRING starting at
  ;;    INDEX
  ;; If CHAR-EQUAL is non-nil, then do character comparisons
  ;; case insensitively with CHAR-EQUAL.
  (declare (fixnum index end) (optimize (safety 0)))
  ;; The simple-string version is much faster, so this is worth the
  ;; complexity.
  ;;
  ;; NOTE: .looking-at takes ONLY symbols. The macro is not hygenic.
  (if* (simple-string-p string)
     then (.looking-at t   thing string index end char-equal)
     else (.looking-at nil thing string index end char-equal)))

(defun scan-forward (string start end bitvector
		     &optional func)
  ;; Scan STRING using BITVECTOR for matching, starting from position
  ;; START, and going no farther than END.
  ;; Return the index of the first non-matching character, or nil if no
  ;; characters matched.
  ;;
  ;; If BITVECTOR does not match, then call FUNC with three arguments
  ;; (STRING, <index>, and END).  If the FUNC returns nil, then scanning
  ;; terminates and this function returns <index>, if it is > START.
  (declare (fixnum start end) (optimize (safety 0)))
  (do ((i start)
       (new-i nil))
      ((= end i)
       (if* (= i start)
	  then nil
	  else i))
    (declare (fixnum i))
    (cond
     ((looking-at bitvector string i end)
      ;; Advance
      (incf i))
     (func
      ;; BITVECTOR failed.
      (if* (setq new-i (funcall func string i end))
	 then ;; FUNC return non-nil, advance I and keep going...
	      (setq i new-i)
	 else ;; FUNC return NIL, we're done
	      (if* (= i start)
		 then ;; Nothing matched => NIL:
		      (return nil)
		 else ;; Something matched => first index that didn't:
		      (return i))))
     (t
      ;; BITVECTOR didn't match.  We're done.
      (if* (= i start)
	 then ;; Nothing matched:
	      (return nil)
	 else ;; Something matched, first index that didn't:
	      (return i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A note about parser naming conventions.
;; There are two types of functions, where <name> comes from the LHS
;; of the ABNF grammar:
;;  state-<name> :: scan and return values based on the parse. The
;;      first value is always the "next" index beyond the parse.
;;      The subsequent values are rule specific, and documented in
;;      the functions themselves.
;;  scan-<name>  :: scan for and return either nil or an index.  If
;;      there is match, return the "next" index beyond the match,
;;      and nil otherwise.
;;
;; Rules marked `TERMINAL' must check for `at-end-p', since they must
;; terminate the parse for the input to be valid.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
(defun state-uri (string start end
		  &aux i scheme userinfo host port path query fragment
		       nid nss q-component f-component r-component i2
		       colon urn-scheme file-scheme)
  ;; rule 01: URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
  ;; --TERMINAL--
  ;; values: i scheme userinfo host port path query fragment
  (if* (and (multiple-value-setq (i scheme) (state-scheme string start end))
	    (setq colon (looking-at #\: string i end))
	    (not (setq urn-scheme (looking-at "urn" string start end t)))
	    (not (setq file-scheme (looking-at "file" string start end t)))
	    (multiple-value-setq (i2 userinfo host port path)
	      (state-hier-part string (1+ i) end)))
     then ;; Have hier-part...
	  (setq i i2)
	  (when (at-end-p i end)
	    (return-from state-uri
	      (values i scheme userinfo host port path)))
	   
	  (when (looking-at #\? string i end)
	    (if* (multiple-value-setq (i2 query)
		   (state-query string (incf i) end))
	       then (setq i i2)
	       else (setq query #.*uri-null-marker*)))
		   
	  (when (looking-at #\# string i end)
	    (if* (multiple-value-setq (i2 fragment)
		   (state-fragment string (incf i) end))
	       then (setq i i2)
	       else (setq fragment #.*uri-null-marker*)))

	  (when (at-end-p i end)
	    (values i scheme userinfo host port path query fragment))
   elseif urn-scheme
     then ;; values: i "urn" nid r-component nil nss q-component f-component
	  (when (multiple-value-setq (i nid nss q-component f-component
				      r-component)
		  (state-urn-namestring string i end))
	    (values i
		    scheme
		    r-component		;userinfo
		    nid			;host
		    nil			;port
		    nss			;path
		    q-component		;query
		    f-component		;fragment
		    ))
   elseif (and file-scheme
	       (multiple-value-setq (i path)
		 (state-uri-file string colon end)))
     then (values i scheme nil nil nil path)
   elseif (and scheme colon)
     then ;; Something like "mailto:foo@bar.com".  Put the
	  ;; the non-scheme part into the path
	  (values end scheme nil nil nil (xsubseq colon end))))

;; called by parse-uri-string-rfc3986
(defun state-uri-reference (string start end
			    &aux i scheme userinfo host port path query
				 fragment)
  ;; rule 02: URI-reference = URI / relative-ref
  ;; values: i scheme host userinfo port path query fragment
  (if* (multiple-value-setq (i scheme userinfo host port path query
			     fragment)
	 (state-uri string start end))
     then (values i scheme userinfo host port path query fragment)
   elseif (multiple-value-setq (i userinfo host port path query fragment)
	    (state-relative-ref string start end))
     then (values i nil userinfo host port path query fragment)))

;; called by parse-uri-string-rfc3986
(defun state-absolute-uri (string start end
			   &aux i scheme userinfo host port path query i2
				colon urn-scheme file-scheme)
  ;; rule 03: absolute-URI  = scheme ":" hier-part [ "?" query ]
  ;; --TERMINAL--
  ;; values: i scheme userinfo host port path query
  (if* (and (multiple-value-setq (i scheme) (state-scheme string start end))
	    (setq colon (looking-at #\: string i end))
	    (not (setq urn-scheme (looking-at "urn" string start end t)))
	    (not (setq file-scheme (looking-at "file" string start end t)))
	    (multiple-value-setq (i2 userinfo host port path)
	      (state-hier-part string colon end)))
     then ;; so far: scheme + ":" + hier-part
	  (setq i i2)
	  (if* (at-end-p i end)
	     then (values i scheme userinfo host port path)
	   elseif (and (looking-at #\? string i end)
		       (multiple-value-setq (i query)
			 (state-query string (incf i) end))
		       (at-end-p i end))
	     then (values i scheme userinfo host port path query))
   elseif urn-scheme
     then ;; values: i "urn" nid r-component nil nss q-component f-component
	  (multiple-value-bind (i3 nid nss q-component f-component r-component)
	      (state-urn-namestring string (incf i) end)
	    (when i3
	      (values i3
		      scheme
		      r-component	;userinfo
		      nid		;host
		      nil		;port
		      nss		;path
		      q-component	;query
		      f-component	;fragment
		      )))
   elseif (and file-scheme
	       (multiple-value-setq (i path)
		 (state-uri-file string colon end)))
     then (values i scheme nil nil nil path)
   elseif (and scheme colon)
     then  ;; Something like "mailto:foo@bar.com".  Put the
	  ;; the non-scheme part into the path
	  (values end scheme nil nil nil (xsubseq colon end))))
       
(defun state-hier-part (string start end &aux i userinfo host port
					      path i2)
  ;; rule 04: hier-part = "//" authority path-abempty
  ;;                    / "//" path-absolute            ***NEW***
  ;;                    / path-absolute
  ;;                    / path-rootless
  ;;                    / path-empty
  ;; values: i userinfo host port path
  (if* (and (setq i (looking-at "//" string start end))
	    (multiple-value-setq (i userinfo host port)
	      (state-authority string i end)))
     then (if* (multiple-value-setq (i2 path) (state-path-abempty string i end))
	     then (values i2 userinfo host port path)
	     else (values i userinfo host port))
   elseif (and (setq i (looking-at "//" string start end))
	       (multiple-value-setq (i path)
		 (state-path-absolute string i end)))
     then (values i nil nil nil path)
   elseif (or
	   (multiple-value-setq (i path) (state-path-absolute string start end))
	   (multiple-value-setq (i path) (state-path-rootless string start end))
	   (multiple-value-setq (i path) (state-path-empty string start end)))
     then (values i nil nil nil path)))
       
(defun state-relative-ref (string start end &aux i2 query fragment)
  ;; rule 05: relative-ref = relative-part [ "?" query ] [ "#" fragment ]
  ;; --TERMINAL--
  ;; values: i userinfo host port path query fragment
  (multiple-value-bind (i userinfo host port path)
      (state-relative-part string start end)
    (when i
      (if* (at-end-p i end)
	 then (values i userinfo host port path)
	 else (when (looking-at #\? string i end)
		(if* (multiple-value-setq (i2 query)
		       (state-query string (incf i) end))
		   then (setq i i2)
		   else (setq query #.*uri-null-marker*)))
	     
	      (when (looking-at #\# string i end)
		(if* (multiple-value-setq (i2 fragment)
		       (state-fragment string (incf i) end))
		   then (setq i i2)
		   else (setq fragment #.*uri-null-marker*)))
	     
	      (when (at-end-p i end)
		(values i userinfo host port path query fragment))))))
       
(defun state-relative-part (string start end
			    &aux (i start) path userinfo host port i2)
  ;; rule 06: relative-part = "//" authority path-abempty
  ;;                        / path-absolute
  ;;                        / path-noscheme
  ;;                        / path-empty
  ;; values: i userinfo host port path
  (if* (and (setq i (looking-at "//" string i end))
	    (multiple-value-setq (i userinfo host port)
	      (state-authority string i end)))
     then (if* (multiple-value-setq (i2 path) (state-path-abempty string i end))
	     then (values i2 userinfo host port path)
	     else (values i userinfo host port))
   elseif (or
	   (multiple-value-setq (i path) (state-path-absolute string start end))
	   (multiple-value-setq (i path) (state-path-noscheme string start end))
	   (multiple-value-setq (i path) (state-path-empty string start end)))
     then (values i nil nil nil path)))

(defun state-scheme (string start end &aux i scheme)
  ;; rule 07: scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
  ;; values: i scheme
  (when (looking-at *alpha-bitvector* string start end)
    (if* (setq i (scan-forward string (1+ start) end *scheme-bitvector*))
       then (setq scheme (xsubseq start i))
       else ;; just the one char
	    (setq scheme (xsubseq start (setq i (1+ start)))))
    (values i scheme)))

(defun state-authority (string start end &aux i i2 userinfo host ipv6 zone-id
					      port)
  ;; rule 08: authority = [ userinfo "@" ] host [ ":" port ]
  ;; values: i userinfo host port
  (cond
   ((and (multiple-value-setq (i userinfo) (state-userinfo string start end))
	 (setq i (looking-at #\@ string i end))
	 (multiple-value-setq (i host ipv6 zone-id)
	   (state-host string i end)))
    ;; Somewhat of a hack, but I don't want to change all the functions
    ;; to expect even more multiple values:
    (when ipv6 (setq host (list host ipv6 zone-id)))
      
    ;; have: userinfo "@" host
    (if* (not (setq i2 (looking-at #\: string i end)))
       then ;; done, return what we have
	    (values i userinfo host)
     elseif (multiple-value-setq (i port) (state-port string i2 end))
       then ;; found ":" and port
	    (values i userinfo host port)
       else ;; found ":" and NO port
	    (values i2 userinfo host)))

   ;; no userinfo, check for host
   ((multiple-value-setq (i host ipv6 zone-id) (state-host string start end))
    ;; Somewhat of a hack, but I don't want to change all the functions
    ;; to expect even more multiple values:
    (when ipv6 (setq host (list host ipv6 zone-id)))

    (if* (not (setq i2 (looking-at #\: string i end)))
       then (values i nil host)
     elseif (multiple-value-setq (i port) (state-port string i2 end))
       then (values i nil host port)
       else ;; found ":" and NO port
	    (values i2 nil host)))))

(defun state-userinfo (string start end &aux i)
  ;; rule 09: userinfo = *( unreserved / pct-encoded / sub-delims / ":" )
  ;; 
  ;; This one is more difficult, due to the alternation with
  ;; pct-encoded:
  ;;  *( unreserved / pct-encoded / sub-delims / ":" )
  ;; All the others are just characters, but pct-encoded is a
  ;; specific sequence of characters.
  (when (setq i (scan-forward string start end *userinfo-bitvector*
			      #'scan-pct-encoded))
    (values i (xsubseq start i))))

(defun state-port (string start end &aux i)
  ;; rule 11: port = *DIGIT
  (when (setq i (scan-forward string start end *digit-bitvector*))
    (values i (xsubseq start i))))

(defun state-host (string start end &aux i host ipv6 zone-id)
  ;; rule 10: host = IP-literal / IPv4address / reg-name
  ;; values: i host ipv6 zone-id
  (if* (multiple-value-setq (i ipv6 zone-id)
	 (state-ip-literal string start end))
     then (values i nil ipv6 zone-id)
   elseif (or
	   (multiple-value-setq (i host) (state-ipv4address string start end))
	   (multiple-value-setq (i host) (state-reg-name string start end)))
     then (values i host)))

(defun state-ip-literal (string start end &aux ip-start i2 end-ip ip zone-id)
  ;; rule 12a: IP-literal = "[" ( IPv6addrz / IPvFuture  ) "]"
  ;; values: i ipaddr zone-id
  ;; NOTE: the [ and ] are not returned as part of the host.
  (when (and (setq ip-start (looking-at #\[ string start end))
	     (or (multiple-value-setq (end-ip ip zone-id)
		   (state-ipv6addrz string ip-start end))
		 (multiple-value-setq (end-ip ip zone-id)
		   (state-ipvfuture string ip-start end)))
	     (setq i2 (looking-at #\] string end-ip end)))
    (values i2 ip zone-id)))

(defun state-ipv6addrz (string start end &aux ip-end zone-start zone-end)
  ;; rule 12b: IPv6addrz = IPv6address [ "%25" ZoneID ]
  ;; values: i ipaddr zone-id
  (when (setq ip-end (scan-ipv6address string start end))
    (if* (and (setq zone-start (looking-at "%25" string ip-end end))
	      (setq zone-end (scan-zone-id string zone-start end)))
       then (values zone-end
		    (xsubseq start ip-end)
		    (xsubseq zone-start zone-end))
       else (values ip-end (xsubseq start ip-end)))))

(defun scan-zone-id (string start end)
  ;; rule 12c: ZoneID  = 1*( unreserved / pct-encoded )
  (scan-forward string start end *unreserved-bitvector* #'scan-pct-encoded))

(defun state-ipvfuture (string start end &aux i)
  ;; rule 13:
  ;;    IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
  ;; values: i ipvfuture
  (when (and (setq i (looking-at #\v string start end))
	     (setq i (scan-forward string i end *hexdig-bitvector*))
	     (setq i (looking-at #\. string i end))
	     (setq i (scan-forward string i end *ipvfuture-bitvector*)))
    (values i (xsubseq start i))))

(defun scan-ipv6address (string start end &aux (i start))
  ;; rule 14:
  ;;  IPv6address =                            6( h16 ":" ) ls32  [1]
  ;;              /                       "::" 5( h16 ":" ) ls32  [2]
  ;;              / [               h16 ] "::" 4( h16 ":" ) ls32  [3]
  ;;              / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32  [4]
  ;;              / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32  [5]
  ;;              / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32  [6]
  ;;              / [ *4( h16 ":" ) h16 ] "::"              ls32  [7]
  ;;              / [ *5( h16 ":" ) h16 ] "::"              h16   [8]
  ;;              / [ *6( h16 ":" ) h16 ] "::"                    [9]
  ;;              /                       "::"                    [10]
  (or
   (and (setq i (scan-h16-colon-pairs string start end 6 6)) ;; [1]
	(setq i (scan-ls32 string i end)))
   (and (setq i (looking-at "::" string start end))          ;; [2]
	(setq i (scan-h16-colon-pairs string i end 5 5))
	(setq i (scan-ls32 string i end)))
   (and (setq i (scan-h16 string start end))                 ;; [3]
	(setq i (looking-at "::" string i end))
	(setq i (scan-h16-colon-pairs string i end 4 4))
	(setq i (scan-ls32 string i end)))
   (setq i (scan-ipv6address-part4 string start end))        ;; [4]
   (setq i (scan-ipv6address-part5 string start end))        ;; [5]
   (setq i (scan-ipv6address-part6 string start end))        ;; [6]
   (setq i (scan-ipv6address-part7 string start end))        ;; [7]
   (setq i (scan-ipv6address-part8 string start end))        ;; [8]
   (and (setq i (scan-h16-colon-pairs string start end 0 6)) ;; [9]
	(setq i (scan-h16 string i end))
	(setq i (looking-at "::" string i end)))
   (setq i (looking-at "::" string start end))               ;; [10]
   ))

(defun scan-ipv6address-part4 (string start end &aux i)
  ;; rule: [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
  (or (and (setq i (looking-at "::" string start end))
	   (setq i (scan-h16-colon-pairs string i end 3 3))
	   (setq i (scan-ls32 string i end)))
      
      (and (setq i (scan-h16-colon-pairs string start end 0 1))
	   (setq i (scan-h16 string i end))
	   (setq i (looking-at "::" string i end))
	   (setq i (scan-h16-colon-pairs string i end 3 3))
	   (setq i (scan-ls32 string i end)))))

(defun scan-ipv6address-part5 (string start end &aux i)
  ;; rule: [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
  (or (and (setq i (looking-at "::" string start end))
	   (setq i (scan-h16-colon-pairs string i end 2 2))
	   (setq i (scan-ls32 string i end)))
   
      (and (setq i (scan-h16-colon-pairs string start end 0 2))
	   (setq i (scan-h16 string i end))
	   (setq i (looking-at "::" string i end))
	   (setq i (scan-h16-colon-pairs string i end 2 2))
	   (setq i (scan-ls32 string i end)))))

(defun scan-ipv6address-part6 (string start end &aux i)
  ;; rule: [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
  (or (and (setq i (looking-at "::" string start end))
	   (setq i (scan-h16 string i end))
	   (setq i (looking-at #\: string i end))
	   (setq i (scan-ls32 string i end)))
      (and (setq i (scan-h16-colon-pairs string start end 0 3))
	   (setq i (scan-h16 string i end))
	   (setq i (looking-at "::" string i end))
	   (setq i (scan-h16 string i end))
	   (setq i (looking-at #\: string i end))
	   (setq i (scan-ls32 string i end)))))

(defun scan-ipv6address-part7 (string start end &aux i)
  ;; rule: [ *4( h16 ":" ) h16 ] "::"              ls32
  (or (and (setq i (looking-at "::" string start end))
	   (setq i (scan-ls32 string i end)))
      (and (setq i (scan-h16-colon-pairs string start end 0 4))
	   (setq i (scan-h16 string i end))
	   (setq i (looking-at "::" string i end))
	   (setq i (scan-ls32 string i end)))))

(defun scan-ipv6address-part8 (string start end &aux i)
  ;; rule: [ *5( h16 ":" ) h16 ] "::"              h16
  (or (and (setq i (looking-at "::" string start end))
	   (setq i (scan-h16 string i end)))
      (and (setq i (scan-h16-colon-pairs string start end 0 5))
	   (setq i (scan-h16 string i end))
	   (setq i (looking-at "::" string i end))
	   (setq i (scan-h16 string i end)))))

(defun scan-h16-colon-pairs (string start end min max
			     &aux (i start)
				  i2
				  (nfound 0))
  ;; subrule: min*max( h16 ":" )
  ;; Scan from min to max pairs of: h16 + ":"
  ;; NOTE: this function needs to lookahead to make sure there isn't a ::
  ;;       after the h16.
  (while (and (< nfound max)
	      (setq i2 (scan-h16 string i end))
	      (setq i2 (looking-at #\: string i2 end))
	      (< i2 end)
	      (not (looking-at #\: string i2 end)))
    (setq i i2)
    (incf nfound))
  (when (<= min nfound max)
    i))

(defun scan-h16 (string start end &aux i)
  ;; rule 15: h16 = 1*4HEXDIG
  (when (null start) (error "start is null"))
  (when (and (setq i
	       (scan-forward string start
			     ;; only look 5 ahead
			     (min end (the fixnum (+ start 5)))
			     *hexdig-bitvector*))
	     (<= 1 (the fixnum (- i start)) 4))
    i))

(defun scan-ls32 (string start end &aux i)
  ;; rule 16: ls32          = ( h16 ":" h16 ) / IPv4address
  (if* (and (setq i (scan-h16 string start end))
	    (setq i (looking-at #\: string i end))
	    (setq i (scan-h16 string i end)))
     then i
     else (scan-ipv4address string start end)))

(defun scan-ipv4address (string start end &aux i)
  ;; rule 17:
  ;;  IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet
  ;; values: i
  (and (setq i (scan-dec-octet string start end))
       (setq i (looking-at #\. string i end))
       (setq i (scan-dec-octet string i end))
       (setq i (looking-at #\. string i end))
       (setq i (scan-dec-octet string i end))
       (setq i (looking-at #\. string i end))
       (scan-dec-octet string i end)))

(defun state-ipv4address (string start end &aux i)
  ;; values: i ipv4
  (when (setq i (scan-ipv4address string start end))
    (values i (xsubseq start i))))

(defun scan-dec-octet (string start end &aux i)
  ;; rule 18:
  ;;   dec-octet     = DIGIT                 ; 0-9
  ;;                 / %x31-39 DIGIT         ; 10-99
  ;;                 / "1" 2DIGIT            ; 100-199
  ;;                 / "2" %x30-34 DIGIT     ; 200-249
  ;;                 / "25" %x30-35          ; 250-255
  ;; Honestly, the above makes little sense to me.  The truth is,
  ;; "http://256.0.0.1/" is a valid URI because even though it doesn't
  ;; parse as a dec-octet, it does parse as a reg-name (rule 19).
  (when (and (setq i (scan-forward string start end *digit-bitvector*))
	     (<= 1 (- i start) 3))
    i))
       
(defun state-reg-name (string start end &aux i)
  ;; rule 19: reg-name      = *( unreserved / pct-encoded / sub-delims )
  ;; values: i host
  (when (setq i (scan-forward string start end *reg-name-bitvector*
			      #'scan-pct-encoded))
    (values i (xsubseq start i))))
       
(defun state-path-abempty (string start end &aux i i2)
  ;; rule 21: path-abempty  = *( "/" *pchar )
  ;; values: i path
  ;; NOTE: if *strict-parse* is nil, we allow the leading "/" to be "//",
  ;;       because it is a common typo in HTML and sometimes fixing it is
  ;;       not under our control.  Browsers work fine with this
  ;;       non-conformance.
  (when (and (not *strict-parse*)
	     (looking-at "//" string start end))
    ;; double leading slash is changed to a single leading slash.
    (incf start))
  (setq i start)
  (loop
    (setq i2 nil)
    (if* (looking-at #\/ string i end)
       then (if* (setq i2 (scan-pchar string (1+ i) end))
	       then (setq i i2)
	       else (incf i) ;; advance for the / we found
		    (return))
       else (return)))
  (when (> i start)
    (values i (xsubseq start i))))

(defun state-path-absolute (string start end &aux (i start) i2 have-slash)
  ;; rule 22: path-absolute = "/" [ 1*pchar *( "/" *pchar ) ]
  ;;   remember: [ foo ] means 0*1( foo )
  ;; values: i path
  (when (setq i (looking-at #\/ string i end))
    (when (setq i2 (scan-pchar string i end))
      ;; parse is good to here
      (setq i i2
	    i2 nil)
      ;; Now, look for *( "/" *pchar )
      (while (and (setq have-slash (looking-at #\/ string i end))
		  (setq i2 (scan-pchar string have-slash end)))
	(setq i i2))
      ;; If it ends with a /:
      (when (and have-slash (not i2)) (incf i)))
    (values i (xsubseq start i))))

(defun state-path-noscheme (string start end &aux (i start) i2 have-slash)
  ;; rule 23: path-noscheme = segment-nz-nc *( "/" *pchar )
  ;; values: i path
  (when (setq i (scan-segment-nz-nc string i end))
    (while (and (setq have-slash (looking-at #\/ string i end))
		(setq i2 (scan-pchar string (1+ i) end)))
      (setq i i2))
    (when (and have-slash (not i2))
      ;; for the slash we did see:
      (incf i))
    (values i (xsubseq start i))))
       
(defun state-path-rootless (string start end &aux (i start) i2)
  ;; rule 24: path-rootless = 1*pchar *( "/" *pchar )
  ;; values: i path
  (when (setq i (scan-pchar string i end))
    (while (and (looking-at #\/ string i end)
		;; The pchar after the slash is optional
		(setq i2 (or (scan-pchar string (1+ i) end)
			     (1+ i))))
      (setq i i2))
    (values i (xsubseq start i))))

(defun state-path-empty (string start end)
  ;; rule 25: path-empty    = 0<pchar>
  ;; values: i path
  ;; NOTE: the RHS was updated in RFC 3986 errata to be "", but that is
  ;;       bogus. "" is very different the 0<pchar>.
  ;; Return nil when looking at a `pchar' and the null marker otherwise.
  (declare (optimize (safety 0))) 
  (if* (looking-at *pchar-bitvector* string start end)
     then nil
     else (values start #.*uri-null-marker*)))

(defun scan-segment-nz-nc (string start end)
  ;; rule 28: 1*( unreserved / pct-encoded / sub-delims / "@" )
  ;; In english: pchar without #\:
  (declare (optimize (safety 0))) 
  (scan-forward string start end *segment-nz-nc-bitvector* #'scan-pct-encoded))
       
(defun scan-pchar (string start end)
  ;; rule 29: pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
  (declare (optimize (safety 0))) 
  (scan-forward string start end *pchar-bitvector* #'scan-pct-encoded))
       
(defun state-query (string start end &aux i)
  ;; rule 30: *( pchar / "/" / "?" )
  ;; values: i query
  (when (setq i
	  (scan-forward string start end
			(if* *strict-parse*
			   then *query-bitvector-strict*
			   else *query-bitvector-non-strict*)
			#'scan-pct-encoded))
    (values i (xsubseq start i))))

(defun state-fragment (string start end &aux i)
  ;; rule 31: *( pchar / "/" / "?" / "#" )
  ;;   NOTE: Allegro CL added "#" in non-strict mode
  ;; values: i fragment
  (when (setq i
	  (scan-forward string start end
			(if* *strict-parse*
			   then *fragment-bitvector-strict*
			   else *fragment-bitvector-non-strict*)
			#'scan-pct-encoded))
    (values i (xsubseq start i))))

(defvar .pct-encoded.)

(defun scan-pct-encoded (string start end)
  ;; rule 32: pct-encoded   = "%" HEXDIG HEXDIG
  (declare (fixnum start end))
  (and (> (the fixnum (- end start)) 2) ;; ... at least 3 chars remaining
       (looking-at #\% string start end)
       (looking-at *hexdig-bitvector* string (incf start) end)
       (looking-at *hexdig-bitvector* string (incf start) end)
       (setq .pct-encoded. start)))

(defun state-uri-file (string start end &aux i)
  ;; rule: uri-file = "//" <anything>
  ;; --TERMINAL--
  ;; values: i path
  ;; It's not the job of the URI parser to validate file:// URIs.
  (when (setq i (looking-at "//" string start end))
    (values i (xsubseq i end))))

(defun state-urn-namestring (string start end
		  &aux (i start) i2 nid nss q-component f-component
		       r-component)
  ;; rule 50: namestring  = assigned-name
  ;;                      [ rq-components ]
  ;;                      [ "#" f-component ]
  ;; rule 58: f-component = fragment
  ;; START is just after "urn:".
  ;; values: i nid nss q-component f-component r-component
  (when (multiple-value-setq (i2 nid nss)
	  (state-urn-assigned-name string start end))
    (when (at-end-p i2 end)
      (return-from state-urn-namestring (values i2 nid nss)))
    
    (setq i i2)
    (when (multiple-value-setq (i2 r-component q-component)
	    (state-urn-rq-components string i end))
      (when (at-end-p i2 end)
	(return-from state-urn-namestring
	  (values i2 nid nss q-component nil r-component)))
      (setq i i2)
      ;; more STRING to process...
      
      (when (looking-at #\# string i end)
	(if* (multiple-value-setq (i2 f-component)
	       ;; Yes, the same fragment (RFC 8141 defines f-component in
	       ;; terms of RFC 3986's fragment).
	       (state-fragment string (incf i) end))
	   then (setq i i2)
	   else (setq f-component #.*uri-null-marker*)))
      
      (when (at-end-p i end)
	(values i2 nid nss q-component f-component r-component)))))

(defun state-urn-assigned-name (string start end &aux i i2 nid nss)
  ;; rule 51: assigned-name = "urn" ":" NID ":" NSS
  ;; START is just after "urn:".
  ;; values: i nid nss
  (when (and (multiple-value-setq (i2 nid) (state-urn-nid string start end))
	     (looking-at #\: string i2 end)
	     (setq i (1+ i2))
	     (multiple-value-setq (i2 nss) (state-urn-nss string i end)))
    (values i2 nid nss)))

(defun state-urn-nid (string start end &aux (i start))
  ;; rule 52: NID = (alphanum) 0*30(ldh) (alphanum)
  ;; rule 53: ldh = alphanum / "-"
  ;; values: i nid
  (declare (fixnum start end i))
  (when (and (looking-at *alphanum-bitvector* string i end)
	     (setq i (scan-forward string (1+ i) end *alphanum+-bitvector*))
	     ;; Check for <= 32 chars, thus far
	     (<= (the fixnum (- i start))
		 32)
	     ;; If the last one was alphanum, then we're done.
	     ;; If the last one was NOT alphanum, then:
	     ;;   1. make sure we had 30 chars (not 31)
	     ;;   2. look for another, single alphanum
	     (or (looking-at *alphanum-bitvector* string (1- i) end)
		 (and (<= (the fixnum (- i start))
			  31)
		      (not (at-end-p i end))
		      (setq i
			(scan-forward string i end *alphanum-bitvector*)))))
    (values i (xsubseq start i))))

(defun state-urn-nss (string start end &aux i i2)
  ;; rule 54: NSS = pchar *(pchar / "/")
  ;; values: i nss
  (when (setq i (scan-pchar string start (1+ start)))
    (if* (setq i2 (scan-forward
		   string i end
		   ;; See the definition of *urn-nss-chars* for
		   ;; why we don't use *pchar/-bitvector* here.
		   *urn-nss-bitvector*
		   #'scan-pct-encoded))
       then (values i2 (xsubseq start i2))
       else (values i (xsubseq start i)))))

(defun state-urn-rq-components (string start end
				&aux i ri qi r-component q-component)
  ;; rule 55: rq-components = [ "?+" r-component ]
  ;;                          [ "?=" q-component ]
  ;; values: i r-component q-component
  (when (and (setq i (looking-at #\? string start end))
	     (not (at-end-p i end))
	     (or (setq ri (looking-at #\+ string i end))
		 (setq qi (looking-at #\= string i end)))
	     (not (at-end-p (or ri qi) end)))
    (when (and ri (multiple-value-setq (i r-component)
		    (state-urn-r-component string ri end)))
      (when (at-end-p i end)
	(return-from state-urn-rq-components
	  (values i r-component)))
      
      (if* (setq qi (looking-at #\? string i end))
	 then (when (and (not (at-end-p qi end))
			 (setq qi (looking-at #\= string qi end))
			 (not (at-end-p qi end)))
		(when (multiple-value-setq (i q-component)
			(state-urn-q-component string qi end))
		  (return-from state-urn-rq-components
		    (values i r-component q-component))))
	 else (return-from state-urn-rq-components (values i r-component))))
    ;; The r-component branch didn't yield anything, check for q-component
    
    (when (and qi (multiple-value-setq (i q-component)
		    (state-urn-q-component string qi end)))
      (return-from state-urn-rq-components
	(values i nil q-component)))))

(defun scan-q-component-or-pct-encoded (string i end &aux i2)
  ;; Do what scan-pct-encoded does, BUT STOP scanning if we see "?=",
  ;; because that is the start of the q-component.
  ;;
  ;; This function is called by SCAN-FORWARD at each character position in
  ;; STRING.
  
  (when (setq i2 (scan-pct-encoded string i end))
    (return-from scan-q-component-or-pct-encoded i2))

  (when (setq i2 (looking-at #\? string i end))
    (if* (and
	  ;; at least 2 chars remaining (for 1 char after ?=)
	  (> (- end i2) 1)
	  (looking-at #\= string i2 end))
       then ;; stop scanning
	    (return-from scan-q-component-or-pct-encoded nil)
       else ;; return the index after the ?
	    (return-from scan-q-component-or-pct-encoded i2))))

(defun state-urn-r-component (string start end &aux i i2)
  ;; rule 56: r-component   = pchar *( pchar / "/" / "?" )
  ;; values: i r-component
  (when (setq i (scan-pchar string start end))
    (when (at-end-p i end)
      (return-from state-urn-r-component
	(values i (xsubseq start i))))
    (cond
     ((setq i2
	(scan-forward
	 string i end
	 ;; NOTE: we don't use *query-bitvector-strict* because we need
	 ;;       to handle #\? specially (see the next argument).
	 *urn-query-bitvector*
	 ;; NOTE: Because r-component can contain "?" without percent
	 ;;       encoding, when processing the r-component we need to
	 ;;       look ahead to make sure there is no #\= after each
	 ;;       #\? (since that means we have a q-component).
	 #'scan-q-component-or-pct-encoded))
      (values i2 (xsubseq start i2)))
     
     ;; We immediately ran into ?=, so return what we found so far:
     (t (values i (xsubseq start i))))))

(defun state-urn-q-component (string start end &aux i)
  ;; rule 57: q-component   = pchar *( pchar / "/" / "?" )
  ;; values: i q-component
  (when (setq i (looking-at *pchar-bitvector* string start end))
    (when (at-end-p i end)
      (return-from state-urn-q-component
	(values i (xsubseq start i))))
    (when (setq i
	    (scan-forward string i end *query-bitvector-strict*
			  #'scan-pct-encoded))
      (values i (xsubseq start i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-uri-string-rfc3986 (string
				 &aux (end (length string))
				      (.pct-encoded. nil)
				      real-host ipv6 zone-id)
  (declare (optimize (safety 0))
	   (fixnum end)) 

  (check-uri-string string)
  
  (multiple-value-bind (i scheme userinfo host port path query fragment)
      (state-absolute-uri string 0 end)
    (when i
      (if* (and host (consp host))
	 then (setq real-host (first host))
	      (setq ipv6 (second host))
	      (setq zone-id (third host))
	 else (setq real-host host))
      (when port
	(setq port (val string port))
	(setq port (parse-integer port :radix 10)))
      (return-from parse-uri-string-rfc3986
	(values (val string scheme)
		(val string real-host)
		(val string userinfo)
		port
		(val string path)
		(val string query)
		;; This is only non-nil for URNs
		(val string fragment)
		.pct-encoded.
		(val string ipv6)
		(val string zone-id)))))
    
  (multiple-value-bind (i scheme userinfo host port path query fragment)
      (state-uri-reference string 0 end)
    (when i
      (if* (and host (consp host))
	 then (setq real-host (first host))
	      (setq ipv6 (second host))
	      (setq zone-id (third host))
	 else (setq real-host host))
      (when port
	(setq port (val string port))
	(setq port (parse-integer port :radix 10)))
      (return-from parse-uri-string-rfc3986
	(values (val string scheme)
		(val string real-host)
		(val string userinfo)
		port
		(val string path)
		(val string query)
		(val string fragment)
		.pct-encoded.
		(val string ipv6)
		(val string zone-id)))))
    
  (excl::.parse-error "Couldn't parse uri: ~s." string))

(defun parse-uri (thing &key (class 'uri) (escape t))
  ;; Parse THING into a URI object, an instance of CLASS.
  ;;
  ;; If ESCAPE is non-nil, then decode percent-encoded characters in places
  ;; where they can legally appear, into the raw characters.  The exception
  ;; to this is when those characters are reserved for the component in
  ;; which they appear, and in this case the percent-encoded character
  ;; stays encoded.

  (when (uri-p thing) (return-from parse-uri thing))
  
  (multiple-value-bind (scheme host userinfo port path query fragment
			pct-encoded ipv6 zone-id)
      (parse-uri-string-rfc3986 thing)

    (when scheme
      (setq scheme
	(cond
	 ;; Ordered from most common to least, and the set of known schemes
	 ;; hardwired for efficiency.
	 ((string-equal scheme "https") :https)
	 ((string-equal scheme "http") :http)
	 ((string-equal scheme "ftp") :ftp)
	 ((string-equal scheme "file") :file)
	 ((string-equal scheme "urn") :urn)
	 ((string-equal scheme "telnet") :telnet)
	 (t
	  (intern (funcall
		   (case *current-case-mode*
		     ((:case-insensitive-upper :case-sensitive-upper)
		      #'string-upcase)
		     ((:case-insensitive-lower :case-sensitive-lower)
		      #'string-downcase))
		   scheme)
		  (load-time-value (find-package :keyword)))))))
    
    (when (and scheme (eq :urn scheme))
      (return-from parse-uri
	(make-instance 'urn :scheme scheme :nid host :nss path
		       :query query :fragment fragment
		       :r-component userinfo)))

    (when (and escape host)
      (setq host (percent-decode-string host *reg-name-bitvector*)))
    (when (and escape userinfo)
      (setq userinfo (percent-decode-string userinfo *userinfo-bitvector*)))
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
    (when (= 0 (length path))
      (setq path nil))
    (when (and escape path)
      (setq path (percent-decode-string path *pchar-bitvector*)))
    (when (and escape query)
      (setq query
	(percent-decode-string query
			       (if* *strict-parse*
				  then *decode-query-bitvector-strict*
				  else *decode-query-bitvector-non-strict*))))
    (when (and escape fragment)
      (setq fragment
	(percent-decode-string fragment
			       (if* *strict-parse*
				  then *fragment-bitvector-strict*
				  else *fragment-bitvector-non-strict*))))
    (if* (eq 'uri class)
       then ;; allow the compiler to optimize the make-instance call:
	    (make-instance 'uri
	      :scheme scheme
	      :host host
	      :ipv6 ipv6
	      :zone-id zone-id
	      :userinfo userinfo
	      :port port
	      :path path
	      :query query
	      :fragment fragment
	      :escaped (when escape pct-encoded))
       else ;; do it the slow way:
	    (make-instance class
	      :scheme scheme
	      :host host
	      :userinfo userinfo
	      :port port
	      :path path
	      :query query
	      :fragment fragment
	      :escaped (when escape pct-encoded)))))

(defun string-to-uri (string)
  ;; Parse STRING as a URI and either signal an error if it cannot be
  ;; parsed or return the URI object.  This function differs from parse-uri
  ;; in that the query is not decoded.  The knowledge of how to properly
  ;; decode the query is outside the bounds of RFC 3986.
  (multiple-value-bind (scheme host userinfo port path query fragment
			pct-encoded ;; non-nil if any %xx in any slot
			ipv6 zone-id)
      (parse-uri-string-rfc3986 string)

    (when scheme
      (setq scheme
	(cond
	 ;; Ordered from most common to least, and the set of known schemes
	 ;; hardwired for efficiency.
	 ((string-equal scheme "https") :https)
	 ((string-equal scheme "http") :http)
	 ((string-equal scheme "ftp") :ftp)
	 ((string-equal scheme "file") :file)
	 ((string-equal scheme "urn") :urn)
	 ((string-equal scheme "telnet") :telnet)
	 (t
	  (intern (funcall
		   (case *current-case-mode*
		     ((:case-insensitive-upper :case-sensitive-upper)
		      #'string-upcase)
		     ((:case-insensitive-lower :case-sensitive-lower)
		      #'string-downcase))
		   scheme)
		  (load-time-value (find-package :keyword)))))))
    
    (when (and scheme (eq :urn scheme))
      (return-from string-to-uri
	;; NOTE: for now, we treat URNs like parse-uri, and do no
	;; decoding.
	(make-instance 'urn :scheme scheme :nid host :nss path
		       :query query :fragment fragment
		       :r-component userinfo)))

    (when (and pct-encoded host)
      (setq host (percent-decode-string host *reg-name-bitvector*)))

    (when (and pct-encoded userinfo)
      (setq userinfo (percent-decode-string userinfo *userinfo-bitvector*)))
    
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

    (when (= 0 (length path))
      (setq path nil))
    (when (and pct-encoded path)
      (setq path (percent-decode-string path *pchar-bitvector*)))

    ;; query is left alone

    (when (and pct-encoded fragment)
      (setq fragment
	(percent-decode-string fragment
			       (if* *strict-parse*
				  then *fragment-bitvector-strict*
				  else *fragment-bitvector-non-strict*))))

    (make-instance 'uri
      :scheme scheme
      :host host
      :ipv6 ipv6
      :zone-id zone-id
      :userinfo userinfo
      :port port
      :path path
      :query query
      :fragment fragment
      :escaped pct-encoded)))

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
			  (if* escape
			     then (percent-decode-string s nil)
			     else s))
		      segments))
       else ;; no param
	    (setf (car pl)
	      (if* escape
		 then (percent-decode-string (car segments) nil)
		 else (car segments))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Printing

(defvar *render-include-slash-on-null-path* nil) ;; rfe11850

(defmethod render-uri ((uri uri) stream
		       &aux (encode (uri-escaped uri))
			    (*print-pretty* nil))
  (declare (optimize (safety 0)))
  (when (null (uri-string uri))
    (setf (uri-string uri)
      (let ((scheme (uri-scheme uri))
	    (host (.uri-host uri))
	    (ipv6 (.uri-ipv6 uri))
	    zone-id ;; don't compute until needed
	    (userinfo (uri-userinfo uri))
	    (port (uri-port uri))
	    (parsed-path (uri-parsed-path uri))
	    (query (uri-query uri))
	    (fragment (uri-fragment uri)))
	(string+
	  (when scheme
	    (case *current-case-mode*
	      ((:case-insensitive-upper :case-sensitive-upper)
	       (string-downcase (symbol-name scheme)))
	      ((:case-insensitive-lower :case-sensitive-lower)
	       (symbol-name scheme))))
	  (when scheme ":")
	  (when (or host ipv6 (eq :file scheme) (eq :hdfs scheme))
	    "//")
	  (when userinfo
	    (if* encode
	       then (percent-encode-string userinfo *userinfo-bitvector*)
	       else userinfo))
	  (when userinfo "@")
	  (if* ipv6
	     then (if* (setq zone-id (.uri-zone-id uri))
		     then (string+ "[" ipv6 "%25" zone-id "]")
		     else (string+ "[" ipv6 "]"))
	   elseif host
	     then (if* encode
		     then (percent-encode-string host *reg-name-bitvector*)
		     else host))
	  (when port ":")
	  (when port port)
	  (if* parsed-path
	     then (render-parsed-path parsed-path encode)
	   elseif (and *render-include-slash-on-null-path*
		       #|no path but:|# scheme host)
	     then "/")
	  (when query "?")
	  (when query
	    (if* encode
	       then (percent-encode-string
		     query
		     (if* *strict-parse*
			then *query-bitvector-strict*
			else *query-bitvector-non-strict*))
	       else query))
	  (when fragment "#")
	  (when fragment
	    (if* encode
	       then (percent-encode-string
		     fragment
		     (if* *strict-parse*
			then *fragment-bitvector-strict*
			else *fragment-bitvector-non-strict*))
	       else fragment))))))
  (if* stream
     then (princ (uri-string uri) stream)
     else (uri-string uri)))

(defmethod render-uri ((urn urn) stream
		       &aux (*print-pretty* nil))
  ;; This doesn't do encoding because no decoding is done for URNs when
  ;; they are parsed.
  (when (null (uri-string urn))
    (setf (uri-string urn)
      (let ((nid (urn-nid urn))
	    (nss (urn-nss urn))
	    (r (urn-r-component urn))
	    (q (urn-q-component urn))
	    (f (urn-f-component urn)))
	(string+ "urn:" nid ":" nss
		 (when r "?+")
		 (when r r)
		 (when q "?=")
		 (when q q)
		 (when f "#")
		 (when f f)))))
  (if* stream
     then (write-string (uri-string urn) stream)
     else (uri-string urn)))

(defmethod uri-to-string ((uri uri)
			  &aux (encode (uri-escaped uri))
			       (*print-pretty* nil)
			       res)
  (declare (optimize (safety 0)))
  (when (null (setq res (uri-string uri)))
    (setf (uri-string uri)
      (let ((scheme (uri-scheme uri))
	    (host (.uri-host uri))
	    (ipv6 (.uri-ipv6 uri))
	    zone-id ;; don't compute until needed
	    (userinfo (uri-userinfo uri))
	    (port (uri-port uri))
	    (parsed-path (uri-parsed-path uri))
	    (query (uri-query uri))
	    (fragment (uri-fragment uri)))
	(setq res
	  (string+
	   (when scheme
	     (case *current-case-mode*
	       ((:case-insensitive-upper :case-sensitive-upper)
		(string-downcase (symbol-name scheme)))
	       ((:case-insensitive-lower :case-sensitive-lower)
		(symbol-name scheme))))
	   (when scheme ":")
	   (when (or host ipv6 (eq :file scheme) (eq :hdfs scheme))
	     "//")
	   (when userinfo
	     (if* encode
		then (percent-encode-string userinfo *userinfo-bitvector*)
		else userinfo))
	   (when userinfo "@")
	   (if* ipv6
	      then (if* (setq zone-id (.uri-zone-id uri))
		      then (string+ "[" ipv6 "%25" zone-id "]")
		      else (string+ "[" ipv6 "]"))
	    elseif host
	      then (if* encode
		      then (percent-encode-string host *reg-name-bitvector*)
		      else host))
	   (when port ":")
	   (when port port)
	   (if* parsed-path
	      then (render-parsed-path parsed-path encode)
	    elseif (and *render-include-slash-on-null-path*
			#|no path but:|# scheme host)
	      then "/")
	   (when query "?")
	   query
	   (when fragment "#")
	   (when fragment
	     (if* encode
		then (percent-encode-string
		      fragment
		      (if* *strict-parse*
			 then *fragment-bitvector-strict*
			 else *fragment-bitvector-non-strict*))
		else fragment)))))))
  res)

(defmethod uri-to-string ((urn urn))
  ;; We can use render-uri here because no decoding/encoding happens for
  ;; URNs.
  (render-uri urn nil))

(defun render-parsed-path (path-list escape)
  (do* ((res '())
	(first (car path-list))
	(pl (cdr path-list) (cdr pl))
	(pe (car pl) (car pl)))
      ((null pl)
       (when res (apply #'string+ (nreverse res))))
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
       then (if* escape
	       then (push (percent-encode-string pe *pchar-bitvector*)
			  res)
	       else (push pe res))
       else ;; contains params
	    (if* escape
	       then (push (percent-encode-string (car pe) *pchar-bitvector*)
			  res)
	       else (push (car pe) res))
	    (dolist (item (cdr pe))
	      (push ";" res)
	      (if* escape
		 then (push (percent-encode-string item *pchar-bitvector*)
			    res)
		 else (push item res))))))

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
  ;; When PLACE is nil, this function returns a new URI.
  ;; When PLACE is non-nil, it is return.
  (tagbody
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

    (when (uri-scheme uri) (go :done))

    (setf (uri-scheme uri) (uri-scheme base))
  
    ;; if URI has a host, we're done
    (when (uri-host uri) (go :done))
    
    (set-host uri
	      (.uri-host base)
	      (.uri-ipv6 base)
	      (.uri-zone-id base))
    (setf (uri-userinfo uri) (uri-userinfo base))
    (setf (uri-port uri) (uri-port base))
    
    (let ((p (uri-parsed-path uri)))
      (when (null p)
	(setf (uri-path uri) (uri-path base))
	(go :done))
      
      (when (and p (eq :absolute (car p)))
	(if* (equal '(:absolute "") p)
	   then ;; Canonicalize the way parsing does:
		(setf (uri-path uri) nil)
	 elseif (eq :absolute (first p))
	   then ;; this also sets uri-path
		(multiple-value-bind (new changed)
		    (canonicalize-path-list p)
		  (when changed
		    (setf (uri-parsed-path uri) new))))
	(go :done)))
    
    (let* ((base-path
	    (or (uri-parsed-path base)
		;; needed because we canonicalize away a path of just `/':
		'(:absolute "")))
	   (path (uri-parsed-path uri))
	   new-path-list)
      (when (not (eq :absolute (car base-path)))
	(error "Cannot merge ~a and ~a, since the latter is not absolute."
	       uri base))

      (setq new-path-list
	(append (butlast base-path)
		(if* path then (cdr path) else '(""))))

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

      (when (eq :absolute (first new-path-list))
	(multiple-value-bind (new changed)
	    (canonicalize-path-list new-path-list)
	  (when changed (setq new-path-list new))))
      
      ;; Also sets uri-path:
      (setf (uri-parsed-path uri) new-path-list))

   :done
    (return-from merge-uris uri)))

(defun canonicalize-path-list (path-list &aux changed)
  ;; Return two values: new version of PATH-LIST and an indicator if it was
  ;; changed.  We are only called when (car path-list) is :absolute.
  (while (or (equal "." (second path-list))
	     (equal ".." (second path-list)))
    (setf (cdr path-list) (cddr path-list))
    (setq changed t))
  (values path-list changed))

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
  ;; Like ENOUGH-PATHNAME, but for URIs.
  (let ((new-scheme nil)
	(new-host nil)
	(new-ipv6 nil)
	(new-zone-id nil)
	(new-userinfo nil)
	(new-port nil)
	(new-parsed-path nil))

    ;; If the scheme and authority are not the same, then return URI.
    (when (or (and (uri-scheme uri)
		   (not (equalp (uri-scheme uri) (uri-scheme base))))
	      ;; We don't use uri-authority, because it conses a lot.
	      (and (uri-host uri)
		   (not (equalp (uri-host uri) (uri-host base))))
	      (not (equalp (uri-userinfo uri) (uri-userinfo base)))
	      (not (equalp (uri-port uri) (uri-port base))))
      (return-from enough-uri uri))

    ;; For this group, if the slot is nil in URI, then the return value is
    ;; copied from from BASE:
    (when (null (uri-scheme uri)) (setq new-scheme (uri-scheme base)))
    (when (null (uri-host uri))
      ;; These are copied as a unit:
      (setq new-host (.uri-host base))
      (setq new-ipv6 (.uri-ipv6 base))
      (setq new-zone-id (.uri-zone-id base)))
    (when (null (uri-userinfo uri)) (setq new-userinfo (uri-userinfo base)))
    (when (null (uri-port uri)) (setq new-port (uri-port base)))

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
	   (or (when new-parsed-path
		 (render-parsed-path new-parsed-path
				     ;; don't know, so have to assume:
				     t))
	       ;; can't have a completely empty uri!
	       "/")))
      (copy-uri nil :class (class-of uri) :place place
            ;;; these come from base if the original slot was nil
		:scheme new-scheme
		:host new-host
		:ipv6 new-ipv6
		:zone-id new-zone-id
		:userinfo new-userinfo
		:port new-port
		:path new-path
		:parsed-path new-parsed-path
            ;;; never from base... why? is this documented?
		:query (uri-query uri)
		:fragment (uri-fragment uri)
		:plist (copy-list (uri-plist uri))))))

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
  ;; On Windows, turn file:///d:/foo/bar.cl into #p"d:/foo/bar.cl"
  ;; On UNIX,    turn file:///foo/bar.cl    into #p"/foo/bar.cl"
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
		      (char= #\: (schar path 2)))
	     (setq path (subseq path 1))
	     (setf (schar path 1) #\:))
	   path))
    (pathname
     (percent-decode-string
      #+mswindows (frob-uri-file-string (uri-path uri))
      #-mswindows (uri-path uri)
      nil))))

(defun pathname-to-uri (pathname)
  (when (not (excl::absolute-pathname-p pathname t))
    (error "A relative pathname cannot be converted to a URI: ~s." pathname))
  (parse-uri
   (let ((s (percent-encode-string
	     #+mswindows (substitute #\/ #\\ (namestring pathname))
	     #-mswindows (namestring pathname)
	     *pchar/-bitvector*)))
     #-mswindows (format nil "file://~a" s)
     #+mswindows (if* (pathname-device pathname)
		    then (format nil "file:///~a" s)
		    else (format nil "file://~a" s)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :uri)
(pushnew :rfc3986 *features*)
(pushnew :rfc6874 *features*)
(pushnew :rfc8141 *features*)

#+ignore ;; debugging only
(progn
  (trace parse-uri-string-rfc3986)
  #+ignore (trace xsubseq)
  #+ignore (trace val)
  (trace looking-at)
  (trace scan-forward)

  (trace state-uri)
  (trace state-uri-reference)
  (trace state-absolute-uri)
  (trace state-hier-part)
  (trace state-relative-ref)
  (trace state-relative-part)
  (trace state-scheme)
  (trace state-authority)
  (trace state-userinfo)
  (trace state-port)
  (trace state-host)
  (trace state-ip-literal)
  (trace state-ipv6addrz)
  (trace scan-zone-id)
  (trace state-ipvfuture)
  (trace scan-ipv6address)
  (trace scan-ipv6address-part4)
  (trace scan-ipv6address-part5)
  (trace scan-ipv6address-part6)
  (trace scan-ipv6address-part7)
  (trace scan-ipv6address-part8)
  (trace scan-h16-colon-pairs)
  (trace scan-h16)
  (trace scan-ls32)
  (trace state-ipv4address)
  (trace scan-dec-octet)
  (trace state-reg-name)
  (trace state-path-abempty)
  (trace state-path-absolute)
  (trace state-path-noscheme)
  (trace state-path-rootless)
  (trace state-path-empty)
  (trace scan-segment-nz-nc)
  (trace scan-pchar)
  (trace state-query)
  (trace state-fragment)
  (trace scan-pct-encoded)

  (trace state-uri-file)

  (trace state-urn-namestring)
  (trace state-urn-assigned-name)
  (trace state-urn-nid)
  (trace state-urn-nss)
  (trace state-urn-rq-components)
  (trace state-urn-r-component)
  (trace state-urn-q-component)
  )
