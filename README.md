uri - Uniform Resource Identifiers for Common Lisp
==================================================

Table of contents
-----------------

 * Description
 * Author
 * Author comments
 * Documentation
 * Platforms
 * Dependencies
 * Installation
 * Configuration
 * Licence
 * Notes
 * Examples
 * Franz Inc. Open Source Info

Description
-----------

Uniform Resource Identifiers (URIs) for Common Lisp. URIs, a superset
of URLs, define a syntax for identifying an abstract or physical
resource.  Read the RFC below for more general information about
URIs.

This is the same URI implementation that first appeared in Allegro CL
6.0. Following RFC 2396 as closely as possible was an important goal
for the project, and some of the tests came directly from the RFC.
RFC 2396 has since been obsoleted by RFC 3986.  At least in one
contentious area, relative URL resolution with only a query component,
our implementation follows RFC 3986 and not 2396.  That is

    (net.uri:merge-uris (net.uri:parse-uri "?bar")
                        (net.uri:parse-uri "http://example.com/foo"))

yields

    #<uri http://example.com/foo?bar>

and not

    #<uri http://example.com/?bar>

as RFC 2396 would indicate.

In the future, further comparisons between the implementation and RFC
2396 will be done, and changes made to bring our implementation into
compliance with it.

Author
------

Kevin Layer, Franz Inc.

Platforms
----------

All ACL versions.

Dependencies
------------

Allegro Common Lisp and the
[tester](http://github.com/franzinc/tester/tree/master) library for
the test suite.

Installation
------------

Start your lisp and load uri.cl which is part of this project:

    (load (compile-file "/path/to/your/uri.cl"))

To run the test suite, load the tests.

    (load (compile-file "/pathto/your/t-uri.cl"))

Configuration
-------------

Setting the variable `net.url:*strict-parse*` to a boolean will
control how strict the parser is.

Documentation
-------------

See the [franz uri documentation]
(http://franz.com/support/documentation/current/doc/uri.htm).

License
-------

The uri source code is licensed under the terms of the 
[Lisp Lesser GNU Public License](http://opensource.franz.com/preamble.html), 
known as the LLGPL. The LLGPL consists of a preamble and the LGPL. Where these 
conflict, the preamble takes precedence.  This project is referenced in the 
preamble as the LIBRARY.

Notes
-----

See the file rfc2396.txt that is part of this project for more info.

Examples and Information
------------------------

See the link in the documentation section above.

Franz Open Source Info
----------------------

This project's homepage is <http://opensource.franz.com>. There is an 
informal community support and development mailing list 
[opensource@franz.com](http://opensource.franz.com/mailinglist.html) 
for these open source projects. We encourage you to take advantage by 
subscribing to the list.  Once you're subscribed, email to 
<opensource@franz.com> with your questions, comments, suggestions, 
and patches.


