uri - Uniform Resource Identifiers for Common Lisp
==================================================

Table of contents
-----------------

 * Description
 * Author comments
 * Documentation
 * Dependencies
 * Installation
 * Configuration
 * Licence

Description
-----------

Uniform Resource Identifiers (URIs) for Common Lisp. URIs, a superset
of URLs, define a syntax for identifying an abstract or physical
resources on the WWW.  Read the referenced RFCs for more information.

The original parser was RFC 2396 compliant, but has updated to
follow RFC 3986 (URIs), RFC 8141 (URNs) and RFC 6874 (IPv6 syntax).

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

    (load (compile-file "/path/to/your/t-uri.cl"))

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

For more information, see <http://opensource.franz.com>.
