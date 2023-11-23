<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# reblocks-file-server - A Reblocks extension allowing to create routes for serving static files from disk.

<a id="reblocks-file-server-asdf-system-details"></a>

## REBLOCKS-FILE-SERVER ASDF System Details

* Description: A Reblocks extension allowing to create routes for serving static files from disk.
* Licence: Unlicense
* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
* Homepage: [https://40ants.com/reblocks-file-server/][f449]
* Bug tracker: [https://github.com/40ants/reblocks-file-server/issues][a450]
* Source control: [GIT][b09a]
* Depends on: [cl-fad][1059], [cl-ppcre][49b9], [reblocks][184b], [routes][48e8], [trivial-mimes][a154]

[![](https://github-actions.40ants.com/40ants/reblocks-file-server/matrix.svg?only=ci.run-tests)][4729]

![](http://quickdocs.org/badge/reblocks-file-server.svg)

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :reblocks-file-server)
```
<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

Here is a few examples on how this library can be used. These lines can be added into the code
which starts your Reblocks web application inside the `initialize-instance` method of your app:

```lisp
(defmethod initialize-instance ((app app) &rest args)
  (declare (ignorable args))

  (reblocks-file-server:make-route :root (asdf:system-relative-pathname "ultralisp"
                                                                        "images/")
                                   :uri "/images/")
  (call-next-method))
```
This is how to serve all `*.txt` files from the `/var/www` folder:

```lisp
(reblocks-file-server:make-route :uri "/static/"
                                 :root "/var/www/"
                                 :dir-listing nil
                                 :filter ".*.txt")
```
You also can provide a `DIR-LISTING` argument to repond on /static/ route with a rendered directory listing:

```lisp
(reblocks-file-server:make-route :uri "/static/"
                                 :root "/var/www/"
                                 :dir-listing t
                                 :filter ".*.txt")
```
In case if you want to serve all files except `*.txt`, you can negate filter expression by giving `NIL` in `FILTER-TYPE` argument:

```lisp
(reblocks-file-server:make-route :uri "/static/"
                                 :root "/var/www/"
                                 :dir-listing t
                                 :filter ".*.txt"
                                 :filter-type nil)
```
<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40REBLOCKS-FILE-SERVER-2FCORE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### REBLOCKS-FILE-SERVER/CORE

<a id="x-28-23A-28-2825-29-20BASE-CHAR-20-2E-20-22REBLOCKS-FILE-SERVER-2FCORE-22-29-20PACKAGE-29"></a>

#### [package](3b1a) `reblocks-file-server/core`

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-FILE-SERVER-2FCORE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40REBLOCKS-FILE-SERVER-2FCORE-24STATIC-FILES-ROUTE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### STATIC-FILES-ROUTE

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ASTATIC-FILES-ROUTE-20CLASS-29"></a>

###### [class](d625) `reblocks-file-server/core:static-files-route` (route)

**Readers**

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-DIR-LISTING-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3ASTATIC-FILES-ROUTE-29-29"></a>

###### [reader](ac28) `reblocks-file-server/core:get-dir-listing` (static-files-route) (:dir-listing = t)

When nil, directory contents is not shown.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-FILTER-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3ASTATIC-FILES-ROUTE-29-29"></a>

###### [reader](dee5) `reblocks-file-server/core:get-filter` (static-files-route) (:filter)

A regular expression.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-FILTER-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3ASTATIC-FILES-ROUTE-29-29"></a>

###### [reader](f1f5) `reblocks-file-server/core:get-filter-type` (static-files-route) (:filter-type = t)

T means show files that match the filter regexp. `NIL` means hide files that match the filter regexp

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-ROOT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3ASTATIC-FILES-ROUTE-29-29"></a>

###### [reader](d20c) `reblocks-file-server/core:get-root` (static-files-route) (:root)

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-URI-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3ASTATIC-FILES-ROUTE-29-29"></a>

###### [reader](b0cb) `reblocks-file-server/core:get-uri` (static-files-route) (:uri)

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-FILE-SERVER-2FCORE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ARENDER-404-20GENERIC-FUNCTION-29"></a>

##### [generic-function](0da3) `reblocks-file-server/core:render-404` route uri

Returns a string with `HTML` for a case when `uri' wasn't found on the disk.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ARENDER-DIRECTORY-20GENERIC-FUNCTION-29"></a>

##### [generic-function](8c12) `reblocks-file-server/core:render-directory` route uri children

Renders a list of files in a directory

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ARENDER-STYLES-20GENERIC-FUNCTION-29"></a>

##### [generic-function](0f21) `reblocks-file-server/core:render-styles` route

This method should use reblocks/html:with-html and output a :style element.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ASERVE-DIRECTORY-20GENERIC-FUNCTION-29"></a>

##### [generic-function](8220) `reblocks-file-server/core:serve-directory` route uri full-path

Returns a Lack response with a rendered directory listing.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ASERVE-FILE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](7bed) `reblocks-file-server/core:serve-file` route full-path

Returns content of the file.

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-FILE-SERVER-2FCORE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ALIST-DIRECTORY-20FUNCTION-29"></a>

##### [function](3536) `reblocks-file-server/core:list-directory` full-path filter filter-type

Returns a list of files in the directory.
All items of the list are relative.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AMAKE-ROUTE-20FUNCTION-29"></a>

##### [function](2707) `reblocks-file-server/core:make-route` &key (route-class 'static-files-route) (uri "/") (root "./") (dir-listing t) (filter ".\*") (filter-type t)


[f449]: https://40ants.com/reblocks-file-server/
[b09a]: https://github.com/40ants/reblocks-file-server
[4729]: https://github.com/40ants/reblocks-file-server/actions
[3b1a]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L1
[3536]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L100
[d625]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L35
[d20c]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L36
[b0cb]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L39
[ac28]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L42
[dee5]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L47
[f1f5]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L52
[2707]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L58
[8220]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L80
[7bed]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L84
[8c12]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L88
[0da3]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L92
[0f21]: https://github.com/40ants/reblocks-file-server/blob/934285cca4e013653a4028a3abb6e1c579b36ae7/src/core.lisp#L96
[a450]: https://github.com/40ants/reblocks-file-server/issues
[1059]: https://quickdocs.org/cl-fad
[49b9]: https://quickdocs.org/cl-ppcre
[184b]: https://quickdocs.org/reblocks
[48e8]: https://quickdocs.org/routes
[a154]: https://quickdocs.org/trivial-mimes

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
