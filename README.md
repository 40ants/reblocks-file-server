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
* Depends on: [40ants-routes][25b9], [cl-fad][1059], [cl-ppcre][49b9], [local-time][46a1], [log4cl][7f8b], [reblocks][184b], [reblocks-ui2][85c5], [routes][48e8], [serapeum][c41d], [str][ef7f], [trivial-mimes][a154]

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

#### [package](5cd8) `reblocks-file-server/core`

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-FILE-SERVER-2FCORE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40REBLOCKS-FILE-SERVER-2FCORE-24FILE-SERVER-ROUTE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### FILE-SERVER-ROUTE

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-ROUTE-20CLASS-29"></a>

###### [class](9468) `reblocks-file-server/core:file-server-route` (page-route)

**Readers**

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AFILENAME-FILTER-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-ROUTE-29-29"></a>

###### [reader](f148) `reblocks-file-server/core:filename-filter` (file-server-route) (:filter = "")

A regular expression to show only selected files.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-DIR-LISTING-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-ROUTE-29-29"></a>

###### [reader](9366) `reblocks-file-server/core:get-dir-listing` (file-server-route) (:dir-listing = t)

When nil, directory contents is not shown.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-ROOT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-ROUTE-29-29"></a>

###### [reader](bc44) `reblocks-file-server/core:get-root` (file-server-route) (:root)

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-FILE-SERVER-2FCORE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-20FUNCTION-29"></a>

##### [function](2ebe) `reblocks-file-server/core:file-server` uri-path &key name (route-class 'file-server-route) (root "./") (dir-listing t) (filter ".\*")

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ALIST-DIRECTORY-20FUNCTION-29"></a>

##### [function](39c1) `reblocks-file-server/core:list-directory` full-path filter

Returns a list of files in the directory.
All items of the list are relative.


[f449]: https://40ants.com/reblocks-file-server/
[b09a]: https://github.com/40ants/reblocks-file-server
[4729]: https://github.com/40ants/reblocks-file-server/actions
[5cd8]: https://github.com/40ants/reblocks-file-server/blob/65e0b8f450617926d5c12f61a72de07123f01bf0/src/core.lisp#L1
[39c1]: https://github.com/40ants/reblocks-file-server/blob/65e0b8f450617926d5c12f61a72de07123f01bf0/src/core.lisp#L130
[9468]: https://github.com/40ants/reblocks-file-server/blob/65e0b8f450617926d5c12f61a72de07123f01bf0/src/core.lisp#L56
[bc44]: https://github.com/40ants/reblocks-file-server/blob/65e0b8f450617926d5c12f61a72de07123f01bf0/src/core.lisp#L57
[9366]: https://github.com/40ants/reblocks-file-server/blob/65e0b8f450617926d5c12f61a72de07123f01bf0/src/core.lisp#L60
[f148]: https://github.com/40ants/reblocks-file-server/blob/65e0b8f450617926d5c12f61a72de07123f01bf0/src/core.lisp#L65
[2ebe]: https://github.com/40ants/reblocks-file-server/blob/65e0b8f450617926d5c12f61a72de07123f01bf0/src/core.lisp#L72
[a450]: https://github.com/40ants/reblocks-file-server/issues
[25b9]: https://quickdocs.org/40ants-routes
[1059]: https://quickdocs.org/cl-fad
[49b9]: https://quickdocs.org/cl-ppcre
[46a1]: https://quickdocs.org/local-time
[7f8b]: https://quickdocs.org/log4cl
[184b]: https://quickdocs.org/reblocks
[85c5]: https://quickdocs.org/reblocks-ui2
[48e8]: https://quickdocs.org/routes
[c41d]: https://quickdocs.org/serapeum
[ef7f]: https://quickdocs.org/str
[a154]: https://quickdocs.org/trivial-mimes

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
