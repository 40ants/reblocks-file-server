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
* Depends on: [cl-fad][1059], [cl-ppcre][49b9], [log4cl][7f8b], [reblocks][184b], [routes][48e8], [trivial-mimes][a154]

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

#### [package](d038) `reblocks-file-server/core`

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-FILE-SERVER-2FCORE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40REBLOCKS-FILE-SERVER-2FCORE-24STATIC-FILES-ROUTE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### STATIC-FILES-ROUTE

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ASTATIC-FILES-ROUTE-20CLASS-29"></a>

###### [class](cfee) `reblocks-file-server/core:static-files-route` (route)

**Readers**

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-DIR-LISTING-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3ASTATIC-FILES-ROUTE-29-29"></a>

###### [reader](59cb) `reblocks-file-server/core:get-dir-listing` (static-files-route) (:dir-listing = t)

When nil, directory contents is not shown.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-FILTER-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3ASTATIC-FILES-ROUTE-29-29"></a>

###### [reader](5a0e) `reblocks-file-server/core:get-filter` (static-files-route) (:filter)

A regular expression.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-FILTER-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3ASTATIC-FILES-ROUTE-29-29"></a>

###### [reader](9cbc) `reblocks-file-server/core:get-filter-type` (static-files-route) (:filter-type = t)

T means show files that match the filter regexp. `NIL` means hide files that match the filter regexp

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-ROOT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3ASTATIC-FILES-ROUTE-29-29"></a>

###### [reader](00f4) `reblocks-file-server/core:get-root` (static-files-route) (:root)

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-URI-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3ASTATIC-FILES-ROUTE-29-29"></a>

###### [reader](1db7) `reblocks-file-server/core:get-uri` (static-files-route) (:uri)

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-FILE-SERVER-2FCORE-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ARENDER-404-20GENERIC-FUNCTION-29"></a>

##### [generic-function](2a40) `reblocks-file-server/core:render-404` route uri

Returns a string with `HTML` for a case when `uri' wasn't found on the disk.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ARENDER-DIRECTORY-20GENERIC-FUNCTION-29"></a>

##### [generic-function](3e72) `reblocks-file-server/core:render-directory` route uri children

Renders a list of files in a directory

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ARENDER-STYLES-20GENERIC-FUNCTION-29"></a>

##### [generic-function](04aa) `reblocks-file-server/core:render-styles` route

This method should use reblocks/html:with-html and output a :style element.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ASERVE-DIRECTORY-20GENERIC-FUNCTION-29"></a>

##### [generic-function](4b8f) `reblocks-file-server/core:serve-directory` route uri full-path

Returns a Lack response with a rendered directory listing.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ASERVE-FILE-20GENERIC-FUNCTION-29"></a>

##### [generic-function](6cc9) `reblocks-file-server/core:serve-file` route full-path

Returns content of the file.

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-FILE-SERVER-2FCORE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ALIST-DIRECTORY-20FUNCTION-29"></a>

##### [function](9269) `reblocks-file-server/core:list-directory` full-path filter filter-type

Returns a list of files in the directory.
All items of the list are relative.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AMAKE-ROUTE-20FUNCTION-29"></a>

##### [function](1cfa) `reblocks-file-server/core:make-route` &key (route-class 'static-files-route) (uri "/") (root "./") (dir-listing t) (filter ".\*") (filter-type t)


[f449]: https://40ants.com/reblocks-file-server/
[b09a]: https://github.com/40ants/reblocks-file-server
[4729]: https://github.com/40ants/reblocks-file-server/actions
[d038]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L1
[9269]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L101
[cfee]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L36
[00f4]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L37
[1db7]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L40
[59cb]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L43
[5a0e]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L48
[9cbc]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L53
[1cfa]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L59
[4b8f]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L81
[6cc9]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L85
[3e72]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L89
[2a40]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L93
[04aa]: https://github.com/40ants/reblocks-file-server/blob/3c9c75c08b2f428afd6b447f80682c96c8969dac/src/core.lisp#L97
[a450]: https://github.com/40ants/reblocks-file-server/issues
[1059]: https://quickdocs.org/cl-fad
[49b9]: https://quickdocs.org/cl-ppcre
[7f8b]: https://quickdocs.org/log4cl
[184b]: https://quickdocs.org/reblocks
[48e8]: https://quickdocs.org/routes
[a154]: https://quickdocs.org/trivial-mimes

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
