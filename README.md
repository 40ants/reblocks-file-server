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
* Depends on: [40ants-routes][25b9], [alexandria][8236], [cl-fad][1059], [cl-ppcre][49b9], [local-time][46a1], [log4cl][7f8b], [reblocks][184b], [reblocks-ui2][85c5], [routes][48e8], [serapeum][c41d], [str][ef7f], [trivial-mimes][a154]

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

A small demo of what this library does:

![](https://storage.yandexcloud.net/40ants-public/reblocks-file-server/reblocks-file-server.gif)

Here is an example on how this library can be used. All you need is to add a results of call to
[`file-server`][7dcb] function call to you Reblocks application's routes.

```lisp
(defapp app
  :prefix "/"
  :routes ((page ("/" :name "index")
             (make-landing-page))
           
           ;; On /documents/
           (file-server "/sources/"
                        :name "sources"
                        :root (asdf:system-relative-pathname :my-app "./"))))
```
In this example, we will show sources of the `my-app` `ASDF` library starting
from path `/sources/`, ie if user opens your site like https://example.com/sources/,
he will see content of the directory returned by `(asdf:system-relative-pathname :my-app "./")`.

By default, all files and directories are shown. If you want to hide something from user,
you might provide a list of functions which accepts a pathname and returns `:ALLOW` `:DENY` or `NIL`.

Here is a relatively complex example of filtering:

```
(file-server "/sources/"
             :name "sources"
             :root (asdf:system-relative-pathname :my-app "./")
             :filter (list
                                ;; This is how to hide a file inside the current
                                ;; directory
                      (deny #P"main.lisp")
                      ;; Or inside some particular directory
                      (deny #P"pages/*.fasl")
                      ;; Also you might whitelist some directories
                      ;; and files:
                      (allow #P"**/" ;; Allow to show any directory
                             #P"**/*.lisp"
                             #P"favicons/*.png"
                             #P"favicons/*.ico")
                      ;; and then deny the rest.
                      ;; 
                      ;; We need this part because by default
                      ;; all files are allowed:
                      (deny-all)))
```
Here we've used these utils functions:

* [`allow`][0d22]
* [`deny`][f929]
* [`deny-all`][e35d]

Read more about how filtering work in the [`Filtering`][1984] section.

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40FILTERING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Filtering

`FILTER` argument of [`file-server`][7dcb] function accepts a function or a list of functions
where each function should accept a pathname relative to the `ROOT` argument given
to the [`file-server`][7dcb] function and return `:ALLOW`, `:DENY` or `NIL`.

When function returns `:ALLOW` or `:DENY`, processing is stopped and user see either
content or 404 error page. If filter function returns `NIL`, then other filter functions
are checked.

If no functions matched the current path, then reblocks file server consider it to
be allowed. So we have a black-list mode as a default - if you want something to
be denied - deny it!

In this example we deny access to a `config.lisp` file and fasl files in all directories.
All other files and directories will be allowed:

```
(list
 (deny #P"config.lisp")
 (deny #P"**/*.fasl"))
```
Here I've used [`deny`][f929] helper which accepts a template pathname and returns a function which
checks given pathname to this template pathname.

To switch to the white-list mode, you need to add a last rule which will deny access to
any file or directory. Use [`deny-all`][e35d] helper for this.

In the next example we deny access to all files and directories but allow listing of
any directory and files with `lisp` extension:

```
(list
 (allow ;; Allow to show any directory
        #P"**/"
        ;; Allow any lisp file
        #P"**/*.lisp")
 (deny-all))
```
If you need some special filtering, then you can use [`allow-if`][adfb] or [`deny-if`][2c59]
functions. This is an example how to define a filter to hide empty directories:

```
(deny-if (lambda (path)
           (and
            (cl-fad:directory-pathname-p path)
            (uiop:emptyp
             (cl-fad:list-directory
              (merge-pathnames path
                               (asdf:system-relative-pathname :my-app "./")))))))
```
<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40REBLOCKS-FILE-SERVER-2FCORE-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### REBLOCKS-FILE-SERVER/CORE

<a id="x-28-23A-28-2825-29-20BASE-CHAR-20-2E-20-22REBLOCKS-FILE-SERVER-2FCORE-22-29-20PACKAGE-29"></a>

#### [package](f28d) `reblocks-file-server/core`

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-FILE-SERVER-2FCORE-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40REBLOCKS-FILE-SERVER-2FCORE-24FILE-SERVER-ROUTE-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### FILE-SERVER-ROUTE

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-ROUTE-20CLASS-29"></a>

###### [class](e36d) `reblocks-file-server/core:file-server-route` (page-route)

**Readers**

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ADIRECTORIES-FIRST-P-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-ROUTE-29-29"></a>

###### [reader](8989) `reblocks-file-server/core:directories-first-p` (file-server-route) (:directories-first-p = t)

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AFILENAME-FILTER-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-ROUTE-29-29"></a>

###### [reader](ee72) `reblocks-file-server/core:filename-filter` (file-server-route) (:filter = (lambda (pathname) (declare (ignore pathname)) t))

A regular expression to show only selected files.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-DIR-LISTING-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-ROUTE-29-29"></a>

###### [reader](a741) `reblocks-file-server/core:get-dir-listing` (file-server-route) (:dir-listing = t)

When nil, directory contents is not shown.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AGET-ROOT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-ROUTE-29-29"></a>

###### [reader](1143) `reblocks-file-server/core:get-root` (file-server-route) (:root)

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-FILE-SERVER-2FCORE-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-20FUNCTION-29"></a>

##### [function](f52e) `reblocks-file-server/core:file-server` uri-path &key name (route-class 'file-server-route) (root (uiop/pathname:ensure-directory-pathname \*default-pathname-defaults\*)) (dir-listing t) (filter nil)

Returns a [`file-server-route`][9550] object suitable for including into Reblocks routes hierarchy.

`FILTER` argument should be a `NIL` or a list of filter functions which accept a pathname
and return `:ALLOW` `:DENY` or `NIL`.

<a id="x-28REBLOCKS-FILE-SERVER-2FCORE-3ALIST-DIRECTORY-20FUNCTION-29"></a>

##### [function](c8f2) `reblocks-file-server/core:list-directory` route-path directory-relative-path filter &key directories-first-p

Returns a list of files in the directory.
All items of the list are relative.

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40REBLOCKS-FILE-SERVER-2FUTILS-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### REBLOCKS-FILE-SERVER/UTILS

<a id="x-28-23A-28-2826-29-20BASE-CHAR-20-2E-20-22REBLOCKS-FILE-SERVER-2FUTILS-22-29-20PACKAGE-29"></a>

#### [package](c03f) `reblocks-file-server/utils`

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-FILE-SERVER-2FUTILS-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28REBLOCKS-FILE-SERVER-2FUTILS-3AALLOW-20FUNCTION-29"></a>

##### [function](718e) `reblocks-file-server/utils:allow` pathname &rest more-pathnames

Returns a function of one argument which will check this argument against given pathnames and if there is match, returns `:ALLOW`.

<a id="x-28REBLOCKS-FILE-SERVER-2FUTILS-3AALLOW-IF-20FUNCTION-29"></a>

##### [function](37e5) `reblocks-file-server/utils:allow-if` predicate

Returns a function of one argument which will check an argument against given predicate and if it returns T, returns `:ALLOW`.

<a id="x-28REBLOCKS-FILE-SERVER-2FUTILS-3ADENY-20FUNCTION-29"></a>

##### [function](0387) `reblocks-file-server/utils:deny` pathname &rest more-pathnames

Returns a function of one argument which will check this argument against given pathnames and if there is match, returns `:DENY`.

<a id="x-28REBLOCKS-FILE-SERVER-2FUTILS-3ADENY-ALL-20FUNCTION-29"></a>

##### [function](cb86) `reblocks-file-server/utils:deny-all`

Returns a function which will deny all files.

<a id="x-28REBLOCKS-FILE-SERVER-2FUTILS-3ADENY-IF-20FUNCTION-29"></a>

##### [function](3476) `reblocks-file-server/utils:deny-if` predicate

Returns a function of one argument which will check an argument against given predicate and if it returns T, returns `:DENY`.

<a id="x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-7C-40REBLOCKS-FILE-SERVER-2FUTILS-3FTypes-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Types

<a id="x-28REBLOCKS-FILE-SERVER-2FUTILS-3AFILTER-FUNCTION-20-28TYPE-29-29"></a>

##### [type](c4b0) `reblocks-file-server/utils:filter-function`

```
(FUNCTION (PATHNAME) (VALUES (MEMBER :ALLOW :DENY NIL)))
```

[f449]: https://40ants.com/reblocks-file-server/
[7dcb]: https://40ants.com/reblocks-file-server/#x-28REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-20FUNCTION-29
[9550]: https://40ants.com/reblocks-file-server/#x-28REBLOCKS-FILE-SERVER-2FCORE-3AFILE-SERVER-ROUTE-20CLASS-29
[0d22]: https://40ants.com/reblocks-file-server/#x-28REBLOCKS-FILE-SERVER-2FUTILS-3AALLOW-20FUNCTION-29
[adfb]: https://40ants.com/reblocks-file-server/#x-28REBLOCKS-FILE-SERVER-2FUTILS-3AALLOW-IF-20FUNCTION-29
[f929]: https://40ants.com/reblocks-file-server/#x-28REBLOCKS-FILE-SERVER-2FUTILS-3ADENY-20FUNCTION-29
[e35d]: https://40ants.com/reblocks-file-server/#x-28REBLOCKS-FILE-SERVER-2FUTILS-3ADENY-ALL-20FUNCTION-29
[2c59]: https://40ants.com/reblocks-file-server/#x-28REBLOCKS-FILE-SERVER-2FUTILS-3ADENY-IF-20FUNCTION-29
[1984]: https://40ants.com/reblocks-file-server/#x-28REBLOCKS-FILE-SERVER-DOCS-2FINDEX-3A-3A-40FILTERING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[b09a]: https://github.com/40ants/reblocks-file-server
[4729]: https://github.com/40ants/reblocks-file-server/actions
[f28d]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/core.lisp#L1
[c8f2]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/core.lisp#L197
[e36d]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/core.lisp#L63
[1143]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/core.lisp#L64
[a741]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/core.lisp#L67
[8989]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/core.lisp#L72
[ee72]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/core.lisp#L76
[f52e]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/core.lisp#L96
[c03f]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/utils.lisp#L1
[0387]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/utils.lisp#L110
[3476]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/utils.lisp#L125
[cb86]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/utils.lisp#L138
[c4b0]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/utils.lisp#L25
[37e5]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/utils.lisp#L82
[718e]: https://github.com/40ants/reblocks-file-server/blob/c79bb3e96ab4aeb7d3449f6113aee30a525dede6/src/utils.lisp#L95
[a450]: https://github.com/40ants/reblocks-file-server/issues
[25b9]: https://quickdocs.org/40ants-routes
[8236]: https://quickdocs.org/alexandria
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
