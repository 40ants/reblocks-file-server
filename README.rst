======================
 reblocks-file-server
======================

.. insert-your badges like that:

.. image:: https://travis-ci.org/40ants/reblocks-file-server.svg?branch=master
    :target: https://travis-ci.org/40ants/reblocks-file-server

.. Everything starting from this commit will be inserted into the
   index page of the HTML documentation.
.. include-from

This is a little helper which is able to serve local files and show
their listing on a web-page.

Reasoning
=========

If you are working on a small project which does not require a CDN
to serve it's static files, then probably you want to use this
Reblocks extension.

It let you to define a root on a file system and provide a way to
browse it via the web.

Here is how it is looks like:

TODO: make a screen shoot of the default view.


To create a page with a file browser, add such code
somewhere in your code somewhere after the call to
the ``reblocks/server:start``:

TODO: continue a tutorial...

.. code-block:: common-lisp

   (defvar log-item '(:|@message| "Some"
                      :|@timestamp| 122434342
                      ;; this field is wrong and
                      ;; shouldn't be here
                      :|@fields| nil))

Customization
=============

TODO: Add this section...
                      
Roadmap
=======

* Provide a way how to exclude some files from the view.
* Add ability for upload files by drag&drop.
* Add some permissions model to check if user is able to upload files.

.. Everything after this comment will be omitted from HTML docs.
.. include-to


Authors
=======

* Alexander Artemenko (svetlyak.40wt@gmail.com)

Copyright
=========

Copyright (c) 2018 Alexander Artemenko (svetlyak.40wt@gmail.com)
