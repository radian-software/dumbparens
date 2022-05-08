# Dumbparens

Dumbparens is a parenthesis-matching solution for Emacs that actually
Just Works™ (finally). It replaces and improves on previous packages
such as [Electric Pair
mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html),
[Paredit](http://danmidwood.com/content/2014/11/21/animated-paredit.html),
and [Smartparens](https://github.com/Fuco1/smartparens). The package
is called Dumbparens as a response to Smartparens, which I feel tries
to be much too smart for its own good, resulting in a host of bugs,
performance issues, and inexplicable behavior. Dumbparens has the
design philosophy of: try to do as little as possible, and **always**
do something reasonable.

Documentation will come when the basic features have been implemented.
Usage is as follows. First install with
[`straight.el`](https://github.com/raxod502/straight.el):

    (straight-use-package
      '(dumbparens :host github :repo "radian-software/dumbparens"))

Then type `M-x dumbparens-mode` or `M-x dumbparens-global-mode`.

To run the tests, you have two options:

1. From Emacs, evaluate the contents of `dumbparens-tests.el` and then
   type `M-x dumbparens-run-all-tests`.
2. From the command line, run `make test`.

Since Dumbparens has a goal of **always** doing something reasonable,
any unreasonable behavior will result in a slap on the wrist and a new
regression test.

Note that there are also various linters, which you can run along with
the unit tests by means of `make lint`. GitHub Actions will
automatically run this against all supported Emacs versions. You can
easily test for yourself by using `make docker VERSION=25.1` or
analogous to use the appropriate version.
