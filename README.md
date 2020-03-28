# Dumbparens

Dumbparens is a parenthesis-matching solution for Emacs that actually
Just Works™ (finally). It replaces and improves on previous packages
such as [Electric Pair
mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html),
[Paredit](http://danmidwood.com/content/2014/11/21/animated-paredit.html),
and [Smartparens](https://github.com/Fuco1/smartparens). The package
is called Dumbparens as a response to Smartparens, which I feel tries
to be much too smart for its own good, resulting in a host of bugs,
performance issues, and behavior that can only be described as just
plain zany (see below). Dumbparens has the design philosophy of: try
to do as little as possible, and **always** do something reasonable.

Documentation will come when the basic features have been implemented.
Usage is as follows. First install with
[`straight.el`](https://github.com/raxod502/straight.el):

    (straight-use-package
      '(dumbparens :host github :repo "raxod502/dumbparens"))

To run the tests, evaluate the contents of `dumbparens-tests.el` and
then type `M-x dumbparens-run-all-tests`. Since Dumbparens has a goal
of **always** doing something reasonable, any unreasonable behavior
will result in a slap on the wrist and a new regression test.

### What does "zany" mean?

Suppose we enable `smartparens-mode` and `delete-selection-mode`, and
we have this buffer of text:

```
Suppose we enable `smartparens-mode` and `delete-selection-mode`, and
we have this buffer of text
```

Now suppose we select the entire buffer and type the backslash key
`\`. What *should* happen is the region is deleted and replaced with a
backslash:

```
\
```

However with `smartparens-mode` enabled, the following is what we get
instead:

```
\Suppose we enable `smartparens-mode` and `delete-selection-mode`, and
we have this buffer of text\"\" \(\) \{\} \\(\\)
```

Okay, to be fair, after you perform a subsequent command in the
buffer, everything but the backslash gets deleted, but ... what???
