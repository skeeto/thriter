# Emacs Thread-based Generator Library

This is a proof-of-concept generator library built on top of Emacs 26
threads. **Do not actually use this library.** It's about five times
slower than the built-in `iter` package, and is prone to deadlocks.
Emacs threads are still in a fairly primitive state.

```el
;; Define a generator:
(thriter-lambda arglist &rest body)
(thriter-defun name arglist &rest body)

;; Pass values back and forth:
(thriter-next iter &optional yield-result)
(thriter-yield value)
(thriter-yield-from iter)

;; Terminate incompleted iterator thread:
(thriter-close iter)

;; Helper function:
(thriter-do var-and-iter &rest body)
```

The API is nearly the same as the `iter` package except that a signal
is not used to indicate iterator completion. Instead, `thriter-next`
always returns a cons whose first element indicates completion and
whose second element is the yield result. This is more similar
JavaScript's generators than Python's generators.

Iterator objects returned by generator functoins are opaque and you
shouldn't rely on any part of its internals.

Similar to with `iter`, iterators that have not been run to completion
will hold onto expensive resources, in this case a thread. These will
eventually be cleaned up by the garbage collector, but it's better not
to rely on this. It is not necessary to close iterators that have run
to completion, though doing so is harmless.

An advantage of this library over the built-in `iter` package is that
it's perfectly legal to yield inside an `unwind-protect`. It's also
perfectly legal to yield within functions called by the generator
function. The iterator state is effectively dynamically bound.
