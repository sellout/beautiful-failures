# Beautiful Failures

A Haskell library that helps you keep your users as happy as possible when
things go wrong.

## Overview

This library intends to make it as easy as possible for you to preserve failure
context, render it to a display-agnostic form, and finally serialize it
beautifully to a variety of outputs -- a terminal, a window in your GUI
application, or even a LaTeX document.

## failure conversions

There are two standard ways to handle failures -- one is treating the failure as
a value, and returning it in a disjunction, like `Either`. The other is
exceptions, which don't appear in the type and have non-local effects. In
general, we prefer the `Either` approach, but sometimes we don't really have a
choice.

`throwLeft` and `throwIOLeft` convert the disjunction into an exception. This
should only be used when necessary -- e.g., your function has external
constraints that make it impossible for an `Either` to cross the boundary.

## ad-hoc exception creation

If a failure type was created by an external library, it may not provide an
`Exception` instance, which means you can't throw it. We provide types both for
turning "regular" types into `Exception`s and for adding `CallStack`s to
`Exception`s.
