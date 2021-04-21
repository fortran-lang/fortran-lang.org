---
layout: book
title: Integer Division
permalink: /learn/best_practices/integer_division
---

Just like in Python 2.x or C, when doing things like `(N-1)/N` where `N`
is an integer and you want a floating point division, force the compiler
to use floats at the right hand side, for example by:

``` fortran
(N - 1.0_dp)/N
```

As long as one of the `/` operands is a float, a floating point division
will be used.
