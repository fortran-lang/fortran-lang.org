---
layout: book
title: Floating Point Numbers
permalink: /learn/best_practices/floating_point
---

Somewhere create and export a parameter `dp`:

``` fortran
integer, parameter:: dp=kind(0.d0)                   ! double precision
```

and declare floats as:

``` fortran
real(dp) :: a, b, c
```

Always write all floating point constants with the `_dp` suffix:

``` fortran
1.0_dp, 3.5_dp, 1.34e8_dp
```

and never any other way (see also the gotcha
`floating_point_numbers_gotcha`). Omitting the dot in the literal
constant is also incorrect.

To print floating point double precision numbers without losing
precision, use the `(es23.16)` format (see
<http://stackoverflow.com/questions/6118231/why-do-i-need-17-significant-digits-and-not-16-to-represent-a-double/>).

It is safe to assign integers to floating point numbers without losing
accuracy:

``` fortran
real(dp) :: a
a = 3
```

In order to impose floating point division (as opposed to integer
division `1/2` equal to `0`), one can convert the integer to a floating
point number by:

``` fortran
real(dp) :: a
a = real(1, dp) / 2       ! 'a' is equal to 0.5_dp
```
