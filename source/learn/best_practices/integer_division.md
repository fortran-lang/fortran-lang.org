---
layout: book
title: Integer Division
permalink: /learn/best_practices/integer_division
sd_hide_title: true
---

# Integer Division

Fortran distinguishes between floating point and integer arithmetic.  It is
important to note that division for integers is always using integer
arithmetic. Furthermore, while Fortran uses the standard order-of-operations
(e.g. multiplication and division preceed addition and subtraction, in the
absence of parenthesis), operations of the same precedence are evaluated from
left to right.  Consider the following example for integer division of an odd
number:

```fortran
integer :: n
n = 3
print *, n / 2  ! prints 1
print *, n*(n + 1)/2  ! prints 6
print *, n/2*(n + 1)  ! prints 4 (left-to-right evaluation order)
n = -3
print *, n / 2  ! prints -1
```

Be careful about whether you want to actually use integer arithmetic
in this context. If you want to use floating point arithmetic instead
make sure to cast to reals before using the division operator, or separate
the integers by multiplying by `1.0_dp`:

```fortran
integer :: n
n = 3
print *, real(n, dp) / 2  ! prints 1.5
print *, n * 1.0_dp / 2  ! prints 1.5
n = -3
print *, real(n, dp) / 2  ! prints -1.5
print *, n * 1.0_dp / 2  ! prints -1.5
```
