---
layout: book
title: Arrays
permalink: /learn/best_practices/arrays
---

When passing arrays in and out of a subroutine/function, use the
following pattern for 1D arrays (it is called *assumed-shape*):

``` fortran
subroutine f(r)
  real(dp), intent(out) :: r(:)
  integer :: n, i
  n = size(r)
  do i = 1, n
    r(i) = 1.0_dp / i**2
  end do
end subroutine
```

2D arrays:

``` fortran
subroutine g(A)
  real(dp), intent(in) :: A(:, :)
  ...
end subroutine
```

and call it like this:

``` fortran
real(dp) :: r(5)
call f(r)
```

No array copying is done above. It has the following advantages:

-   the shape and size of the array is passed in automatically
-   the shape is checked at compile time, the size optionally at runtime
-   allows to use strides and all kinds of array arithmetic without
    actually copying any data.

This should always be your default way of passing arrays in and out of
subroutines. However in the following cases one can (or has to) use *explicit-shape* arrays:

-   returning an array from a function
-   interfacing with C code or legacy Fortran (like Lapack)
-   operating on arbitrary shape array with the given function (however
    there are also other ways to do that, see `elemental` for more
    information)

To use *explicit-shape* arrays, do:

``` fortran
subroutine f(n, r)
  integer, intent(in) :: n
  real(dp), intent(out) :: r(n)
  integer :: i
  do i = 1, n
    r(i) = 1.0_dp / i**2
  end do
end subroutine
```

2D arrays:

``` fortran
subroutine g(m, n, A)
  integer, intent(in) :: m, n
  real(dp), intent(in) :: A(m, n)
  ...
end subroutine
```

and call it like this:

``` fortran
real(dp) :: r(5)
call f(size(r), r)
```

In order to return an array from a function, do:

``` fortran
function f(n) result(r)
  integer, intent(in) :: n
  real(dp) :: r(n)
  integer :: i
  do i = 1, n
    r(i) = 1.0_dp / i**2
  end do
end function
```

If you want to enforce/check the size of the arrays, put at the
beginning of the function:

``` fortran
if (size(r) /= 4) stop "Incorrect size of 'r'"
```

To initialize an array, do:

``` fortran
integer :: r(5)
r = [1, 2, 3, 4, 5]
```

This syntax is valid since the Fortran 2003 standard and it is the
preferred syntax (the old syntax `r = (/ 1, 2, 3, 4, 5 /)` should only
be used if you cannot use Fortran 2003).

In order for the array to start with different index than 1, do:

``` fortran
subroutine print_eigenvalues(kappa_min, lam)
  integer, intent(in) :: kappa_min
  real(dp), intent(in) :: lam(kappa_min:)

  integer :: kappa
  do kappa = kappa_min, ubound(lam, 1)
    print *, kappa, lam(kappa)
  end do
end subroutine
```
