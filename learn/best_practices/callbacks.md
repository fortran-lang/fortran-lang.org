---
layout: book
title: Callbacks
permalink: /learn/best_practices/callbacks
---

There are two ways to implement callbacks to be used like this:

``` fortran
subroutine foo(a, k)
use integrals, only: simpson
real(dp) :: a, k
print *, simpson(f, 0._dp, pi)
print *, simpson(f, 0._dp, 2*pi)

contains

real(dp) function f(x) result(y)
real(dp), intent(in) :: x
y = a*sin(k*x)
end function f

end subroutine foo
```

The traditional approach is to simply declare the `f` dummy variable as
a subroutine/function using:

``` fortran
module integrals
use types, only: dp
implicit none
private
public simpson

contains

real(dp) function simpson(f, a, b) result(s)
real(dp), intent(in) :: a, b
interface
    real(dp) function f(x)
    use types, only: dp
    implicit none
    real(dp), intent(in) :: x
    end function
end interface
s = (b-a) / 6 * (f(a) + 4*f((a+b)/2) + f(b))
end function

end module
```

The other approach since f2003 is to first define a new type for our
callback, and then use `procedure(func)` as the type of the dummy
argument:

``` fortran
module integrals
use types, only: dp
implicit none
private
public simpson

contains

real(dp) function simpson(f, a, b) result(s)
real(dp), intent(in) :: a, b
interface
    real(dp) function func(x)
    use types, only: dp
    implicit none
    real(dp), intent(in) :: x
    end function
end interface
procedure(func) :: f
s = (b-a) / 6 * (f(a) + 4*f((a+b)/2) + f(b))
end function

end module
```

The new type can also be defined outside of the function (and reused),
like:

``` fortran
module integrals
use types, only: dp
implicit none
private
public simpson

interface
    real(dp) function func(x)
    use types, only: dp
    implicit none
    real(dp), intent(in) :: x
    end function
end interface

contains

real(dp) function simpson(f, a, b) result(s)
real(dp), intent(in) :: a, b
procedure(func) :: f
s = (b-a) / 6 * (f(a) + 4*f((a+b)/2) + f(b))
end function

real(dp) function simpson2(f, a, b) result(s)
real(dp), intent(in) :: a, b
procedure(func) :: f
real(dp) :: mid
mid = (a + b)/2
s = simpson(f, a, mid) + simpson(f, mid, b)
end function

end module
```
