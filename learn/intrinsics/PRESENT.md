---
layout: book
title: present
permalink: /learn/intrinsics/PRESENT
---
## __Name__

__present__(3) - \[\] Determine whether an optional dummy argument is specified
(GFDL)

## __Syntax__

result = __present__(a)

## __Description__

Determines whether an optional dummy argument is present.

## __Arguments__

  - __a__
    : May be of any type and may be a pointer, scalar or array value, or a
    dummy procedure. It shall be the name of an optional dummy argument
    accessible within the current subroutine or function.

## __Returns__

Returns either __.true.__ if the optional argument __a__ is present, or __.false.__
otherwise.

## __Examples__

Sample program:

```fortran
program demo_present
implicit none
   write(*,*) f(), f(42)      ! "f t"
contains
logical function f(x)
integer, intent(in), optional :: x
   f = present(x)
   end function
end program demo_present
```

## __Standard__

Fortran 95 and later
