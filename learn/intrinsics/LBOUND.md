---
layout: book
title: lbound
permalink: /learn/intrinsics/LBOUND
---
## __Name__

__lbound__(3) - \[ARRAY INQUIRY\] Lower dimension bounds of an array
(GFDL)

## __Syntax__

result = __lbound__(array \[, dim \[, kind\]\])

## __Description__

Returns the lower bounds of an array, or a single lower bound along the
DIM dimension.

## __Arguments__

  - __ARRAY__
    Shall be an array, of any type.

  - __DIM__
    (Optional) Shall be a scalar _integer_.

  - __KIND__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind KIND. If KIND is absent,
the return value is of default integer kind. If DIM is absent, the
result is an array of the lower bounds of ARRAY. If DIM is present, the
result is a scalar corresponding to the lower bound of the array along
that dimension. If ARRAY is an expression rather than a whole array or
array structure component, or if it has a zero extent along the relevant
dimension, the lower bound is taken to be 1.

## __Examples__

Note that in my opinion this function should not be used on assumed-size
arrays or in any function without an explicit interface. Errors can
occur if there is no interface defined.

Sample program

```
   ! program demo_lbound
   module m_bounds
   implicit none
    contains
       subroutine msub(arr)
          !!integer,intent(in) :: arr(*)  ! cannot be assumed-size array
          integer,intent(in) :: arr(:)
          write(*,*)'MSUB: LOWER=',lbound(arr), &
          & 'UPPER=',ubound(arr), &
          & 'SIZE=',size(arr)
       end subroutine msub
    end module m_bounds

    use m_bounds, only : msub
    implicit none
    interface
       subroutine esub(arr)
       integer,intent(in) :: arr(:)
       end subroutine esub
    end interface
    integer :: arr(-10:10)
       write(*,*)'MAIN: LOWER=',lbound(arr), &
       & 'UPPER=',ubound(arr), &
       & 'SIZE=',size(arr)
       call csub()
       call msub(arr)
       call esub(arr)
    contains
       subroutine csub
          write(*,*)'CSUB: LOWER=',lbound(arr), &
          & 'UPPER=',ubound(arr), &
          & 'SIZE=',size(arr)
       end subroutine csub
    end

    subroutine esub(arr)
    implicit none
    integer,intent(in) :: arr(:)
       ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE
       ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)
       write(*,*)'ESUB: LOWER=',lbound(arr), &
       & 'UPPER=',ubound(arr), &
       & 'SIZE=',size(arr)
    end subroutine esub
   !end program demo_lbound
```

> Expected output
>
> ```
>    MAIN: LOWER=         -10 UPPER=          10 SIZE=          21
>    CSUB: LOWER=         -10 UPPER=          10 SIZE=          21
>    MSUB: LOWER=           1 UPPER=          21 SIZE=          21
>    ESUB: LOWER=           1 UPPER=          21 SIZE=          21
> ```

## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003 and later

## __See Also__

__ubound__(3), __co\_lbound__(3)
