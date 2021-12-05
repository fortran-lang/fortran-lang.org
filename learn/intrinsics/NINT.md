---
layout: book
title: nint
permalink: /learn/intrinsics/NINT
---
## __Name__

__nint__(3) - \[NUMERIC:TYPE\] Nearest whole number

## __Syntax__


```fortran
    elemental function nint(x [, kind=NN]) result(ANSWER)
     real(kind=??),intent(in) :: X
     integer(kind=NN) :: ANSWER
```

## __Description__

__NINT__(X) rounds its argument to the nearest whole number with its
sign preserved.

The user must ensure the value is a valid value for the range of the
KIND returned. If the processor cannot represent the result in the kind
specified, the result is undefined.

If X is greater than zero, __NINT__(X) has the value __INT__(X+0.5).

If X is less than or equal to zero, __NINT__(X) has the value
__INT__(a-0.5).

## __Arguments__

  - __X__
    : The type of the argument shall be _real_.

  - __KIND__
    : (Optional) A constant _integer_ expression indicating the kind
    parameter of the result. Otherwise, the kind type parameter is that
    of default _integer_ type.

## __Returns__

  - __ANSWER__
    : The result is the integer nearest X, or if there are two integers
    equally near X, the result is whichever such integer has the greater
    magnitude.

## __Examples__

Sample program:

```fortran
program demo_nint
implicit none
integer,parameter :: dp=kind(0.0d0)
real              :: x4 = 1.234E0
real(kind=dp)     :: x8 = 4.721_dp

! basic use
   print *, nint(x4), nint(x8),nint(-x8)

! issues
ISSUES: block
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
integer :: icheck
   ! make sure input is in range for the type returned
   write(*,*)'Range limits for typical KINDS:'
   write(*,'(1x,g0,1x,g0)')  &
   & int8,huge(0_int8),   &
   & int16,huge(0_int16), &
   & int32,huge(0_int32), &
   & int64,huge(0_int64)

   ! the standard does not require this to be an error ```
   x8=12345.67e15 ! too big of a number
   icheck=selected_int_kind(ceiling(log10(x8)))
   write(*,*)'Any KIND big enough? ICHECK=',icheck
   print *, 'These are all wrong answers for ',x8
   print *, nint(x8,kind=int8)
   print *, nint(x8,kind=int16)
   print *, nint(x8,kind=int32)
   print *, nint(x8,kind=int64)
endblock ISSUES

end program demo_nint
```

Results

```
   1 5 -5
   Range limits for typical KINDS:
   1 127
   2 32767
   4 2147483647
   8 9223372036854775807
   Any KIND big enough? ICHECK=          -1
   These are all wrong answers for   1.234566949990144E+019
   0
   0
   -2147483648
   -9223372036854775808
```

## __Standard__

FORTRAN 77 and later, with KIND argument - Fortran 90 and later

## __See Also__

[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

###### fortran-lang intrinsic descriptions (@urbanjost)
