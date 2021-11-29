---
layout: book
title: huge
permalink: /learn/intrinsics/HUGE
---
## __Name__

__huge__(3) - \[NUMERIC MODEL\] Largest number of a type and kind

## __Syntax__
```fortran
result = huge(x)

   TYPE(kind=KIND) function huge(x) result(answer)
   TYPE(kind=KIND) :: x
   TYPE(kind=KIND) :: answer
```
   where TYPE may be _real_ or _integer_ and KIND is any supported
   associated _kind_.

## __Description__

__huge__(x) returns the largest number that is not an infinity for the
kind and type of __x__.

## __Arguments__

  - __x__
    Shall be an arbitrary value of type _real_ or _integer_.
    The value is used merely to determine what _kind_ and _type_ of
    scalar is being queried.

## __Returns__

The return value is of the same type and kind as _x_ and is the
largest value supported by the specified model.

## __Examples__

Sample program:

```fortran
program demo_huge
! or, "why I have my own NINT function"
implicit none
character(len=*),parameter :: f='(i2,1x,2(i11,1x),f14.0:,1x,l1,1x,a)'
integer :: i,j,k,biggest
real :: v, w
   ! basic
   print *, huge(0), huge(0.0), huge(0.0d0)
   print *, tiny(0.0), tiny(0.0d0)

   ! advanced
   biggest=huge(0)
   ! be careful when using integers in computation
   do i=1,14
      j=6**i   ! Danger, Danger
      w=6**i   ! Danger, Danger
      v=6.0**i
      k=v      ! Danger, Danger
      if(v.gt.biggest)then
         write(*,f) i, j, k, v, v.eq.w, 'wrong j and k and w'
      else
         write(*,f) i, j, k, v, v.eq.w
      endif
   enddo
end program demo_huge
```
Sample output:
```
  2147483647  3.4028235E+38  1.797693134862316E+308
  1.1754944E-38  2.225073858507201E-308

    1      6           6             6. T

    2      36          36            36. T

    3      216         216           216. T

    4      1296        1296          1296. T

    5      7776        7776          7776. T

    6      46656       46656         46656. T

    7      279936      279936        279936. T

    8      1679616     1679616       1679616. T

    9      10077696    10077696      10077696. T

    10     60466176    60466176      60466176. T

    11     362797056   362797056     362797056. T

    12 -2118184960 -2147483648
           2176782336. F wrong for j and k and w

    13     175792128 -2147483648   13060694016. F wrong for j and k
           and w

    14     1054752768 -2147483648   78364164096. F wrong for j and k and w
```
## __Standard__

Fortran 95 and later
## __See Also__

[digits(3)](DIGITS),
[epsilon(3)](EPSILON),
[exponent(3)](EXPONENT),
[fraction(3)](FRACTION),
[maxexponent(3)](MAXEXPONENT),
[minexponent(3)](MINEXPONENT),
[nearest(3)](NEAREST),
[precision(3)](PRECISION),
[radix(3)](RADIX),
[range(3)](RANGE),
[rrspacing(3)](RRSPACING),
[scale(3)](SCALE),
[set_exponent(3)](SET_EXPONENT),
[spacing(3)](SPACING),
[tiny(3)](TINY)

###### fortran-lang intrinsic descriptions (@urbanjost)

