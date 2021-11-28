---
layout: book
title: huge
permalink: /learn/intrinsics/HUGE
---
## __Name__

__huge__(3) - \[NUMERIC MODEL\] Largest number of a kind
(GFDL)

## __Syntax__

result = __huge__(x)

## __Description__

__huge__(x) returns the largest number that is not an infinity in the
model of the type of X.

## __Arguments__

  - __X__
    Shall be of type _real_ or _integer_.

## __Returns__

The return value is of the same type and kind as X

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
