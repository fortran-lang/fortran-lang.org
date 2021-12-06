---
layout: book
title: gamma
permalink: /learn/intrinsics/GAMMA
---
## __Name__

__gamma__(3) - \[MATHEMATICS\] Gamma function
## __Syntax__
```fortran
x = gamma(x)
```


## __Description__

__gamma(x)__ computes Gamma of __x__. For positive, whole number values of __x__ the
Gamma function simplifies to the factorial function.

__Gamma(x)=(x-1)\!__.

$$ \\__Gamma__(x) = \\int\_0\*\*\\infty
t\*\*{x-1}{\\mathrm{e}}\*\*{__-t__}\\,{\\mathrm{d}}t $$

## __Arguments__

  - __x__
    : Shall be of type _real_ and neither zero nor a negative integer.

## __Returns__

The return value is of type _real_ of the same kind as _x_.

## __Examples__

Sample program:

```fortran
program demo_gamma
implicit none
real :: x, xa(4)
integer :: i

   x = gamma(1.0) 
   write(*,*)'gamma(1.0)=',x

   ! elemental
   xa=gamma([1.0,2.0,3.0,4.0])
   write(*,*)xa
   write(*,*)

   ! gamma(3) is related to the factorial function
   do i=1,20
      ! check value is not too big for default integer type
      if(factorial(i).gt.huge(0))then
         write(*,*)i,factorial(i)
      else
         write(*,*)i,factorial(i),int(factorial(i))
      endif
   enddo

contains

function factorial(i) result(f)
integer,parameter :: dp=kind(0d0)
integer,intent(in) :: i
real :: f
   if(i.le.0)then
      write(*,'(*(g0))')'<ERROR> gamma(3) function value ',i,' <= 0'
      stop      '<STOP> bad value in gamma function'
   endif
   f=gamma(real(i+1))
end function factorial

end program demo_gamma
```
Results
```text
 gamma(1.0)=   1.000000    
   1.000000       1.000000       2.000000       6.000000    
 
           1   1.000000               1
           2   2.000000               2
           3   6.000000               6
           4   24.00000              24
           5   120.0000             120
           6   720.0000             720
           7   5040.000            5040
           8   40320.00           40320
           9   362880.0          362880
          10   3628800.         3628800
          11  3.9916800E+07    39916800
          12  4.7900160E+08   479001600
          13  6.2270208E+09
          14  8.7178289E+10
          15  1.3076744E+12
          16  2.0922791E+13
          17  3.5568741E+14
          18  6.4023735E+15
          19  1.2164510E+17
          20  2.4329020E+18
```

## __Standard__

Fortran 2008 and later

## __See Also__

Logarithm of the Gamma function: [__log\_gamma__(3)](LOG_GAMMA)

[Wikipedia: Gamma_function](https://en.wikipedia.org/wiki/Gamma_function)
