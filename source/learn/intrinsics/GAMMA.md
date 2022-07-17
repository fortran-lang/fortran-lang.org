---
layout: book
title: gamma
permalink: /learn/intrinsics/GAMMA
---
# GAMMA
## __Name__

__gamma__(3) - \[MATHEMATICS\] Gamma function, which yields factorials for positive whole numbers

## __Syntax__
```fortran
x = gamma(x)
```
## __Description__

__gamma(x)__ computes Gamma of __x__. For positive whole number values of __n__ the
Gamma function can be used to calculate factorials, as  __(n-1)!  == gamma(real(n))__.
That is 
```text
n! == gamma(real(n+1))
```

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
use, intrinsic :: iso_fortran_env, only : wp=>real64
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
   ! more factorials
   FAC: block
   integer,parameter :: n(*)=[0,1,5,11,170]
   integer :: j
      do j=1,size(n)
         write(*,'(*(g0,1x))')'factorial of', n(j),' is ', &
          & product([(real(i,kind=wp),i=1,n(j))]),  &
          & gamma(real(n(j)+1,kind=wp))
      enddo
   endblock FAC

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
  Results:
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
   factorial of 0  is  1.000000000000000 1.000000000000000
   factorial of 1  is  1.000000000000000 1.000000000000000
   factorial of 5  is  120.0000000000000 120.0000000000000
   factorial of 11  is  39916800.00000000 39916800.00000000
   factorial of 170  is  .7257415615307994E+307 .7257415615307999E+307
```

## __Standard__

Fortran 2008 and later

## __See Also__

Logarithm of the Gamma function: [__log\_gamma__(3)](LOG_GAMMA)

[Wikipedia: Gamma_function](https://en.wikipedia.org/wiki/Gamma_function)

###### fortran-lang intrinsic descriptions
