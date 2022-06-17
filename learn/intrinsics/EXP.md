---
layout: book
title: exp
permalink: /learn/intrinsics/EXP
---
## __Name__

__exp__(3) - \[MATHEMATICS\] Exponential function

## __Syntax__
```fortran
result = exp(x)
```
## __Description__

__exp__(x) computes the base "_e_" exponential of __x__ where "_e_" is
_Euler's constant_.

If __x__ is of type _complex_, its imaginary part is regarded as a value
in radians such that (see _Euler's formula_):

if 
     __cx=(re,im)__ 
then 
     __exp(cx)=exp(re)*cmplx(cos(im),sin(im))__

Since __exp__(3) is the inverse function of __log__(3) the maximum valid magnitude
of the _real_ component of __x__ is __log(huge(x))__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The value of the result is __e\*\*x__ where __e__ is Euler's constant.

The return value has the same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_exp
implicit none
real :: x , re, im
complex :: cx

   x = 1.0
   write(*,*)"Euler's constant is approximately",exp(x)

   !! complex values
   ! given
   re=3.0
   im=4.0
   cx=cmplx(re,im)

   ! complex results from complex arguments are Related to Euler's formula
   write(*,*)'given the complex value ',cx
   write(*,*)'exp(x) is',exp(cx)
   write(*,*)'is the same as',exp(re)*cmplx(cos(im),sin(im),kind=kind(cx))

   ! exp(3) is the inverse function of log(3) so
   ! the real compoenent of the input must be less than or equal to 
   write(*,*)'maximum real component',log(huge(0.0)) 
   ! or for double precision
   write(*,*)'maximum doubleprecision component',log(huge(0.0d0)) 

   ! but since the imaginary component is passed to the cos(3) and sin(3)
   ! functions the imaginary component can be any real value

end program demo_exp
```
Results:
```text
 Euler's constant is approximately   2.718282    
 given the complex value  (3.000000,4.000000)
 exp(x) is (-13.12878,-15.20078)
 is the same as (-13.12878,-15.20078)
 maximum real component   88.72284    
 maximum doubleprecision component   709.782712893384     
```
## __Standard__

FORTRAN 77 and later

## __See Also__

* [__log__(3)](LOG)

* Wikipedia:[Exponential function](https://en.wikipedia.org/wiki/Exponential_function)

* Wikipedia:[Euler's formula](https://en.wikipedia.org/wiki/Euler%27s_formula)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
