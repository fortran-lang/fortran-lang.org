## erf
### __Name__

__erf__(3) - \[MATHEMATICS\] Error function


### __Syntax__
```fortran
result = erf(x)
```
### __Description__

__erf__(x) computes the error function of __x__, defined as 
$$
\text{erf}(x) = \frac{2}{\sqrt{\pi}} \int_0^x e^{__-t__^2} dt.
$$

### __Arguments__

  - __x__
    : The type shall be _real_.

### __Returns__

The return value is of type _real_, of the same kind as __x__ and lies in the
range __-1__ \<= __erf__(x) \<= 1 .

### __Examples__

Sample program:

```fortran
program demo_erf
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 0.17_real64
    write(*,*)x, erf(x)
end program demo_erf
```
  Results:
```text
     0.17000000000000001       0.18999246120180879     
```
### __Standard__

Fortran 2008 and later

### See also

[__erfc__(3)](ERFC)

- [Wikipedia:error function](https://en.wikipedia.org/wiki/Error_function)

####### fortran-lang intrinsic descriptions
