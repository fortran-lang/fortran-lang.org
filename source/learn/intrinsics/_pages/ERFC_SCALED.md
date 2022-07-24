## erfc\_scaled
### __Name__

__erfc\_scaled__(3) - \[MATHEMATICS\] Error function


### __Syntax__
```fortran
result = erfc_scaled(x)
```
### __Description__

__erfc\_scaled__(x) computes the exponentially-scaled complementary
error function of __x__:

$$
e^{x^2} \frac{2}{\sqrt{\pi}} \int_{x}^{\infty}
e^{-t^2} dt.
$$

### __Arguments__

  - __x__
    : The type shall be _real_.

### __Returns__

The return value is of type _real_ and of the same kind as __x__.

### __Examples__

Sample program:

```fortran
program demo_erfc_scaled
implicit none
real(kind(0.0d0)) :: x = 0.17d0
   x = erfc_scaled(x)
   print *, x 
end program demo_erfc_scaled
```
  Results:
```text
     0.83375830214998126     
```

### __Standard__

Fortran 2008 and later

####### fortran-lang intrinsic descriptions
