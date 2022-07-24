## mod
### __Name__

__mod__(3) - \[NUMERIC\] Remainder function


### __Syntax__
```fortran
result = mod(a, p)
```
### __Description__

__mod__(a,p) computes the remainder of the division of __a__ by __p__.

### __Arguments__

  - __a__
    : Shall be a scalar of type _integer_ or _real_.

  - __p__
    : Shall be a scalar of the same type and kind as __a__ and not equal to
    zero.

### __Returns__

The return value is the result of __a - (int(a/p) \* p)__. The type and kind
of the return value is the same as that of the arguments. The returned
value has the same sign as __a__ and a magnitude less than the magnitude of
__p__.

### __Examples__

Sample program:

```fortran
program demo_mod
implicit none
     print *, mod(17,3)           ! yields 2
     print *, mod(17.5,5.5)       ! yields 1.0
     print *, mod(17.5d0,5.5d0)   ! yields 1.0d0
     print *, mod(17.5d0,5.5d0)   ! yields 1.0d0

     print *, mod(-17,3)          ! yields -2
     print *, mod(-17.5,5.5)      ! yields -1.0
     print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0
     print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0

     print *, mod(17,-3)          ! yields 2
     print *, mod(17.5,-5.5)      ! yields 1.0
     print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0
     print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0
end program demo_mod
```
  Results:
```text
              2
      1.00000000    
      1.0000000000000000     
      1.0000000000000000     
             -2
     -1.00000000    
     -1.0000000000000000     
     -1.0000000000000000     
              2
      1.00000000    
      1.0000000000000000     
      1.0000000000000000     
```
### __Standard__

FORTRAN 77 and later

### __See Also__

[__modulo__(3)](MODULO)

####### fortran-lang intrinsic descriptions
