## dble
### __Name__

__dble__(3) - \[TYPE:NUMERIC\] Double conversion function


### __Syntax__
```fortran
result = dble(a)

    elemental function dble(a)
    type(real(kind=kind(0.0d0)))     :: dble
    type(TYPE(kind=KIND)),intent(in) :: a
```
where TYPE may be _integer_, _real_, or _complex_ and KIND any kind
supported by the TYPE.
### __Description__

__dble(a)__ Converts __a__ to double precision _real_ type.

### __Arguments__

  - __a__
    : The type shall be _integer_, _real_, or _complex_.

### __Returns__

The return value is of type _doubleprecision_. For _complex_ input,
the returned value has the magnitude and sign of the real component
of the input value.

### __Examples__

Sample program:

```fortran
program demo_dble
implicit none
real:: x = 2.18
integer :: i = 5
complex :: z = (2.3,1.14)
   print *, dble(x), dble(i), dble(z)
end program demo_dble
```
  Results:
```text
  2.1800000667572021  5.0000000000000000   2.2999999523162842     
```
### __Standard__

FORTRAN 77 and later

### __See Also__

[__float__(3)](FLOAT),
[__real__(3)](REAL)

####### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
