## shape
### __Name__

__shape__(3) - \[ARRAY INQUIRY\] Determine the shape of an array


### __Syntax__
```fortran
result = shape(source, kind)
```
### __Description__

Determines the shape of an array.

### __Arguments__

  - __source__
    : Shall be an array or scalar of any type. If __source__ is a pointer it
    must be associated and allocatable arrays must be allocated.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

### __Returns__

An _integer_ array of rank one with as many elements as __source__ has
dimensions. The elements of the resulting array correspond to the extend
of __source__ along the respective dimensions. If __source__ is a scalar, the
result is the rank one array of size zero. If __kind__ is absent, the return
value has the default integer kind otherwise the specified kind.

### __Examples__

Sample program:

```fortran
program demo_shape
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
integer, dimension(-1:1, -1:2) :: a
   print all, 'shape of array=',shape(a)
   print all, 'shape of constant=',shape(42)
   print all, 'size of shape of constant=',size(shape(42))
   print all, 'ubound of array=',ubound(a)
   print all, 'lbound of array=',lbound(a)
end program demo_shape
```
  Results:
```text
   shape of array= 3 4
   shape of constant=
   size of shape of constant= 0
   ubound of array= 1 2
   lbound of array= -1 -1
```
### __Standard__

Fortran 95 and later; with KIND argument Fortran 2003 and later

### __See Also__

[__reshape__(3)](RESHAPE),
[__size__(3)](SIZE)

####### fortran-lang intrinsic descriptions
