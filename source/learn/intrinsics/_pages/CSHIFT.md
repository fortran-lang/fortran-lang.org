## cshift
### __Name__

__cshift__(3) - \[TRANSFORMATIONAL\] Circular shift elements of an array


### __Syntax__
```fortran
result = cshift(array, shift, dim)
```
### __Description__

__cshift(array, shift \[, dim\])__ performs a circular shift on elements
of __array__ along the dimension of __dim__. If __dim__ is omitted it is taken to be
__1__. __dim__ is a scalar of type _integer_ in the range of __1 \<= dim \<= n__,
where "n" is the rank of __array__. If the rank of __array__ is one, then all
elements of __array__ are shifted by __shift__ places. If rank is greater than
one, then all complete rank one sections of __array__ along the given
dimension are shifted. Elements shifted out one end of each rank one
section are shifted back in the other end.

### __Arguments__

  - __array__
    : Shall be an array of any type.

  - __shift__
    : The type shall be _integer_.

  - __dim__
    : The type shall be _integer_.

### __Returns__

Returns an array of same type and rank as the __array__ argument.

### __Examples__

Sample program:

```fortran
program demo_cshift
implicit none
integer, dimension(3,3) :: a
    a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
    a = cshift(a, SHIFT=[1, 2, -1], DIM=2)
    print *
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
end program demo_cshift
```
  Results:
```text
     1  4  7
     2  5  8
     3  6  9
    
     4  7  1
     8  2  5
     9  3  6
```
### __Standard__

Fortran 95 and later

####### fortran-lang intrinsic descriptions
