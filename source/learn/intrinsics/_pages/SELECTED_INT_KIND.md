## selected\_int\_kind
### __Name__

__selected\_int\_kind__(3) - \[KIND\] Choose integer kind


### __Syntax__
```fortran
result = selected_int_kind(r)
```
### __Description__

__selected\_int\_kind(r)__ return the kind value of the smallest integer
type that can represent all values ranging from __-10\*\*r__ (exclusive)
to __10\*\*r__ (exclusive). If there is no integer kind that accommodates
this range, selected\_int\_kind returns __-1__.

### __Arguments__

  - __r__
    : Shall be a scalar and of type _integer_.

### __Examples__

Sample program:

```fortran
program demo_selected_int_kind
implicit none
integer,parameter :: k5 = selected_int_kind(5)
integer,parameter :: k15 = selected_int_kind(15)
integer(kind=k5) :: i5
integer(kind=k15) :: i15

    print *, huge(i5), huge(i15)

    ! the following inequalities are always true
    print *, huge(i5) >= 10_k5**5-1
    print *, huge(i15) >= 10_k15**15-1
end program demo_selected_int_kind
```
  Results:
```text
     2147483647  9223372036854775807
    T
    T
```
### __Standard__

Fortran 95 and later

### __See Also__

[__aint__(3)](AINT),
[__anint__(3)](ANINT),
[__int__(3)](INT),
[__nint__(3)](NINT),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

####### fortran-lang intrinsic descriptions
