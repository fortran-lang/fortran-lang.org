## selected\_real\_kind
### __Name__

__selected\_real\_kind__(3) - \[KIND\] Choose real kind


### __Syntax__
```fortran
result = selected_real_kind(p, r, radix)
```
### __Description__

__selected\_real\_kind(p, r, radix)__ return the kind value of a real
data type with decimal precision of at least __p__ digits, exponent range of
at least __r__, and with a radix of __radix__.

### __Arguments__

  - __p__
    : (Optional) shall be a scalar and of type _integer_.

  - __r__
    : (Optional) shall be a scalar and of type _integer_.

  - __radix__
    : (Optional) shall be a scalar and of type _integer_.

Before __Fortran 2008__, at least one of the arguments __r__ or __p__ shall
be present; since __Fortran 2008__, they are assumed to be zero if
absent.

### __Returns__

selected\_real\_kind returns the value of the kind type parameter of a
real data type with decimal precision of at least __p__ digits, a decimal
exponent range of at least R, and with the requested __radix__. If the __radix__
parameter is absent, real kinds with any radix can be returned. If more
than one real data type meet the criteria, the kind of the data type
with the smallest decimal precision is returned. If no real data type
matches the criteria, the result is

  - __-1__ if the processor does not support a real data type with a
    precision greater than or equal to __p__, but the __r__ and __radix__
    requirements can be fulfilled

      - __-2__ if the processor does not support a real type with an
        exponent range greater than or equal to __r__, but __p__ and __radix__ are
        fulfillable

      - __-3__ if __radix__ but not __p__ and __r__ requirements are fulfillable

      - __-4__ if __radix__ and either __p__ or __r__ requirements are fulfillable

      - __-5__ if there is no real type with the given __radix__

### __Examples__

Sample program:

```fortran
program demo_selected_real_kind
implicit none
integer,parameter :: p6 = selected_real_kind(6)
integer,parameter :: p10r100 = selected_real_kind(10,100)
integer,parameter :: r400 = selected_real_kind(r=400)
real(kind=p6) :: x
real(kind=p10r100) :: y
real(kind=r400) :: z

   print *, precision(x), range(x)
   print *, precision(y), range(y)
   print *, precision(z), range(z)
end program demo_selected_real_kind
```
  Results:
```text
              6          37
             15         307
             18        4931
```
### __Standard__

Fortran 95 and later; with RADIX - Fortran 2008 and later

### __See Also__

[__precision__(3)](PRECISION),
[__range__(3)](RANGE),
[__radix__(3)](RADIX)

####### fortran-lang intrinsic descriptions
