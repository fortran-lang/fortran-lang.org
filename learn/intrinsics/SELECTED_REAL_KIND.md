---
layout: book
title: selected_real_kind
permalink: /learn/intrinsics/SELECTED_REAL_KIND
---
### NAME

**selected\_real\_kind**(3f) - \[KIND\] Choose real kind
(GFDL)

### SYNTAX

result = **selected\_real\_kind**(\[p, r, radix\])

### DESCRIPTION

**selected\_real\_kind**(p, r, radix) return the kind value of a real
data type with decimal precision of at least P digits, exponent range of
at least R, and with a radix of RADIX.

### ARGUMENTS

  - **P**
    (Optional) shall be a scalar and of type INTEGER.

  - **R**
    (Optional) shall be a scalar and of type INTEGER.

  - **RADIX**
    (Optional) shall be a scalar and of type INTEGER.

Before \[\[Fortran 2008\]\], at least one of the arguments R or P shall
be present; since \[\[Fortran 2008\]\], they are assumed to be zero if
absent.

### RETURN VALUE

selected\_real\_kind returns the value of the kind type parameter of a
real data type with decimal precision of at least P digits, a decimal
exponent range of at least R, and with the requested RADIX. If the RADIX
parameter is absent, real kinds with any radix can be returned. If more
than one real data type meet the criteria, the kind of the data type
with the smallest decimal precision is returned. If no real data type
matches the criteria, the result is

  - **-1** if the processor does not support a real data type with a
    precision greater than or equal to P, but the R and RADIX
    requirements can be fulfilled

      - **-2** if the processor does not support a real type with an
        exponent range greater than or equal to R, but P and RADIX are
        fulfillable

      - **-3** if RADIX but not P and R requirements are fulfillable

      - **-4** if RADIX and either P or R requirements are fulfillable

      - **-5** if there is no real type with the given RADIX

### EXAMPLE

Sample program:

```
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

### STANDARD

Fortran 95 and later; with RADIX - Fortran 2008 and later

### CLASS

Transformational function

### SEE ALSO

**precision**(3), **range**(3), **radix**(3)
