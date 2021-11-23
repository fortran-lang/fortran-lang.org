---
layout: book
title: co_ubound
permalink: /learn/intrinsics/CO_UBOUND
---
### NAME

**co\_ubound**(3f) - \[COLLECTIVE\] Upper codimension bounds of an array
(GFDL)

### SYNTAX

result = **CO\_UBOUND**(coarray \[, dim \[, kind\]\])

### DESCRIPTION

Returns the upper cobounds of a coarray, or a single upper cobound along
the DIM codimension.

### ARGUMENTS

  - **ARRAY**
    Shall be an coarray, of any type.

  - **DIM**
    (Optional) Shall be a scalar INTEGER.

  - **KIND**
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type INTEGER and of kind KIND. If KIND is absent,
the return value is of default integer kind. If DIM is absent, the
result is an array of the lower cobounds of COARRAY. If DIM is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

### STANDARD

Fortran 2008 and later

### CLASS

Inquiry function

### SEE ALSO

**co\_lbound**(3), **lbound**(3), **ubound**(3)
