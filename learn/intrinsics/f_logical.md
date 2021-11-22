---
layout: book
title: logical
permalink: /learn/intrinsics/f_logical
---
### NAME

**logical**(3f) - \[BIT MANIPULATION\] Converts one
kind of LOGICAL variable to another

### SYNTAX

result = **logical**(l \[, kind\])

### DESCRIPTION

Converts one kind of LOGICAL variable to another.

### ARGUMENTS

  - **L**
    The type shall be LOGICAL.

  - **KIND**
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is a LOGICAL value equal to L, with a kind
corresponding to KIND, or of the default logical kind if KIND is not
given.

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure|Elemental function

### SEE ALSO

**int**(3), **real**(3), **cmplx**(3)
