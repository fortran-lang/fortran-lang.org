---
layout: book
title: sngl
permalink: /learn/intrinsics/SNGL
---
### NAME

**sngl**(3f) - \[NUMERIC:TYPE\] Convert double precision real to default real
(GFDL)

### SYNTAX

result = **sngl**(a)

### DESCRIPTION

**sngl**(a) converts the double precision real A to a default real
value. This is an archaic form of REAL that is specific to one type for
A.

### ARGUMENTS

  - **A**
    The type shall be a double precision REAL.

### RETURN VALUE

The return value is of type default REAL.

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental function

### SEE ALSO

**dble**(3)
