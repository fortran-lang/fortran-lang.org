---
layout: book
title: sngl
permalink: /learn/intrinsics/SNGL
---
### NAME

__sngl__(3f) - \[NUMERIC:TYPE\] Convert double precision real to default real
(GFDL)

### SYNTAX

result = __sngl__(a)

### DESCRIPTION

__sngl__(a) converts the double precision real A to a default real
value. This is an archaic form of REAL that is specific to one type for
A.

### ARGUMENTS

  - __A__
    The type shall be a double precision REAL.

### RETURN VALUE

The return value is of type default REAL.

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental function

### SEE ALSO

__dble__(3)
