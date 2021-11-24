---
layout: book
title: co_lbound
permalink: /learn/intrinsics/CO_LBOUND
---
#### NAME

__co\_lbound__(3f) - \[COLLECTIVE\] Lower codimension bounds of an array
(GFDL)

#### SYNTAX

result = __CO\_LBOUND__(coarray \[, dim \[, kind\]\])

#### DESCRIPTION

Returns the lower bounds of a coarray, or a single lower cobound along
the DIM codimension.

#### ARGUMENTS

  - __ARRAY__
    Shall be an coarray, of any type.

  - __DIM__
    (Optional) Shall be a scalar INTEGER.

  - __KIND__
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

#### RETURN VALUE

The return value is of type INTEGER and of kind KIND. If KIND is absent,
the return value is of default integer kind. If DIM is absent, the
result is an array of the lower cobounds of COARRAY. If DIM is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

#### STANDARD

Fortran 2008 and later

#### CLASS

Inquiry function

#### SEE ALSO

__co\_ubound__(3), __lbound__(3)
