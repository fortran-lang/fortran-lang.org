---
layout: book
title: logical
permalink: /learn/intrinsics/LOGICAL
---
#### NAME

__logical__(3f) - \[BIT MANIPULATION\] Converts one kind of LOGICAL variable to another
(GFDL)

#### SYNTAX

result = __logical__(l \[, kind\])

#### DESCRIPTION

Converts one kind of LOGICAL variable to another.

#### ARGUMENTS

  - __L__
    The type shall be LOGICAL.

  - __KIND__
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

#### RETURN VALUE

The return value is a LOGICAL value equal to L, with a kind
corresponding to KIND, or of the default logical kind if KIND is not
given.

#### STANDARD

Fortran 95 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__int__(3), __real__(3), __cmplx__(3)
