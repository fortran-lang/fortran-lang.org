---
layout: book
title: storage_size
permalink: /learn/intrinsics/STORAGE_SIZE
---
#### NAME

__storage\_size__(3f) - \[BIT INQUIRY\] Storage size in bits
(GFDL)

#### SYNTAX

result = __storage\_size__(a \[, kind\])

#### DESCRIPTION

Returns the storage size of argument A in bits.

#### ARGUMENTS

  - __A__
    Shall be a scalar or array of any type.

  - __KIND__
    (Optional) shall be a scalar integer constant expression.

#### RETURN VALUE

The result is a scalar integer with the kind type parameter specified by
KIND (or default integer type if KIND is missing). The result value is
the size expressed in bits for an element of an array that has the
dynamic type and type parameters of A.

#### EXAMPLES

Sample program

```
    program demo_storage_size
    implicit none
       write(*,*)'size of integer ',storage_size(0)
       write(*,*)'size of real    ',storage_size(0.0)
       write(*,*)'size of logical ',storage_size(.true.)
       write(*,*)'size of complex ',storage_size((0.0,0.0))
       write(*,*)'size of integer array ',storage_size([0,1,2,3,4,5,6,7,8,9])
    end program demo_storage_size
```

#### STANDARD

Fortran 2008 and later

#### CLASS

Inquiry function

#### SEE ALSO

__c\_sizeof__(3)
