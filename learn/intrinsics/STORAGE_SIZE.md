---
layout: book
title: storage_size
permalink: /learn/intrinsics/STORAGE_SIZE
---
## __Name__

__storage\_size__(3) - \[BIT:INQUIRY\] Storage size in bits


## __Syntax__
```fortran
result = storage_size(a, kind)
```
## __Description__

Returns the storage size of argument __a__ in bits.

## __Arguments__

  - __a__
    : Shall be a scalar or array of any type.

  - __kind__
    : (Optional) shall be a scalar integer constant expression.

## __Returns__

The result is a scalar integer with the kind type parameter specified by
__kind__ (or default integer type if __kind__ is missing). The result value is
the size expressed in bits for an element of an array that has the
dynamic type and type parameters of __a__.

## __Examples__

Sample program

```fortran
program demo_storage_size
implicit none
   write(*,*)'size of integer       ',storage_size(0)
   write(*,*)'size of real          ',storage_size(0.0)
   write(*,*)'size of logical       ',storage_size(.true.)
   write(*,*)'size of complex       ',storage_size((0.0,0.0))
   write(*,*)'size of integer array ',storage_size([0,1,2,3,4,5,6,7,8,9])
end program demo_storage_size
```
  Results:
```text
    size of integer                 32
    size of real                    32
    size of logical                 32
    size of complex                 64
    size of integer array           32
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__c\_sizeof__(3)](C_SIZEOF)

###### fortran-lang intrinsic descriptions
