---
layout: book
title: Organising code structure
permalink: /learn/quickstart/organising_code
---

Most programming languages allow you to collect commonly-used code into
_procedures_ that can be reused by _calling_ them from other sections of code.

Fortran has two forms of procedure:

- __Subroutine__: invoked by a `call` statement
- __Function__: invoked within an expression or assignment to which it returns a value

Both subroutines and functions have access to variables in the parent scope by _argument association_;
unless the `value` attribute is specified, this is similar to call by reference.

## Subroutines

The subroutine input arguments, known as _dummy arguments_, are specified in parentheses after the subroutine name;
the dummy argument types and attributes are declared within the body of the subroutine just like local variables.

__Example:__

```fortran
! Print matrix A to screen
subroutine print_matrix(n,m,A)
  implicit none
  integer, intent(in) :: n
  integer, intent(in) :: m
  real, intent(in) :: A(n,m)

  integer :: i
  do i=1,n
    print *,A(i,1:m)
  end do

end subroutine print_matrix
```


Note the additional `intent` attribute when declaring the dummy arguments; this optional attribute signifies to the compiler whether the argument
is ''read-only'' (`intent(in)`) ''write-only'' (`intent(out)`) or ''read-write'' (`intent(inout)`) within the procedure.
In this example, the subroutine does not modify its arguments, hence all arguments are `intent(in)`.

{% include tip.html content="It is good practice to always specify the `intent` attribute for
dummy arguments; this allows the compiler to check for unintentional errors and provides self-documentation." %}


We can call this subroutine from a program using a `call` statement:
```fortran
program call_sub
  implicit none
  
  real :: mat(10,20)
  
  mat(:,:) = 0.0

  call print_matrix(10,20,mat)

end program call_sub
```

{% include note.html content="This example uses a so-called _explicit-shape_ array argument since we have passed additional variables to describe
the dimensions of the array `A`; this will not be necessary if we place our subroutine in a module as described later." %}


## Functions

```fortran
! L2 Norm of a vector
function vector_norm(n,vec) result(norm)
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: vec(n)
  real :: norm

  norm = sqrt(sum(vec**2))

end function vector_norm
```
{% include tip.html content="In production code, the intrinsic function `norm2` should be used." %}

To execute this function:

```fortran
program run_fcn
  implicit none

  real :: v(9)
  real :: vector_norm
  
  v(:) = 9

  print *, 'Vector norm = ',vector_norm(9,v)

end program run_fcn
```

{% include tip.html content="It is good programming practice for functions not to modify their arguments---that is, all function arguments should be `intent(in)`.
Such functions are known as `pure` functions.
Use subroutines if your procedure needs to modify its arguments." %}


## Modules

Fortran modules contain definitions that are made accessible to programs, procedures, and other modules through the `use` statement.
They can contain data objects, type definitions, procedures, and interfaces.

- Modules allow controlled scoping extension whereby entity access is made explicit
- Modules automatically generate explicit interfaces required for modern procedures

{% include tip.html content="It is recommended to always place functions and subroutines
within modules." %}

__Example:__ 

```fortran
module my_mod 
  implicit none

  private                          ! All entities are module-private by default
  public public_var, print_matrix  ! Explicitly export public entities

  real, parameter :: public_var = 2
  integer :: private_var

  contains
    
    ! Print matrix A to screen
    subroutine print_matrix(A)
      real, intent(in) :: A(:,:)  ! An assumed-shape dummy argument

      integer :: i
      do i=1,size(A,1)
        print *,A(i,:)
      end do

    end subroutine print_matrix

end module my_mod
```

{% include note.html content="Compare this `print_matrix` subroutine with [that written outside of a module](#subroutines);
we no longer have to explicitly pass the matrix dimensions and can instead take
advantage of _assumed-shape_ arguments since the module will generate the required
explicit interface for us. This results in a much simpler subroutine interface." %}

To `use` the module within a program:
```fortran
program use_mod
  use my_mod
  implicit none

  real :: mat(10,10)

  mat(:,:) = public_var

  call print_matrix(mat)

end program use_mod
```

__Example:__ explicit import list

```fortran
  use my_mod, only: public_var
```

__Example:__ aliased import

```fortran
  use my_mod, only: printMat=>print_matrix
```

{% include note.html content="Each module should be written in a separate .f90 source file. Modules need to be compiled prior to any program units that `use` them." %}



