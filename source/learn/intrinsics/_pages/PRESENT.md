## present
### __Name__

__present__(3) - [STATE\] Determine whether an optional dummy argument
                 is specified

### __Syntax__
```fortran
result = present(a)

   function present (a)
   logical :: present
```
### __Description__

Determines whether an optional dummy argument is present.

### __Arguments__

  - __a__
    : May be of any type and may be a pointer, scalar or array value,
    or a dummy procedure. It shall be the name of an optional dummy
    argument accessible within the current subroutine or function.

### __Returns__

Returns either __.true.__ if the optional argument __a__ is present,
or __.false.__ otherwise.

### __Examples__

Sample program:

```fortran
program demo_present
implicit none
   write(*,*) func(), func(42)
contains

integer function func(x)
integer, intent(in), optional :: x
   if(present(x))then
     func=x**2
   else
     func=0
   endif
end function

end program demo_present
```
  Results:
```text
     0        1764
```

### __Standard__

Fortran 95 and later

####### fortran-lang intrinsic descriptions
