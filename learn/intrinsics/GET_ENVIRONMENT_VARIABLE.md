---
layout: book
title: get_environment
permalink: /learn/intrinsics/GET_ENVIRONMENT
---
## __Name__

__get\_environment\_variable__(3) - \[SYSTEM ENVIRONMENT\] Get an environmental variable

## __Syntax__
```fortran
  call get_environment_variable(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
   character(len=*),intent(in) :: NAME character(len=*),intent(out),optional :: VALUE
   integer,intent(out),optional :: LENGTH
   integer,intent(out),optional :: STATUS
   logical,intent(out),optional :: TRIM\_NAME
```
## __Description__

Get the VALUE of the environmental variable NAME.

Note that __get\_environment\_variable__(3) need not be thread-safe. It
is the responsibility of the user to ensure that the environment is not
being updated concurrently.

## __Options__

  - __NAME__
    (Optional) Shall be a scalar of type CHARACTER and of default kind.

## __Returns__

  - __VALUE__
    (Optional) Shall be a scalar of type CHARACTER and of default kind.
    The value of NAME is stored in VALUE. If VALUE is not large enough
    to hold the data, it is truncated. If NAME is not set, VALUE will be
    filled with blanks.

  - __LENGTH__
    (Optional) Shall be a scalar of type _integer_ and of default kind.
    Argument LENGTH contains the length needed for storing the
    environment variable NAME or zero if it is not present.

  - __STATUS__
    (Optional) Shall be a scalar of type _integer_ and of default kind.
    STATUS is __-1__ if VALUE is present but too short for the
    environment variable; it is 1 if the environment variable does not
    exist and 2 if the processor does not support environment variables;
    in all other cases STATUS is zero.

  - __TRIM\_NAME__
    (Optional) Shall be a scalar of type _logical_ and of default kind. If
    TRIM\_NAME is present with the value .FALSE., the trailing blanks in
    NAME are significant; otherwise they are not part of the environment
    variable name.

## __Examples__

Sample program:

````fortran
   program demo_getenv
   implicit none
   character(len=:),allocatable :: var
   character(len=:),allocatable :: homedir
   integer :: howbig, stat
     var='HOME'
     ! get length required to hold value
     call get_environment_variable(var, length=howbig,status=stat)
     select case (stat)
     case (1)
        print *, "HOME is not defined in the environment. Strange```"
     case (2)
        print *, "This processor doesn't support environment variables. Boooh!"
     case default
        ! make string to hold value of sufficient size
        allocate(character(len=howbig) :: homedir)
        ! get value
        call get_environment_variable(var, homedir)
        ! print environment variable name value
        write (*,'(a,"=""",a,"""")')var,trim(homedir)
     end select
   end program demo_getenv
````

Typical Results:

```
   HOME="/home/urbanjs"
```

## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions
