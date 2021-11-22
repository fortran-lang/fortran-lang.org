---
layout: book
title: get_environment
permalink: /learn/intrinsics/f_get_environment
---
### NAME

**get\_environment\_variable**(3f) - \[SYSTEM ENVIRONMENT\] Get an environmental variable

### SYNTAX
```fortran
  call get_environment_variable(NAME, VALUE, LENGTH, STATUS, TRIM_NAME)
   character(len=*),intent(in) :: NAME character(len=*),intent(out),optional :: VALUE
   integer,intent(out),optional :: LENGTH
   integer,intent(out),optional :: STATUS
   logical,intent(out),optional :: TRIM\_NAME
```
### DESCRIPTION

Get the VALUE of the environmental variable NAME.

Note that **get\_environment\_variable**(3f) need not be thread-safe. It
is the responsibility of the user to ensure that the environment is not
being updated concurrently.

### OPTIONS

  - **NAME**
    (Optional) Shall be a scalar of type CHARACTER and of default kind.

### RETURN VALUE

  - **VALUE**
    (Optional) Shall be a scalar of type CHARACTER and of default kind.
    The value of NAME is stored in VALUE. If VALUE is not large enough
    to hold the data, it is truncated. If NAME is not set, VALUE will be
    filled with blanks.

  - **LENGTH**
    (Optional) Shall be a scalar of type INTEGER and of default kind.
    Argument LENGTH contains the length needed for storing the
    environment variable NAME or zero if it is not present.

  - **STATUS**
    (Optional) Shall be a scalar of type INTEGER and of default kind.
    STATUS is **-1** if VALUE is present but too short for the
    environment variable; it is 1 if the environment variable does not
    exist and 2 if the processor does not support environment variables;
    in all other cases STATUS is zero.

  - **TRIM\_NAME**
    (Optional) Shall be a scalar of type LOGICAL and of default kind. If
    TRIM\_NAME is present with the value .FALSE., the trailing blanks in
    NAME are significant; otherwise they are not part of the environment
    variable name.

### EXAMPLE

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

### STANDARD

Fortran 2003 and later

### CLASS

Subroutine

#### @urbanjost
