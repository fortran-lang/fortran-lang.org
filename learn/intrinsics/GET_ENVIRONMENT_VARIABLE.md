---
layout: book
title: get_environment
permalink: /learn/intrinsics/GET_ENVIRONMENT_VARIABLE
---
## __Name__

__get\_environment\_variable__(3) - \[SYSTEM ENVIRONMENT\] Get an environmental variable

## __Syntax__
```fortran
  call get_environment_variable(name, value, length, status, trim_name)

   character(len=*),intent(in) :: name
   character(len=*),intent(out),optional :: value
   integer,intent(out),optional :: length
   integer,intent(out),optional :: status
   logical,intent(out),optional :: trim_name
```
## __Description__

Get the __value__ of the environmental variable __name__.

Note that __get\_environment\_variable__(3) need not be thread-safe. It
is the responsibility of the user to ensure that the environment is not
being updated concurrently.

## __Options__

  - __name__
    The name of the environment variable to query.

    Shall be a scalar of type _character_ and of default kind.

## __Returns__

  - __value__
    The value of the environment variable being queried.

    Shall be a scalar of type _character_ and of default kind.
    The value of NAME is stored in __value__. If __value__ is not large enough
    to hold the data, it is truncated. If __name__ is not set, __value__ will be
    filled with blanks.

  - __length__
    Argument __length__ contains the length needed for storing the
    environment variable __name__ or zero if it is not present.

    Shall be a scalar of type _integer_ and of default kind.

  - __status__
    __status__ is __-1__ if __value__ is present but too short for the
    environment variable; it is __1__ if the environment variable does not
    exist and __2__ if the processor does not support environment variables;
    in all other cases __status__ is zero.

    Shall be a scalar of type _integer_ and of default kind.

  - __trim\_name__
    If __trim\_name__ is present with the value __.false.__, the trailing blanks in
    __name__ are significant; otherwise they are not part of the environment
    variable name.

    Shall be a scalar of type _logical_ and of default kind.

## __Examples__

Sample program:

```fortran
program demo_getenv
implicit none
character(len=:),allocatable :: homedir
character(len=:),allocatable :: var
     var='HOME'
     homedir=get_env(var)
     write (*,'(a,"=""",a,"""")')var,homedir

contains

function get_env(NAME,DEFAULT) result(VALUE)
! a function that makes calling get_environment_variable(3) simple
implicit none
character(len=*),intent(in)          :: NAME
character(len=*),intent(in),optional :: DEFAULT
character(len=:),allocatable         :: VALUE
integer                              :: howbig
integer                              :: stat
integer                              :: length
   ! get length required to hold value
   length=0
   VALUE=''
   if(NAME.ne.'')then
      call get_environment_variable(NAME, length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
         !*!print *, NAME, " is not defined in the environment. Strange..."
         VALUE=''
      case (2)
         !*!print *, "This processor doesn't support environment variables. Boooh!"
         VALUE=''
      case default
         ! make string to hold value of sufficient size
         if(allocated(VALUE))deallocate(VALUE)
         allocate(character(len=max(howbig,1)) :: VALUE)
         ! get value
         call get_environment_variable(NAME,VALUE,status=stat,trim_name=.true.)
         if(stat.ne.0)VALUE=''
      end select
   endif
   if(VALUE.eq.''.and.present(DEFAULT))VALUE=DEFAULT
end function get_env

end program demo_getenv
```

Typical Results:

```text
   HOME="/home/urbanjs"
```

## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions (@urbanjost)
