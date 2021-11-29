---
layout: book
title: command_argument_count
permalink: /learn/intrinsics/COUNT
---
## __Name__

__command\_argument\_count__(3) - \[SYSTEM ENVIRONMENT\] Get number of command line arguments

## __Syntax__

```fortran
    result = command_argument_count()

     integer function command_argument_count() result(count)
     integer :: count
```
## __Description__

__command\_argument\_count()__ returns the number of arguments passed on the
command line when the containing program was invoked.

## __Arguments__

None

## __Returns__

  - __count__
    The return value is of type default _integer_. It is the number of
    arguments passed on the command line when the program was invoked.

## __Examples__

Sample program:

```fortran
program demo_command_argument_count
implicit none
integer :: count
   count = command_argument_count()
   print *, count
end program demo_command_argument_count
```

Sample output:

```bash
   # the command verb does not count
   ./test_command_argument_count
       0
   # quoted strings may count as one argument
   ./test_command_argument_count count arguments
       2
   ./test_command_argument_count 'count arguments'
       1
```
## __Standard__

Fortran 2003 and later

## __See Also__

[__get\_command__(3)](GET_COMMAND),
[__get\_command\_argument__(3)](GET_COMMAND_ARGUMENT)

###### fortran-lang intrinsic descriptions (@urbanjost)
