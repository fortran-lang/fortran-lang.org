---
layout: book
title: count
permalink: /learn/intrinsics/COUNT
---
## __Name__

__command\_argument\_count__(3) - \[SYSTEM ENVIRONMENT\] Get number of command line arguments

## __Syntax__


```fortran
    result = command_argument_count()
     integer :: result
```

## __Description__

command\_argument\_count returns the number of arguments passed on the
command line when the containing program was invoked.

## __Arguments__

None

## __Returns__

  - __RESULT__
    The return value is of type default integer. It is the number of
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

```
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

__get\_command__(3), __get\_command\_argument__(3)

##### fortran-lang intrinsic descriptions
