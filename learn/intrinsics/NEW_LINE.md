---
layout: book
title: new_line
permalink: /learn/intrinsics/NEW_LINE
---
### NAME

__new\_line__(3f) - \[CHARACTER\] New line character
(GFDL)

### SYNTAX

result = __new\_line__(c)

### DESCRIPTION

__new\_line__(c) returns the new-line character.

### ARGUMENTS

  - __C__
    \- The argument shall be a scalar or array of the type CHARACTER.

### RETURN VALUE

Returns a CHARACTER scalar of length one with the new-line character of
the same kind as parameter C.

### EXAMPLE

Sample program:

```
    program demo_new_line
      implicit none
      write(*,'(A)') 'This is record 1.'//NEW_LINE('A')//'This is record 2.'
    end program demo_new_line
```

### STANDARD

Fortran 2003 and later

### CLASS

Inquiry function
