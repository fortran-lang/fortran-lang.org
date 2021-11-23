---
layout: book
title: new_line
permalink: /learn/intrinsics/NEW_LINE
---
### NAME

**new\_line**(3f) - \[CHARACTER\] New line character
(GFDL)

### SYNTAX

result = **new\_line**(c)

### DESCRIPTION

**new\_line**(c) returns the new-line character.

### ARGUMENTS

  - **C**
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
