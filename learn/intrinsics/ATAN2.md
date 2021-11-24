---
layout: book
title: atan2
permalink: /learn/intrinsics/ATAN2
---
### NAME

**atan2**(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent function
(GFDL)

### SYNTAX

result = **atan2**(y, x)

### DESCRIPTION

**atan2**(y, x) computes the arctangent of the complex number

```
      X + i Y.
```

This function can be used to transform from Cartesian into polar
coordinates and allows to determine the angle in the correct quadrant.
To convert from Cartesian Coordinates (x,y) to polar coordinates

(r,theta): $$ \\begin{aligned} r &= \\sqrt{x\*\*2 + y\*\*2} \\\\ \\theta
&= \\tan\*\*{**-1**}(y / x) \\end{aligned} $$

### ARGUMENTS

  - **Y**
    The type shall be **REAL**.

  - **X**
    The type and kind type parameter shall be the same as **Y**. If **Y** is
    zero, then **X** must be nonzero.

### RETURN VALUE

The return value has the same type and kind type parameter as **Y**. It is
the principal value of the complex number **(X + i, Y)**. If X is nonzero,
then it lies in the range **-PI \<= atan(x) \<= PI**. The sign is
positive if **Y** is positive. If **Y** is zero, then the return value is zero
if **X** is strictly positive, **PI** if **X** is negative and **Y** is positive zero
(or the processor does not handle signed zeros), and **-PI** if **X** is
negative and **Y** is negative zero. Finally, if **X** is zero, then the
magnitude of the result is **PI/2**.

### EXAMPLE

Sample program:

```
program demo_atan2
implicit none
real(4) :: x = 1.e0_4, y = 0.5e0_4
    x = atan2(y,x)
end program demo_atan2
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\|Elemental function
