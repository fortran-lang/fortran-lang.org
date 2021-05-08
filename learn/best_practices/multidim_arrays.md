---
layout: book
title: Multidimensional Arrays
permalink: /learn/best_practices/multidim_arrays
---

Always access slices as `V(:, 1)`, `V(:, 2)`, or `V(:, :, 1)`, e.g. the
colons should be on the left. That way the stride is contiguous and it
will be fast. So when you need some slice in your algorithm, always
setup the array in a way, so that you call it as above. If you put the
colon on the right, it will be slow.

Example:

``` fortran
dydx = matmul(C(:, :, i), y) ! fast
dydx = matmul(C(i, :, :), y) ! slow
```

In other words, the "fortran storage order" is: smallest/fastest
changing/innermost-loop index first, largest/slowest/outermost-loop
index last ("Inner-most are left-most."). So the elements of a 3D array
`A(N1,N2,N3)` are stored, and thus most efficiently accessed, as:

``` fortran
do i3 = 1, N3
  do i2 = 1, N2
    do i1 = 1, N1
      A(i1, i2, i3)
    end do
  end do
end do
```

Associated array of vectors would then be most efficiently accessed as:

``` fortran
do i3 = 1, N3
  do i2 = 1, N2
    A(:, i2, i3)
  end do
end do
```

And associated set of matrices would be most efficiently accessed as:

``` fortran
do i3 = 1, N3
  A(:, :, i3)
end do
```

Storing/accessing as above then accesses always contiguous blocks of
memory, directly adjacent to one another; no skips/strides.

When not sure, always rewrite (in your head) the algorithm to use
strides, for example the first loop would become:

``` fortran
do i3 = 1, N3
  Ai3 = A(:, :, i3)
  do i2 = 1, N2
    Ai2i3 = Ai3(:, i2)
    do i1 = 1, N1
      Ai2i3(i1)
    end do
  end do
end do
```

the second loop would become:

``` fortran
do i3 = 1, N3
  Ai3 = A(:, :, i3)
  do i2 = 1, N2
    Ai3(:, i2)
  end do
end do
```

And then make sure that all the strides are always on the left. Then it
will be fast.
