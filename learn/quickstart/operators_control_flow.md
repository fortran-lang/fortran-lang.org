---
layout: book
title: Operators and flow control
permalink: /learn/quickstart/operators_control_flow
---

One of the powerful advantages of computer algorithms, compared to simple mathematical formulae,
comes in the form of program _branching_ whereby the program can decide which instructions to
execute next based on a logical condition.

There are two main forms of controlling program flow:

- _Conditional_ (if): choose program path based on a boolean (true or false) value

- _Loop_: repeat a portion of code multiple times



## Logical operators
Before we use a conditional branching operator, we need to be able to form
a logical expression.

To form a logical expression, the following set of relational operators are available:

| Operator &nbsp;  | Alternative &nbsp;    | Description                                                     |
|:----------------:|:---------------------:|-----------------------------------------------------------------|
| `==`             | `.eq.`                | Tests for equality of two operands                              |
| `/=`             | `.ne.`                | Test for inequality of two operands                             |
| `> `             | `.gt.`                | Tests if left operand is strictly greater than right operand    |
| `< `             | `.lt.`                | Tests if left operand is strictly less than right operand       |
| `>=`             | `.ge.`                | Tests if left operand is greater than or equal to right operand |
| `<=`             | `.le.`                | Tests if left operand is less than or equal to right operand    |

<br>

as well as the following logical operators:

| Operator &nbsp;       | Description                                                          |
|:---------------------:|----------------------------------------------------------------------|
| `.and.`               | TRUE if both left and right operands are TRUE                        |
| `.or.`                | TRUE if either left or right or both operands are TRUE               |
| `.not.`               | TRUE if right operand is FALSE                                       |
| `.eqv.`               | TRUE if left operand has same logical value as right operand         |
| `.neqv.`              | TRUE if left operand has the opposite logical value as right operand |

<br>


## Conditional construct (`if`)

In the following examples, a conditional `if` construct is used to print out a
message to describe the nature of the `angle` variable:

__Example:__ single branch `if`

```fortran
if (angle < 90.0) then
  print *, 'Angle is acute'
end if
```

In this first example, the code within the `if` construct is _only executed if_ the
test expression (`angle < 90.0`) is true.

{% include tip.html content="It is good practice to indent code within constructs such as `if` and `do`
to make code more readable." %}

We can add an alternative branch to the construct using the `else` keyword:

__Example:__ two-branch `if`-`else`

```fortran
if (angle < 90.0) then
  print *, 'Angle is acute'
else
  print *, 'Angle is obtuse'
end if
```

Now there are two _branches_ in the `if` construct, but _only one branch is executed_ depending
on the logical expression following the `if` keyword.

We can actually add any number of branches using `else if` to specify more conditions:

__Example:__ multi-branch  `if`-`else if`-`else`
```fortran
if (angle < 90.0) then
  print *, 'Angle is acute'
else if (angle < 180.0) then
  print *, 'Angle is obtuse'
else
  print *, 'Angle is reflex'
end if
```

When multiple conditional expressions are used, each conditional expression is tested only if none of the previous
expressions have evaluated to true.

## Loop constructs (`do`)

In the following example, a `do` loop construct is used to print out the numbers in
a sequence.
The `do` loop has an integer _counter_ variable which is used to track which iteration of the loop
is currently executing. In this example we use a common name for this counter variable: `i`.

When we define the start of the `do` loop, we use our counter variable name followed by an equals (`=`) sign
to specify the start value and final value of our counting variable.

__Example:__ `do` loop

```fortran
integer :: i

do i = 1, 10
  print *, i
end do
```

__Example:__ `do` loop with skip

```fortran
integer :: i

do i = 1, 10, 2
  print *, i  ! Print odd numbers
end do
```

### Conditional loop (`do while`)

A condition may be added to a `do` loop with the `while` keyword. The loop will be executed while the condition given
in `while()` evaluates to `.true.`.

__Example:__ `do while()` loop

```fortran
integer :: i

i = 1
do while (i < 11)
  print *, i
  i = i + 1
end do
! Here i = 11
```

### Loop control statements (`exit` and `cycle`)

Most often than not, loops need to be stopped if a condition is met. Fortran provides two executable statements to deal
with such cases.

`exit` is used to quit the loop prematurely. It is usually enclosed inside an `if`.

__Example:__ loop with `exit`

```fortran
integer :: i

do i = 1, 100
  if (i > 10) then
    exit  ! Stop printing numbers
  end if
  print *, i
end do
! Here i = 11
```

On the other hand, `cycle` skips whatever is left of the loop and goes into the next cycle.

__Example:__ loop with `cycle`

```fortran
integer :: i

do i = 1, 10
  if (mod(i, 2) == 0) then
      cycle  ! Don't print even numbers
  end if
  print *, i
end do
```
{% include note.html content="When used within nested loops, the `cycle` and `exit` statements operate on the innermost loop." %}

### Nested loop control: tags

A recurring case in any programming language is the use of nested loops. Nested loops refer to loops that exist within another loop. Fortran allows the programmer to _tag_ or _name_ each loop. If loops are tagged, there are two potential benefits:
1. The readability of the code may be improved (when the naming is meaningful).
2. `exit` and `cycle` may be used with tags, which allows for very fine-grained control of the loops.

__Example:__ tagged nested loops

```fortran
integer :: i, j

outer_loop: do i = 1, 10
  inner_loop: do j = 1, 10
    if ((j + i) > 10) then  ! Print only pairs of i and j that add up to 10
      cycle outer_loop  ! Go to the next iteration of the outer loop
    end if
    print *, 'I=', i, ' J=', j, ' Sum=', j + i
  end do inner_loop
end do outer_loop
```

### Parallelizable loop (`do concurrent`)

The `do concurrent` loop is used to explicitly specify that the _inside of the loop has no interdependencies_; this informs the compiler that it may use parallelization/_SIMD_ to speed up execution of the loop and conveys programmer intention more clearly. More specifically, this means
that any given loop iteration does not depend on the prior execution of other loop iterations. It is also necessary that any state changes that may occur must only happen within each `do concurrent` loop.
These requirements place restrictions on what can be placed within the loop body.


{% capture note %}
Simply replacing a `do` loop with a `do concurrent` does not guarantee parallel execution.
The explanation given above does not detail all the requirements that need to be met in order to write a correct `do concurrent` loop.
Compilers are also free to do as they see fit, meaning they may not optimize the loop (e.g., a small number of iterations doing a simple calculation, like the below example).
In general, compiler flags are required to activate possible parallelization for `do concurrent` loops.
{% endcapture %}
{% include important.html content=note %}

__Example:__ `do concurrent()` loop

```fortran
real, parameter :: pi = 3.14159265
integer, parameter :: n = 10
real :: result_sin(n)
integer :: i

do concurrent (i = 1:n)  ! Careful, the syntax is slightly different
  result_sin(i) = sin(i * pi/4.)
end do

print *, result_sin
```
