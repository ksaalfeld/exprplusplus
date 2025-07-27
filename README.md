# About

This package adds vector/list support to the Tcl expr command.

It's a proof-of-concept implementation: Use it at your own risk.

# Example

This example computes the median absolute deviation of a vector/list:

    set x {2.3 3.1 2.8 2.5 3.2}
    set mad [expr {median(abs(sub($x, median($x))))}]

Computing the standard deviation of a vector/list:

    set x {2.3 3.1 2.8 2.5 3.2}
    set stddev [expr {sqrt(mean(pow(sub($x, mean($x)), 2)))}]

It's possible to intermix scalar values and vectors:

    set x {1.0 2.0 3.0 4.0 5.0}
    set y [expr {sub(mul($x,2),1)}]
    # y: 1.0 3.0 5.0 7.0 9.0

# Usage

Put package into auto_path and load it:

    package require expr++

Changes get into effect after loading the package.

Beside replacing existing functions the package adds some new expr functions:

    nroot(x, n) - Compute the Nth root of X
    sgn(x) - Signum function
    fix(x) - Return integral part X
    trunc(x) - Truncate the fractional part of X
    asinh(x) - Inverse hyperbolic sine
    acosh(x) - Inverse hyperbolic cosine
    atanh(x) - Inverse hyperbolic tangens
    sat(x, z, a) - Saturate X in range [a, z]
    fit(x, type) - Fit (saturate) value X to range of given type
    mean(x) - Compute the mean value (average) of X
    median(x) - Compute the median value of X

Tcl does not allow to replace expr operators. Therefore new expr functions
are added that can handle vectors/lists:

    add(a, [b]) - Add scalars or lists/vectors A and B
    sub(a, [b]) - Subtract scalars or lists/vectors A and B
    mul(a, [b]) - Multiply scalars or lists/vectors A and B
    div(a, [b]) - Divide scalars or lists/vectors A and B
    mod(a, [b]) - Modulo supporting scalars and lists/vectors

When called with a single list/vector argument the functions successively
apply the operation to the elements of that list and return a scalar value.
For example the following adds all elements of X returning their sum:

    set x {1 2 3 4 5 6 7}
    set y [expr {add($x)}]
    # y: 28

The capability of round() function is extended by optional new arguments
to specify the rounding mode and integral saturation:

    round(x, [mode, [type]])

By default the behavior of round() when called with a single argument is unchanged.
The mode argument can be one of the following values:

    "infinity" (round ties away from zero)
    "zero" (round ties towards zero)
    "ceil" (round ties towards +inf)
    "floor" (round ties towards -inf)

The type argument corresponds to the type argument of the fit() function
and can be set to one of the following values:

    "int" (integer of arbitrary size)
    "uint" (unsigned integer of arbitrary size)
    "bool" (boolean value)
    "double" (floating point value)
    "byte" (8 bit unsigned integer)
    "int8" (8 bit signed integer)
    "int16" (16 bit signed integer)
    "int32" (32 bit signed integer)
    "int64" (64 bit signed integer)
    "uint8" (8 bit unsigned integer)
    "uint16" (16 bit unsigned integer)
    "uint32" (32 bit unsigned integer)
    "uint64" (64 bit unsigned integer)

To compare elements of lists/vectors a cmp() math function has been added:

    cmp(a, b) - Compare scalar or list/vector A and B

The function returns -1, 0 or 1 for each element in list A and B
depending on their relation:

    -1 - Element A is smaller than B element
     0 - Element A is equal to B element
     1 - Element A is bigger than B element

There are specific version to test for particular relations:

    cmpeq(a, [b]) - Test elements for equality (A == B)
    cmpne(a, [b]) - Test elements for inequality (A != B)
    cmpgt(a, [b]) - Test if A elements are greater than B elements (A > B)
    cmpge(a, [b]) - Test if A elements are greater/equal than B elements (A >= B)
    cmplt(a, [b]) - Test if A elements are lower than B elements (A < B)
    cmple(a, [b]) - Test if A elements are lower/equal B elements (A <= B)

Bit manipulation of lists/vectors is supported by the following math functions:

    bitand(a, [b]) - Bitwise AND of scalars or elements in lists/vectors A and B
    bitor(a, [b]) - Bitwise OR of scalars or elements in lists/vectors A and B
    bitxor(a, [b]) - Bitwise XOR of scalars or elements in lists/vectors A and B
    bitnot(x) - Bitwise NOT of scalars or elements in list/vector X
    bitget(x, k, [n]) - Get N bits (default: 1) from X starting at zero-based index K
    bitshift(x, k) - Bit shifting of elements in X by K positions

The functions bitshift() and bitget() support K being a list/vector.

    range(N) - Create a list of values containing 0,1,2, ... N-1
    range(A, Z) - Create a list of values A <= A+i*STEP < Z with step size 1
    range(A, Z, STEP) - Create a list of values A <= A+i*STEP < Z with given step size
    pair(A, B) - Combine elements in A and B as key/value pairs returning the result as a single list
    dup(N, X) - Duplicate elements combining the result to a single list

Note that expr is implicitely used within the conditionals of control structures (if, while, for, ...).
The features added by this package are therefore available in their conditionals as well.
