#!/usr/bin/env python3

from operator import add, sub, mul


"""
Q2. Write a function that takes three positive numbers and returns the sum of
the squares of the two larger numbers. Use only a single expression for the
body of the function:
"""
def two_of_three(a, b, c):
    """Return x**2 + y**2, where x and y are the two largest of a, b, c.

    >>> two_of_three(1, 2, 3)
    13
    >>> two_of_three(5, 3, 1)
    34
    >>> two_of_three(10, 2, 8)
    164
    >>> two_of_three(5, 5, 5)
    50
    """
    return sub(sum(map(lambda x: x**2, (a, b, c))), min(a, b, c)**2)



"""
Q3. Let us try to write a function that does the same thing as an if statement:

      def if_function(condition, true_result, false_result):
          # Return true_result if condition is a true value,
          # and false_result otherwise.
          if condition:
              return true_result
          else:
              return false_result
      
This function actually doesn't do the same thing as an if statement in all cases.
To prove this fact, write functions c, t, and f such that either with_if_function
or with_if_statement returns the number 1, but the other does not:

      def with_if_statement():
          if c():
              return t()
          else:
              return f()

      def with_if_function():
          return if_function(c(), t(), f())
"""
def if_function(condition, true_result, false_result):
    if condition:
        return true_result
    else:
        return false_result


def c():
    return True

def f():
    f()

def t():
    return 1


def with_if_statement():
    if c():
        return t()
    else:
        return f()


def with_if_function():
    return if_function(c(), t(), f())

try:
    # with if statement, the 'f()' will not be evaluated when the 'c()' is
    # evalutated to True.
    print(with_if_statement())
    print(with_if_function())
except:
    pass



"""
Q4. Fill in the following function definition to add a to the absolute value of
b, without calling abs:
"""
def a_plus_abs_b(a, b):
    """Return a+abs(b), but without calling abs.

    >>> a_plus_abs_b(1, -1)
    2
    >>> a_plus_abs_b(1, 1)
    2
    >>> a_plus_abs_b(1, 0)
    1
    """
    if b > 0:
        op = add
    else:
        op = sub
    return op(a, b)



"""
Q5. Define a function piecewise that takes two functions, f and g, along with a
number b and returns a new function that takes a number x and returns either
f(x) if x is less than b, or g(x) if x is greater than or equal to b.
"""
def piecewise(f, b, g):
    """ Returns the piecewise function h where:

    h(x) = f(x) if x < b, g(x) otherwise

    >>> def negate(x):
    ...     return -x
    >>> def identity(x):
    ...     return x
    >>> abs = piecewise(negate, 0, identity)
    >>> abs(6)
    6
    >>> abs(-1)
    1
    """
    return lambda x: f(x) if (x < b) else g(x)



"""
Q6. The summation function from lecture is only the simplest of a vast number
of similar abstractions that can be captured as higher-order functions. Write
a similar product function that returns the product of the values of a function
for n natural number arguments. Show how to define the factorial function in
terms of product.
"""
def product(n, term):
    """Return the product of the first n terms in the sequence formed by
    applying term to the integers 1, ..., n.

    term -- a function that takes one argument

    >>> def identity(x):
    ...     return x
    >>> def square(x):
    ...     return x * x
    >>> product(3, identity)    # 1 * 2 * 3
    6
    >>> product(5, identity)    # 1 * 2 * 3 * 4 * 5
    120
    >>> product(3, square)      # 1^2 * 2^2 * 3^2
    36
    >>> product(5, square)      # 1^2 * 2^2 * 3^2 * 4^2 * 5^2
    14400
    >>> def factorial(n):
    ...     return product(n, identity)
    >>> factorial(3)
    6
    >>> factorial(6)
    720
    """
    return reduce(mul, map(term, range(1, (n + 1))), 1)


"""
Q7. Show that both summation and product are instances of a more general
function, called accumulate, with the following signature:

def accumulate(combiner, start, n, term):

accumulate takes the same arguments term and n as summation and product,
together with a combiner function (of two arguments) that specifies how the
accumulation of the preceding terms is to be combined with each value returned
by term, and a start value that specifies what base value to use to start the
accumulation. Implement accumulate and show how summation and product can both
be defined as simple calls to accumulate.
"""
def accumulate(combiner, start, n, term):
    """Return the result of combining the first n terms in a sequence.

    >>> from operator import add
    >>> from operator import mul
    >>> def identity(x):
    ...     return x
    >>> def square(x):
    ...     return x * x
    >>> accumulate(add, 0, 5, identity)     # 0 + 1 + 2 + 3 + 4 + 5
    15
    >>> accumulate(add, 11, 5, identity)    # 11 + 1 + 2 + 3 + 4 + 5
    26
    >>> accumulate(add, 11, 0, identity)    # 11
    11
    >>> accumulate(add, 11, 3, square)      # 11 + 1^2 + 2^2 + 3^2
    25
    >>> def summationn(n, term):
    ...     return accumulate(add, 0, n, term)
    >>> def product(n, term):
    ...     return accumulate(mul, 1, n, term)
    >>> summationn(5, identity)
    15
    >>> product(5, identity)
    120
    """
    return reduce(combiner, map(term, range(1, (n + 1))), start)
