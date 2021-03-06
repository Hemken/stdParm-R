# How many terms in a full factorial model with polynomial terms?

Suppose you are considering a full-factorial model, a model with all combinations of all variables crossed.  How many terms would it have?

Many computing languages that fit linear models let you specify a model like this in very compact form.  For example, in Stata you might specify
```
regress y c.x1##c.x1##c.x2##c.x2##c.x3##c.x3
```
to indicate a model in three (continous) variables, including all their
higher order interactions, and including a quadratic term for `x1`.

The same model in R would look like
```
lm(y ~ (x1+I(x1^2)*(x2+I(x2^2))*(x3+I(x3^2)))
```

How many terms - and therefore, how many parameters - are there in these models?

## Full factorial term count

Let's start by counting the terms in a factorial model, sans polynomial terms.  (Or looking ahead, limiting the polynomial degree to 1.)

Let $v$ = a given number of variables.  Then the number of terms (parameters)
in a full factorial model is
$$\sum\limits_{i=0}^v {v \choose i}$$

- One variable
    - 1 0th order term, the constant
    - 1 1st order term
    
    total: 2 terms
    $$\begin{aligned}
    \sum\limits_{i=0}^1 {1 \choose i} &={1 \choose 0}+{1 \choose 1} \\
    &=2
    \end{aligned}$$
    
- Two variables
    - 1 0th order term, 
      (in R) `choose(2,0)` = 1
    - 2 1st order terms, 
      `choose(2,1)` = 2
    - 1 2nd order term, 
      `choose(2,2)` = 1
    
    total:  4 terms
    
    $$\begin{aligned}
    \sum\limits_{i=0}^2 {2 \choose i} &={2 \choose 0}+{2 \choose 1}+
    {2 \choose 2}\\
    &=4
    \end{aligned}$$
    
- Three variables
    - 1 0th order term, `choose(3,0)` = 1
    - 3 1st order terms, `choose(3,1)` = 3
    - 3 2nd order term2, `choose(3,2)` = 3
    - 1 3rd order term2, `choose(3,3)` = 1
    
    total:  8 terms
    
    (in R) `sum(choose(3, 0:3))` = 8
    
- Four variables
$$\sum\limits_{i=0}^4 {4 \choose i} =16$$
    (in R) `sum(choose(4, 0:4))` = 16
    
## Polynomial term count
Next let's consider the number of terms in a polynomial model with no interaction terms.

This is just $v$ variables, times $d$ the polynomial degree, choosen 0 and 1 variable at a time, one degree at a time.
$$\sum\limits_{i=0}^1{v \choose i}{d \choose 1}^i$$

- Two variables, polynomial degree 2
$$\begin{aligned}
\sum\limits_{i=0}^1{v \choose i}{2 \choose 1}^i&={2 \choose 0}{2 \choose 1}^0 + {2 \choose 1}{2 \choose 1}^1\\&=5
\end{aligned}$$
    - (in R) `sum(choose(2,0:1)*choose(2,1)^(0:1))` = 5
    
## Factorial combinations of polynomial terms
Now we combine our previous notions.
$$\sum\limits_{i=0}^v{v \choose i}{d \choose 1}^i$$

- Two variables, polynomial degree 2
$$\begin{aligned}
\sum\limits_{i=0}^2{2 \choose i}{2 \choose 1}^i
&={2 \choose 0}{2 \choose 1}^0 +
{2 \choose 1}{2 \choose 1}^1 +
{2 \choose 2}{2 \choose 1}^2 \\
&=1 + 4 + 4 \\
&=9
\end{aligned}$$
    - 0th order: 
    
      `choose(2,0)*choose(2,0)^0` =1
    - 1st order: 
    
      `choose(2,1)*choose(2,1)^1` =4
    - 2nd order: 
    
      `choose(2,2)*choose(2,1)^2` =4
      
        - both 1 degree:  1
        - 1 first degree, 1 squared: 2
        - both squared: 1
        
        total = 4
        
    grand total = 9
    
- Two variables, polynomial degree 3
$$\sum\limits_{i=0}^2{2 \choose i}{3 \choose 1}^i$$
    - 0th order:  ${2 \choose 0}\times{3 \choose 1}^0$ =1
    - 1st order:  ${2 \choose 1}\times{3 \choose 1}^1$ =6
    - 2nd order: ${2 \choose 2}\times{3 \choose 1}^2$ =9
        - both degree 1: 1
        - one degree 1, one squared:  2
        - one degree 1, one cubed:  2
        - both degree 2: 1
        - one degree 2, one degree 3: 2
        - both degree 3: 1
        
        total = 9
        
    grand total = 16
    
    `sum(choose(2,0:2)*choose(3,1)^(0:2))` =16
    
## Back to the original question ...
Our initial example was to count the terms in a full factorial model with three variables of polynomial degree 2.
$$\sum\limits_{i=0}^v{v \choose i}{d \choose 1}^i$$
Here $v=3$ and $d=2$, so we have
$$\sum\limits_{i=0}^3{3 \choose i}{2 \choose 1}^i$$
Which gives us
`sum(choose(3,0:3)*choose(2,1)^(0:3))` =27 terms.
