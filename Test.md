ahhh
================
Jason Burns
2022-08-05

``` r
# programming in R

# if (cond)

# sytnax
# if (condition) {
#   # if true
#   statement
# }

# check if a number if positive or not
x <- 2
if (x > 0) {
  print("positive")
}
```

    ## [1] "positive"

``` r
x <- -2
if (x > 0) {
  print("positive")
}

# if (cond) else
# if (condition) {
#   # if condition is TRUE
#   statement
# } else {
#   # if condition is FALSE
#   statement
# }

x <- -2
if (x > 0) {
  print("positive")
} else {
  print("nonpositive")
}
```

    ## [1] "nonpositive"

``` r
# write a function to absolute value

my_abs <- function(x) {
  if (x >= 0) {
    return(x)
  } else {
    return(-x)
  }
}

my_abs(4)
```

    ## [1] 4

``` r
my_abs(-2)
```

    ## [1] 2

``` r
my_abs(0)
```

    ## [1] 0

``` r
# use "if" to check if there is output error in a function

my_sqrt <- function(x) {
  if (x < 0) {
    print("your input is negative, please provide a positive number")
  } else {
    return(sqrt(x))
  }
}

my_sqrt(4)
```

    ## [1] 2

``` r
my_sqrt(-2)
```

    ## [1] "your input is negative, please provide a positive number"

``` r
# by default, the last expression will be returned
sqrt(-2)
```

    ## Warning in sqrt(-2): NaNs produced

    ## [1] NaN

``` r
# if else ladder
# sytnax

# if (condition) {
#   statement1
# } else if (condition2) {
#   statement2
# } else if (condition3) {
#   statement3
# }
  
# change the score to grade
# input = a number (x)
# output = the grade
# x >= 90, A+
# 85 <= x < 90, A, etc

score_to_grade <- function(x) {
  if (x >= 90) {
    print("A+")
  } else if (x >= 85) {
    print("A")
  } else if (x >= 80) {
    print("A-")
  } else {
    print("B+ or below")
  }
}

score_to_grade(91)
```

    ## [1] "A+"

``` r
score_to_grade(90)
```

    ## [1] "A+"

``` r
score_to_grade(89)
```

    ## [1] "A"

``` r
score_to_grade(82)
```

    ## [1] "A-"

``` r
score_to_grade(79)
```

    ## [1] "B+ or below"

``` r
score_to_grade2 <- function(x) {
  if (x >= 90) {
    print("A+")
  } else if (x >= 85) {
    print("A")
  } else if (x >= 80) {
    print("A-")
  } else if (x < 80) {
    print("B+ or below")
  }
}

score_to_grade2(82)
```

    ## [1] "A-"

``` r
score_to_grade2(79)
```

    ## [1] "B+ or below"

``` r
# speed consideration
# suppose we want to simulate 200,000 normal rv's
n <- 2000000

initial_time <- proc.time()
x <- rep(0, n)
for (i in 1:n) {
  x[i] <- rnorm(1)
}
proc.time() - initial_time 
```

    ##    user  system elapsed 
    ##    2.55    0.00    2.64

``` r
initial_time <- proc.time()
x <- rnorm(n)
proc.time() - initial_time 
```

    ##    user  system elapsed 
    ##    0.08    0.00    0.07

``` r
system.time({
  x <- rnorm(n)
})
```

    ##    user  system elapsed 
    ##    0.08    0.00    0.07

``` r
# vectorized operation
x <- 1:5
y <- 10:14
x + y
```

    ## [1] 11 13 15 17 19

``` r
# slow
sum <- rep(0, 5)
for (i in 1:5) {
  sum[i] <- x[i] + y[i]
}
sum
```

    ## [1] 11 13 15 17 19

``` r
rep(0, 3) # repeat 0, 3 times
```

    ## [1] 0 0 0

``` r
# 3.5 another simulation example
no_sim <- 10000 # no. of simulations, the "n" in the law of large numbers
P0 <- 100
below <- rep(0, no_sim)

for (i in 1:no_sim) {
  Pt <- P0 * exp(cumsum(rnorm(30, mean = 0.002, sd = 0.035)))
  below[i] <- min(Pt[1:20]) < 95
  
}
mean(below) # estimate required probability
```

    ## [1] 0.582

``` r
# greater than 0.4411


plot(Pt, type = "l", ylim = c(80, 120))

abline(h = 95)
```

![](Test_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# cumsum(1:3)

for (i in 1:10) {
  for (j in 1:10) {
    
  }
}
```
