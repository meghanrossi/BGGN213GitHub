BGGN213\_4\_24\_19
================

We want to make a function to drop the lowest homework grade and give the student's \# average score.
=====================================================================================================

First, individual students 1 and 2
==================================

``` r
# Let's make our function, called "grade"
grade <- function(x, na.rm = TRUE){
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
y <- x[x > min(x)]
return(mean(y))
}

# Two example students
st1 <- c(100, 100, 100, 100, 100, 100, 100, 90)
grade(st1)
```

    ## [1] 100

``` r
st2 <- c(100, NA, 90, 90, 90, 90, 97, 80)
grade(st2)
```

    ## [1] 92.83333

Let's try this on a larger data-set
===================================

``` r
class <- read.csv("student_homework.csv", header = TRUE, row.names = 1)
ans <- apply(class, 1, grade)
```

Who has the highest grade in the class?
=======================================

``` r
sort(ans, decreasing = TRUE)
```

    ## student-18  student-7  student-8 student-13  student-1 student-12 
    ##   97.00000   94.00000   93.75000   92.25000   91.75000   91.75000 
    ##  student-9 student-16  student-6  student-5  student-4 student-17 
    ##   91.33333   89.50000   89.00000   88.25000   88.00000   88.00000 
    ## student-14 student-11  student-3 student-15 student-19 student-20 
    ##   87.75000   86.00000   84.25000   83.33333   82.75000   82.75000 
    ##  student-2 student-10 
    ##   82.50000   81.33333
