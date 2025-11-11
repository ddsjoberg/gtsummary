# Default glance function

This is an S3 generic used as the default function in
`add_glance*(glance_fun)`. It's provided so various regression model
classes can have their own default functions for returning statistics.

## Usage

``` r
glance_fun_s3(x, ...)

# Default S3 method
glance_fun_s3(x, ...)

# S3 method for class 'mira'
glance_fun_s3(x, ...)
```

## Arguments

- x:

  (regression model)  
  a regression model object

- ...:

  These dots are for future extensions and must be empty.

## Value

a function

## Examples

``` r
mod <- lm(age ~ trt, trial)

glance_fun_s3(mod)
#> function (x, ...) 
#> {
#>     UseMethod("glance")
#> }
#> <bytecode: 0x55e14e67fe30>
#> <environment: namespace:generics>
```
