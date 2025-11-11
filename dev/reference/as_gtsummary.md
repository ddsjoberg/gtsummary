# Create gtsummary table

This function ingests a data frame and adds the infrastructure around it
to make it a gtsummary object.

## Usage

``` r
as_gtsummary(table_body, ...)
```

## Arguments

- table_body:

  (`data.frame`)  
  a data frame that will be added as the gtsummary object's `table_body`

- ...:

  other objects that will be added to the gtsummary object list

## Value

gtsummary object

## Details

Function uses `table_body` to create a gtsummary object

## Examples

``` r
mtcars[1:2, 1:2] |>
  as_gtsummary()


  

mpg
```

cyl

21

6

21

6
