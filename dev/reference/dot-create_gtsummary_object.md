# Create gtsummary table

USE
[`as_gtsummary()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gtsummary.md)
INSTEAD! This function ingests a data frame and adds the infrastructure
around it to make it a gtsummary object.

## Usage

``` r
.create_gtsummary_object(table_body, ...)
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
