# Print an rtf_document object

Print an rtf_document object

## Usage

``` r
# S3 method for class 'rtf_document'
print(x, ...)
```

## Arguments

- x:

  An rtf_document object.

- ...:

  Additional arguments (unused).

## Value

`x`, invisibly. Called for the side effect of printing a one-line
summary (page count, sections defined, page size).

## Examples

``` r
print(rtf_document())
#> rtf_document object
#>   Pages: 0 
#>   Sections defined: 0 
#>   Document page size: x inches
```
