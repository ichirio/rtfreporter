# Join values with a separator, skipping the missing ones

Concatenates its arguments element by element, separated by `sep`,
leaving out every value that is missing (`NA`) or empty (`""`). This is
the SAS `CATX` rule, and it is what
[`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
applies to its `vars`: a record whose middle value is missing prints
`"COMPLETED/ADENOCARCINOMA"`, never `"COMPLETED//ADENOCARCINOMA"`.

## Usage

``` r
catx(sep, ...)
```

## Arguments

- sep:

  Separator, a single string. It is inserted only *between* values that
  survive, so it never appears at the start or the end.

- ...:

  The values to join: character vectors, or anything
  [`as.character()`](https://rdrr.io/r/base/character.html) accepts. A
  factor joins by its **labels**, not its level codes.

## Value

A character vector as long as the longest argument. With no `...`
arguments, `character(0)`.

## Details

Arguments are vectorised together. A length-1 argument is recycled to
the longest one; any other length mismatch is an error, because in a
clinical listing it means two columns that were meant to line up do not.

Leading and trailing blanks are **stripped from each value** before it
is joined – again as SAS `CATX` does. Source data read from a SAS-shaped
export is routinely padded, and padding that survived into a joined cell
would push the text over the column width for no visible reason. (This
is the one place the function differs from `ydisctools::catx()`, which
keeps the padding.)

## See also

[`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md),
which joins a listing column's `vars` this way;
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md),
which applies it.

## Examples

``` r
catx("/", "COMPLETED", "BRCA1", "ADENOCARCINOMA")
#> [1] "COMPLETED/BRCA1/ADENOCARCINOMA"

# A missing value is skipped, not printed as a doubled separator.
catx("/", "COMPLETED", NA, "ADENOCARCINOMA")
#> [1] "COMPLETED/ADENOCARCINOMA"

# Vectorised, with length-1 arguments recycled.
adsl <- data.frame(
  HIST = c("ADENOCARCINOMA", "SMALL CELL"),
  BRCA = c("BRCA1", NA),
  stringsAsFactors = FALSE
)
catx("/", adsl$HIST, adsl$BRCA)
#> [1] "ADENOCARCINOMA/BRCA1" "SMALL CELL"          
catx(" ", "Cohort", c("A", "B"))
#> [1] "Cohort A" "Cohort B"

# Any separator, including none at all.
catx(", ", "Grade 3", "Prior radiation")
#> [1] "Grade 3, Prior radiation"
catx("", "AE", "0001")
#> [1] "AE0001"
```
