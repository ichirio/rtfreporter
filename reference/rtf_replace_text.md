# Replace text inside a generated RTF file

A post-processing helper that performs find-and-replace directly on a
rendered `.rtf` file's bytes. It is meant for the "last mile" of TLG
production – small textual fixes to a finished deliverable – not as a
substitute for building the report correctly with the pipe API.

## Usage

``` r
rtf_replace_text(
  input_file,
  target,
  replacement,
  output_file = NULL,
  encoding = "UTF-8",
  use_regex = FALSE,
  case_insensitive = FALSE,
  backup = TRUE
)
```

## Arguments

- input_file:

  Path to the RTF file to read.

- target:

  Character vector of strings (or regex patterns when
  `use_regex = TRUE`) to search for. Must be non-empty.

- replacement:

  Character vector of replacements. Either the same length as `target`,
  or length 1 (recycled to every target).

- output_file:

  Path to write the result to. `NULL` (default) overwrites `input_file`
  in place (see `backup`).

- encoding:

  Encoding of `input_file`, used when reading and writing. Default
  `"UTF-8"`. rtfreporter writes ASCII-safe RTF, so the default is
  usually correct.

- use_regex:

  Logical. `FALSE` (default) treats `target` as a fixed string; `TRUE`
  treats it as a Perl-compatible regular expression.

- case_insensitive:

  Logical. `FALSE` (default) matches case-sensitively. When `TRUE`,
  matching ignores case (fixed targets are safely escaped before the
  case-insensitive match, so regex metacharacters stay literal).

- backup:

  Logical. When overwriting in place (`output_file = NULL`), first copy
  the original to `paste0(input_file, ".bak")`. Default `TRUE`. Ignored
  when `output_file` is given.

## Value

The normalised path to the written file, invisibly.

## Details

One or more `target` strings are replaced by `replacement`. Pass
equal-length vectors to apply several replacements, or a single
`replacement` to use the same value for every `target`. Replacements are
applied **sequentially**, so an earlier replacement can affect a later
one.

## Important – this operates on the raw RTF bytes

The file is treated as plain text, so a `target` only matches when it
appears **literally** in the file. RTF stores formatting as control
words and escapes non-ASCII characters (`\\uN`, `\\'XX`), so:

- Plain ASCII runs and RTF control words match and replace fine.

- Text that is split across RTF control words, or non-ASCII characters
  that the renderer escaped, will **not** match their on-screen form.

For anything structural, change the inputs to the pipe API instead.

## Examples

``` r
if (FALSE) { # \dontrun{
generate_rtfreport(doc, "table.rtf", overwrite = TRUE)

# Fix a footnote wording in place (keeps table.rtf.bak)
rtf_replace_text("table.rtf", "Saftey Population", "Safety Population")

# Several replacements at once, writing to a new file
rtf_replace_text(
  "table.rtf",
  target      = c("DRAFT", "vX.Y"),
  replacement = c("FINAL", "v1.0"),
  output_file = "table_final.rtf"
)
} # }
```
