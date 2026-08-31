suppressMessages(pkgload::load_all(".", quiet = TRUE))

eq <- function(lbl, a, b) {
  cat(sprintf("  %-58s %s\n", lbl,
              if (identical(a, b)) "IDENTICAL" else "DIFFERS"))
  if (!identical(a, b)) { cat("    a: "); str(a); cat("    b: "); str(b) }
}

cat("=== can the 6 sugar constructors be written with rtf_border + rtf_border_side? ===\n")
eq('rtf_border_top()      == rtf_border(top = rtf_border_side())',
   rtf_border_top(),
   rtf_border(top = rtf_border_side()))
eq('rtf_border_top("double", 30) == rtf_border(top = rtf_border_side("double", 30))',
   rtf_border_top("double", 30),
   rtf_border(top = rtf_border_side("double", 30)))
eq('rtf_border_bottom()   == rtf_border(bottom = rtf_border_side())',
   rtf_border_bottom(),
   rtf_border(bottom = rtf_border_side()))
eq('rtf_border_box()      == rtf_border(4 sides = rtf_border_side())',
   rtf_border_box(),
   local({ s <- rtf_border_side(); rtf_border(s, s, s, s) }))
eq('rtf_border_none()     == rtf_border()',
   rtf_border_none(),
   rtf_border())

cat("\n=== rtf_border_with(): what does it add? ===\n")
b <- rtf_border(top = rtf_border_side("single", 15L))
w <- rtf_border_with(b, bottom = rtf_border_side("double", 30L))
cat("  rtf_border_with(b, bottom = x) -> "); str(unclass(w), max.level = 1)
cat("  hand-written equivalent        -> ")
str(unclass(rtf_border(top = b$top, bottom = rtf_border_side("double", 30L))),
    max.level = 1)
eq("  identical?", w, rtf_border(top = b$top, bottom = rtf_border_side("double", 30L)))

cat("\n=== rtf_border_tfl(): is it sugar, or a preset? ===\n")
t <- rtf_border_tfl()
cat("  class:", class(t), "\n")
cat("  structure:\n"); str(unclass(t), max.level = 2)

cat("\n=== rtf_table_border(): a different class ===\n")
cat("  args:", paste(setdiff(names(formals(rtf_table_border)), "..."), collapse = ", "), "\n")
cat("  class of result:", class(rtf_table_border()), "\n")
