# The title, footnote and content of every block of a document, in order:
# what the document's parallel lists (`titles`, `footnotes`, `contents`)
# held before each content became a block with its own (R/blocks.R)
.doc_titles    <- function(doc) unname(lapply(doc$contents, `[[`, "title"))
.doc_footnotes <- function(doc) unname(lapply(doc$contents, `[[`, "footnote"))
.doc_items     <- function(doc) unname(lapply(doc$contents, `[[`, "content"))
