#' Include a Markdown file with LaTeX math rendered by MathJax
#'
#' @description Reads a Markdown file and converts it to HTML suitable for
#'   rendering inside a Shiny modal, with LaTeX math expressions preserved and
#'   handed to MathJax for client-side rendering.
#'
#'   Both `markdown::mark()` and `commonmark::markdown_html()` interpret
#'   underscores inside `$...$` spans as Markdown emphasis, mangling LaTeX
#'   subscripts (e.g. `$\bar{y}_{svy}$` becomes `$\bar{y}<em>{svy}$`).
#'   This function avoids that by extracting all math spans **before** Markdown
#'   conversion, substituting unique placeholders, converting the remaining
#'   Markdown to HTML, then restoring the math spans with MathJax-compatible
#'   `\(...\)` / `\[...\]` delimiters.
#'
#'   Supported delimiter pairs (all handled defensively even if not present in
#'   current source files):
#'   - `$$...$$`  → `\[...\]`  (display math)
#'   - `$...$`    → `\(...\)`  (inline math)
#'   - `\[...\]`  →  kept as-is (display math)
#'   - `\(...\)`  →  kept as-is (inline math)
#'
#' @param path Character scalar. Absolute path to a `.md` file.
#'
#' @return A `shiny.tag.list` produced by [shiny::withMathJax()], containing
#'   the MathJax CDN script tag and the rendered HTML.
#'
#' @importFrom commonmark markdown_html
#' @importFrom shiny HTML withMathJax
#'
#' @export
#'
#' @examples
#' \dontrun{
#' md_path <- system.file("app/data/dm_full_description.md",
#'                        package = "InnovationHubDashboard")
#' include_markdown_with_math(md_path)
#' }
include_markdown_with_math <- function(path) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))

  text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  # ── 1. Extract math spans and replace with placeholders ──────────────────
  # Order matters: process $$ before $ to avoid partial matches.
  # Each placeholder is MATHPH_<zero-padded index> — chosen to be unlikely
  # to appear in normal prose and safe to round-trip through commonmark.
  math_store  <- character(0L)  # stores original math content (no delimiters)
  math_delim  <- character(0L)  # stores original delimiter type

  replace_math <- function(text, pattern, delim_type) {
    m <- gregexpr(pattern, text, perl = TRUE)[[1L]]
    if (m[[1L]] == -1L) return(text)

    matches   <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1L]]
    n_before  <- length(math_store)

    for (i in seq_along(matches)) {
      raw <- matches[[i]]
      # Strip the outer delimiters to get just the LaTeX content
      inner <- switch(
        delim_type,
        "display_dollar"  = substr(raw, 3L, nchar(raw) - 2L),
        "inline_dollar"   = substr(raw, 2L, nchar(raw) - 1L),
        "display_bracket" = substr(raw, 3L, nchar(raw) - 2L),
        "inline_paren"    = substr(raw, 3L, nchar(raw) - 2L)
      )
      math_store <<- c(math_store, inner)
      math_delim <<- c(math_delim, delim_type)
    }

    idx      <- seq(n_before + 1L, length(math_store))
    placeholders <- sprintf("MATHPH%04d", idx)
    # Replace each match with its placeholder in order
    for (i in seq_along(matches)) {
      text <- sub(pattern, placeholders[[i]], text, perl = TRUE, fixed = FALSE)
    }
    return(text)
  }

  # Process in order: display first, then inline; dollar then backslash
  text <- replace_math(text, "\\$\\$(?s:.+?)\\$\\$",       "display_dollar")
  text <- replace_math(text, "\\$(?!\\$)(?s:.+?)(?<!\\$)\\$", "inline_dollar")
  text <- replace_math(text, "\\\\\\[(?s:.+?)\\\\\\]",     "display_bracket")
  text <- replace_math(text, "\\\\\\((?s:.+?)\\\\\\)",     "inline_paren")

  # ── 2. Convert Markdown to HTML (math spans are now placeholders) ─────────
  html <- commonmark::markdown_html(text)

  # ── 3. Restore math spans with MathJax-compatible delimiters ─────────────
  for (i in seq_along(math_store)) {
    placeholder <- sprintf("MATHPH%04d", i)
    rendered <- switch(
      math_delim[[i]],
      "display_dollar"  = paste0("\\[", math_store[[i]], "\\]"),
      "inline_dollar"   = paste0("\\(", math_store[[i]], "\\)"),
      "display_bracket" = paste0("\\[", math_store[[i]], "\\]"),
      "inline_paren"    = paste0("\\(", math_store[[i]], "\\)")
    )
    html <- gsub(placeholder, rendered, html, fixed = TRUE)
  }

  # ── 4. Wrap with MathJax so the browser renders the equations ────────────
  shiny::withMathJax(shiny::HTML(html))
}
