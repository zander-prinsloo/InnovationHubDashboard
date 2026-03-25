# Helper: write a temp .md file and return its path.
# Uses base tempfile() so it works both inside and outside test_that() blocks.
write_md <- function(text) {
  path <- tempfile(fileext = ".md")
  writeLines(text, path)
  path
}

# Helper: extract the HTML string from include_markdown_with_math() output.
# withMathJax() returns a tagList; the rendered HTML element is item [[2]].
html_str <- function(path) as.character(include_markdown_with_math(path)[[2]])

# ── Output type ────────────────────────────────────────────────────────────

test_that("returns a shiny tag list containing a MathJax script tag", {
  path   <- write_md("No math here.")
  result <- include_markdown_with_math(path)
  # withMathJax returns a tagList
  expect_s3_class(result, "shiny.tag.list")
  # First element is the <head> tag containing the MathJax CDN script
  result_str <- as.character(result)
  expect_true(grepl("MathJax", result_str, ignore.case = TRUE))
})

# ── Inline dollar math ($...$) ─────────────────────────────────────────────

test_that("inline dollar math $alpha$ is converted to \\(alpha\\)", {
  path <- write_md("The parameter $\\alpha$ is important.")
  html <- html_str(path)
  expect_true(grepl("\\(\\alpha\\)", html, fixed = TRUE))
  # No literal dollar signs should remain around the math
  expect_false(grepl("$\\alpha$", html, fixed = TRUE))
})

test_that("inline dollar math with subscripts is preserved without <em> corruption", {
  path <- write_md("The mean $\\bar{y}_{svy}$ is the survey mean.")
  html <- html_str(path)
  # Subscript must be intact
  expect_true(grepl("\\bar{y}_{svy}", html, fixed = TRUE))
  # No emphasis tags inside math
  expect_false(grepl("<em>", html, fixed = TRUE))
})

test_that("complex inline math with fractions and subscripts is preserved", {
  path <- write_md(
    "Defined as $L^* = \\frac{l_{0.1}\\bar{y}_{svy}}{l_{0.1}\\bar{y}_{svy} + \\frac{1}{2}(\\bar{y}_{na} - \\bar{y}_{svy})}$."
  )
  html <- html_str(path)
  expect_true(grepl("\\frac", html, fixed = TRUE))
  expect_true(grepl("\\bar{y}_{svy}", html, fixed = TRUE))
  expect_false(grepl("<em>", html, fixed = TRUE))
})

# ── Display dollar math ($$...$$) ─────────────────────────────────────────

test_that("display dollar math $$...$$ is converted to \\[...\\]", {
  path <- write_md("Display: $$\\frac{1}{2}$$")
  html <- html_str(path)
  expect_true(grepl("\\[\\frac{1}{2}\\]", html, fixed = TRUE))
  expect_false(grepl("$$", html, fixed = TRUE))
})

# ── Backslash-paren inline math (\(...\)) ──────────────────────────────────

test_that("backslash-paren \\(...\\) delimiters are preserved in output", {
  path <- write_md("Inline: \\(\\beta\\).")
  html <- html_str(path)
  expect_true(grepl("\\(\\beta\\)", html, fixed = TRUE))
})

# ── Backslash-bracket display math (\[...\]) ──────────────────────────────

test_that("backslash-bracket \\[...\\] delimiters are preserved in output", {
  path <- write_md("Display: \\[x = 1\\]")
  html <- html_str(path)
  expect_true(grepl("\\[x = 1\\]", html, fixed = TRUE))
})

# ── Mixed math and Markdown ────────────────────────────────────────────────

test_that("Markdown links survive alongside math", {
  path <- write_md(
    "See [Author (2023)](https://example.com) for $\\alpha$ details."
  )
  html <- html_str(path)
  expect_true(grepl('<a href="https://example.com"', html, fixed = TRUE))
  expect_true(grepl("\\(\\alpha\\)", html, fixed = TRUE))
})

test_that("Markdown paragraphs and bold survive alongside math", {
  path <- write_md("First para.\n\n**Bold** and $x = 1$.")
  html <- html_str(path)
  expect_true(grepl("<p>", html, fixed = TRUE))
  expect_true(grepl("<strong>", html, fixed = TRUE))
  expect_true(grepl("\\(x = 1\\)", html, fixed = TRUE))
})

# ── No-math files ─────────────────────────────────────────────────────────

test_that("file with no math renders as normal HTML without errors", {
  path <- write_md("Just plain prose. No equations here at all.")
  html <- html_str(path)
  expect_true(grepl("Just plain prose", html, fixed = TRUE))
  expect_false(grepl("MATHPH", html, fixed = TRUE))
})

# ── Multiple inline spans ─────────────────────────────────────────────────

test_that("multiple inline math spans are all converted correctly", {
  path <- write_md("Both $\\alpha$ and $\\beta$ are parameters.")
  html <- html_str(path)
  expect_true(grepl("\\(\\alpha\\)", html, fixed = TRUE))
  expect_true(grepl("\\(\\beta\\)", html, fixed = TRUE))
  expect_false(grepl("MATHPH", html, fixed = TRUE))
})

# ── Actual project files (integration) ────────────────────────────────────

test_that("dm_full_description.md: no <em> in output and math delimiters present", {
  path <- system.file("app/data/dm_full_description.md",
                      package = "InnovationHubDashboard")
  skip_if_not(file.exists(path), "dm_full_description.md not installed")
  html <- html_str(path)
  # Should contain at least one MathJax delimiter
  expect_true(grepl("\\(", html, fixed = TRUE))
  # No emphasis corruption inside math spans
  expect_false(grepl("<em>", html, fixed = TRUE))
})

test_that("yk_full_description.md: no <em> in output, complex math preserved", {
  path <- system.file("app/data/yk_full_description.md",
                      package = "InnovationHubDashboard")
  skip_if_not(file.exists(path), "yk_full_description.md not installed")
  html <- html_str(path)
  expect_true(grepl("\\(\\alpha\\)", html, fixed = TRUE))
  expect_true(grepl("\\bar{y}_{svy}", html, fixed = TRUE))
  expect_false(grepl("<em>", html, fixed = TRUE))
})

# ── Server wiring ─────────────────────────────────────────────────────────

test_that("mod_interactive_dashboard_server calls include_markdown_with_math", {
  body_str <- paste(deparse(body(mod_interactive_dashboard_server)), collapse = "\n")
  expect_true(grepl("include_markdown_with_math", body_str, fixed = TRUE))
  # Confirm the old includeMarkdown call is gone from the learn-more handler
  expect_false(grepl("includeMarkdown(md_path)", body_str, fixed = TRUE))
})
