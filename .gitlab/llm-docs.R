## LLM copies of a pkgdown site: pandoc choice, math repair, final check
##
## pkgdown (>= 2.2.0) writes llms.txt and one .md per page with
## build_llm_docs(). Two defects reach those copies.
##
## Code blocks. pandoc 3.11 reads <pre>\n<code> as inline code, which
## flattens every knitr block into runs of `x ``<-`` 1`. A pandoc that keeps
## the fence (3.10 does) has to be chosen before the conversion.
##
## Math. The page HTML keeps MathJax's raw delimiters, \(...\) for \eqn and
## $$...$$ for \deqn (\[...\] in articles). pandoc reads them as text, and
## the gfm writer escapes them, so \(\pi_i\) becomes \\\pi_i\\ and $$ becomes
## \$\$. pandoc's HTML reader does not turn a math span into math either.
## The repair swaps each formula for a placeholder word before the
## conversion and writes it back as $...$ or $$...$$.
##
## Source this file from a package's build script, in the session that runs
## the conversion, since the pandoc choice is per session:
##
##   source(file.path(Sys.getenv("R_GATES_DIR", "~/Devel/R/gates"),
##                    "llm-docs.R"))
##   llm_docs_pandoc()
##   pkg <- pkgdown::as_pkgdown(".")
##   pkgdown::build_llm_docs(pkg)
##   repair_llm_math(pkg)
##   check_llm_docs(pkg)
##
## repair_llm_math() calls three pkgdown internals (get_site_paths(),
## full_url(), convert_md()), checked on entry, so a pkgdown release that
## renames them fails loudly instead of skipping the repair.

## Choose a pandoc that keeps <pre>\n<code> as a fenced block
##
## `extra` paths are tried first, then LLM_DOCS_PANDOC, the pandoc on PATH,
## and the copies bundled with Quarto, RStudio and Positron. Sets it as
## rmarkdown's pandoc for this session and returns its path.
llm_docs_pandoc <- function(extra = character(0)) {
  candidates <- unique(c(
    extra,
    Sys.getenv("LLM_DOCS_PANDOC"),
    Sys.which("pandoc"),
    "/opt/quarto/bin/tools/x86_64/pandoc",
    "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64/pandoc",
    "/usr/share/positron/resources/app/quarto/bin/tools/x86_64/pandoc"
  ))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]

  keeps_fence <- function(pandoc) {
    input <- tempfile(fileext = ".html")
    on.exit(unlink(input), add = TRUE)
    writeLines(
      paste0(
        "<main><div class=\"sourceCode\"><pre class=\"r\">\n",
        "<code class=\"sourceCode R\"><span>x</span>\n<span>y</span>",
        "</code></pre></div></main>"
      ),
      input
    )
    out <- system2(
      pandoc,
      c("-f", "html", "-t", "gfm+definition_lists-raw_html", input),
      stdout = TRUE
    )
    length(out) > 0 && startsWith(out[[1]], "```")
  }

  usable <- Filter(keeps_fence, candidates)
  if (!length(usable)) {
    stop(
      "No pandoc keeps <pre>\\n<code> as a code block. Tried: ",
      paste(candidates, collapse = ", "),
      ". Set LLM_DOCS_PANDOC to a pandoc 3.10 binary."
    )
  }
  pandoc <- usable[[1]]
  invisible(rmarkdown::find_pandoc(cache = FALSE, dir = dirname(pandoc)))
  message(
    "Using ", system2(pandoc, "--version", stdout = TRUE)[[1]], " at ", pandoc
  )
  invisible(pandoc)
}

## Swap each formula outside <pre> and <code> for a placeholder word
##
## Only <main> is read, as pkgdown's conversion reads it. The head holds
## MathJax's own configuration, whose delimiters are not formulas. Returns
## the rewritten HTML and the formulas, in placeholder order. The TeX is
## read from HTML text, so its entities are decoded.
llm_math_placeholders <- function(html) {
  protected <- paste(
    "(?s)\\A.*?<main\\b",
    "</main>.*\\z",
    "<pre\\b.*?</pre>",
    "<code\\b.*?</code>",
    "<script\\b.*?</script>",
    sep = "|"
  )
  math <- paste(
    "(?s)\\\\\\((.+?)\\\\\\)",
    "\\\\\\[(.+?)\\\\\\]",
    "\\$\\$(.+?)\\$\\$",
    sep = "|"
  )
  decode <- function(x) {
    x <- gsub("&lt;", "<", x, fixed = TRUE)
    x <- gsub("&gt;", ">", x, fixed = TRUE)
    x <- gsub("&quot;", "\"", x, fixed = TRUE)
    x <- gsub("&#39;", "'", x, fixed = TRUE)
    x <- gsub("&amp;", "&", x, fixed = TRUE)
    trimws(gsub("\\s+", " ", x))
  }

  # Split into protected and free runs, and rewrite the free ones only.
  cuts <- gregexpr(protected, html, perl = TRUE)[[1]]
  starts <- if (cuts[1] == -1) integer(0) else as.integer(cuts)
  ends <- starts + attr(cuts, "match.length") - 1L
  bounds <- sort(c(1L, starts, ends + 1L, nchar(html) + 1L))
  pieces <- substring(html, bounds[-length(bounds)], bounds[-1] - 1L)
  is_free <- !(bounds[-length(bounds)] %in% starts)

  formulas <- list()
  for (i in which(is_free & nzchar(pieces))) {
    found <- gregexpr(math, pieces[[i]], perl = TRUE)
    hits <- regmatches(pieces[[i]], found)[[1]]
    if (!length(hits)) {
      next
    }
    tokens <- character(length(hits))
    for (j in seq_along(hits)) {
      k <- length(formulas) + 1L
      display <- !startsWith(hits[[j]], "\\(")
      tex <- decode(substr(hits[[j]], 3L, nchar(hits[[j]]) - 2L))
      formulas[[k]] <- list(tex = tex, display = display)
      tokens[[j]] <- sprintf("LLMMATH%05dX", k)
    }
    regmatches(pieces[[i]], found) <- list(tokens)
  }
  list(html = paste(pieces, collapse = ""), formulas = formulas)
}

## Re-convert every page whose HTML holds math, with the formulas restored
##
## Run after pkgdown::build_llm_docs(pkg), with the pandoc llm_docs_pandoc()
## chose. llms.txt is rebuilt from the repaired index pages, as pkgdown
## builds it. Returns the number of formulas restored.
repair_llm_math <- function(pkg) {
  internals <- c("get_site_paths", "full_url", "convert_md")
  absent <- internals[!vapply(
    internals,
    function(f) exists(f, envir = asNamespace("pkgdown"), inherits = FALSE),
    logical(1)
  )]
  if (length(absent)) {
    stop(
      "pkgdown ", utils::packageVersion("pkgdown"), " lacks ",
      paste(absent, collapse = ", "), ". Update llm-docs.R."
    )
  }

  dst <- pkg$dst_path
  restored <- 0L
  for (path in pkgdown:::get_site_paths(pkg)) {
    src <- file.path(dst, path)
    html <- paste(readLines(src, warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
    swapped <- llm_math_placeholders(html)
    if (!length(swapped$formulas)) {
      next
    }
    tmp <- tempfile(fileext = ".html")
    writeLines(swapped$html, tmp, useBytes = TRUE)
    out <- sub("\\.html$", ".md", src)
    pkgdown:::convert_md(tmp, out, pkgdown:::full_url(pkg, path))
    unlink(tmp)

    md <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
    for (k in seq_along(swapped$formulas)) {
      f <- swapped$formulas[[k]]
      token <- sprintf("LLMMATH%05dX", k)
      tex <- if (f$display) {
        paste0("\n\n$$\n", f$tex, "\n$$\n\n")
      } else {
        paste0("$", f$tex, "$")
      }
      if (!grepl(token, md, fixed = TRUE)) {
        stop("Formula ", k, " of ", path, " was lost in the conversion.")
      }
      md <- sub(token, tex, md, fixed = TRUE)
      restored <- restored + 1L
    }
    # Adjacent display blocks leave runs of blank and space-only lines.
    md <- gsub("\n[ \t]*(\n[ \t]*)+\n", "\n\n", md)
    writeLines(md, out, useBytes = TRUE)
  }

  read_if <- function(f) {
    if (file.exists(f)) readLines(f, warn = FALSE) else character(0)
  }
  writeLines(
    c(
      read_if(file.path(dst, "index.md")), "",
      read_if(file.path(dst, "reference", "index.md")), "",
      read_if(file.path(dst, "articles", "index.md"))
    ),
    file.path(dst, "llms.txt")
  )
  message(sprintf("Restored %d formulas as TeX", restored))
  invisible(restored)
}

## Fail on flattened code or escaped math left in the LLM copies
##
## Code spans, fences and restored $...$ math are removed first, so a
## backslash an R example or a formula legitimately holds is not counted.
check_llm_docs <- function(pkg) {
  dst <- pkg$dst_path
  pages <- c(
    file.path(dst, "llms.txt"),
    list.files(dst, "\\.md$", recursive = TRUE, full.names = TRUE)
  )
  pages <- pages[file.exists(pages)]

  prose <- function(lines) {
    text <- paste(lines, collapse = "\n")
    text <- gsub("(?s)```.*?```", "", text, perl = TRUE)
    text <- gsub("(?m)^    .*$", "", text, perl = TRUE)
    # An escaped dollar is the defect, so only unescaped ones delimit math.
    text <- gsub(
      "(?s)(?<!\\\\)\\$\\$.*?(?<!\\\\)\\$\\$", "", text, perl = TRUE
    )
    text <- gsub("`[^`\n]*`", "", text, perl = TRUE)
    gsub("(?<!\\\\)\\$[^$\n]+(?<!\\\\)\\$", "", text, perl = TRUE)
  }
  flattened <- character(0)
  mangled <- character(0)
  for (f in pages) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    if (any(grepl("`` ``", lines, fixed = TRUE))) {
      flattened <- c(flattened, f)
    }
    if (grepl("\\\\\\\\|\\\\\\(|\\\\\\[|\\\\\\$\\\\\\$", prose(lines))) {
      mangled <- c(mangled, f)
    }
  }
  rel <- function(x) sub(paste0("^", dst, "/?"), "", x)
  if (length(flattened) || length(mangled)) {
    stop(
      if (length(flattened)) {
        paste0("Flattened code in ", paste(rel(flattened), collapse = ", "),
               ". ")
      },
      if (length(mangled)) {
        paste0("Escaped math in ", paste(rel(mangled), collapse = ", "), ".")
      }
    )
  }
  message(sprintf("%d LLM pages checked, no flattened code or escaped math",
                  length(pages)))
  invisible(TRUE)
}
