## Every use of a suggested package is guarded

suggests_calls <- function(expr) {
  out <- character(0)
  walk <- function(e) {
    if (is.call(e)) {
      f <- e[[1]]
      if (is.call(f) && identical(f[[1]], as.name("::"))) {
        out <<- c(out, paste0("pkg:", as.character(f[[2]])))
      } else if (is.name(f)) {
        out <<- c(out, as.character(f))
      }
      for (a in as.list(e)[-1]) if (!missing(a)) walk(a)
    }
  }
  walk(expr)
  unique(out)
}

skipped_packages <- function(expr) {
  out <- character(0)
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], as.name("skip_if_not_installed"))) {
        out <<- c(out, as.character(e[[2]]))
      }
      for (a in as.list(e)[-1]) if (!missing(a)) walk(a)
    }
  }
  walk(expr)
  out
}

unguarded_suggests_blocks <- function(dir) {
  suggested <- c("survey", "svrep", "srvyr", "sampling")
  exports <- c(as_svydesign = "survey", as_svrepdesign = "survey")
  # svrep and srvyr cannot load without survey.
  implied <- list(svrep = "survey", srvyr = "survey")
  defs <- list()
  blocks <- list()
  file_skips <- list()
  for (path in list.files(dir, "^(test|helper)-.*[.]R$", full.names = TRUE)) {
    file <- basename(path)
    for (e in parse(path, keep.source = FALSE)) {
      if (!is.call(e)) next
      head <- e[[1]]
      if (identical(head, as.name("<-")) && is.call(e[[3]]) &&
            identical(e[[3]][[1]], as.name("function"))) {
        defs[[as.character(e[[2]])]] <- suggests_calls(e[[3]])
      } else if (identical(head, as.name("test_that"))) {
        # A block that mocks the installation check runs without the package.
        mocked <- any(grepl("check_installed = ", deparse(e[[3]]),
                            fixed = TRUE))
        blocks[[length(blocks) + 1L]] <- list(
          label = paste0(file, ": ", e[[2]]),
          calls = if (mocked) character(0) else suggests_calls(e[[3]]),
          skips = skipped_packages(e[[3]]),
          file = file
        )
      } else if (identical(head, as.name("skip_if_not_installed"))) {
        file_skips[[file]] <- c(file_skips[[file]], as.character(e[[2]]))
      }
    }
  }
  needs <- function(calls, seen = character(0)) {
    pkgs <- intersect(sub("^pkg:", "", calls[startsWith(calls, "pkg:")]),
                      suggested)
    pkgs <- c(pkgs, unname(exports[intersect(calls, names(exports))]))
    for (fn in setdiff(intersect(calls, names(defs)), seen)) {
      seen <- c(seen, fn)
      pkgs <- c(pkgs, needs(defs[[fn]], seen))
    }
    unique(pkgs)
  }
  unguarded <- character(0)
  for (b in blocks) {
    have <- c(b$skips, file_skips[[b$file]])
    have <- unique(c(have, unlist(implied[have])))
    lacking <- setdiff(needs(b$calls), have)
    if (length(lacking) > 0L) {
      unguarded <- c(unguarded, paste0(b$label, " [", toString(lacking), "]"))
    }
  }
  unguarded
}

test_that("a test using a suggested package skips when it is absent", {
  expect_identical(unguarded_suggests_blocks(test_path()), character(0))
})

test_that("the guard scan sees direct, exported and helper uses", {
  dir <- withr::local_tempdir()
  writeLines(c(
    "helper_export <- function(x) as_svydesign(x)",
    "test_that(\"direct\", { survey::svymean(~y, d) })",
    "test_that(\"exported\", { as_svrepdesign(s) })",
    "test_that(\"through a helper\", { helper_export(s) })",
    "test_that(\"guarded\", {",
    "  skip_if_not_installed(\"survey\")",
    "  as_svydesign(s)",
    "})",
    "test_that(\"mocked absence\", {",
    "  local_mocked_bindings(check_installed = function(...) NULL)",
    "  as_svydesign(s)",
    "})",
    "test_that(\"svrep implies survey\", {",
    "  skip_if_not_installed(\"svrep\")",
    "  survey::svymean(~y, d)",
    "})"
  ), file.path(dir, "test-probe.R"))
  expect_identical(
    unguarded_suggests_blocks(dir),
    c("test-probe.R: direct [survey]", "test-probe.R: exported [survey]",
      "test-probe.R: through a helper [survey]")
  )
  probe <- file.path(dir, "test-probe.R")
  writeLines(c("skip_if_not_installed(\"survey\")", readLines(probe)), probe)
  expect_identical(unguarded_suggests_blocks(dir), character(0))
})
