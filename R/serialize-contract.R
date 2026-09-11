# Native checks for the generated sampling-document contract. The contract is
# installed package data, never supplied by a design file. Only the operations
# emitted by the generator in the source repository are understood here.

#' @noRd
serialization_contract <- local({
  contract <- NULL
  function() {
    if (is_null(contract)) {
      contract <<- jsonlite::fromJSON(
        system.file("schema", "runtime-contract.json", package = "samplyr",
                    mustWork = TRUE), simplifyVector = FALSE
      )
      if (!identical(contract$version, 1L)) {
        stop("Unsupported installed sampling-document contract version.")
      }
    }
    contract
  }
})

# Values come from jsonlite::fromJSON(simplifyVector = FALSE). In particular,
# an empty JSON object is a named list and an empty array is an unnamed list.
#' @noRd
serialization_value_type <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) return(if (is.null(names(x))) "array" else "object")
  if (is.character(x) && length(x) == 1L && !is.na(x)) return("string")
  if (is.logical(x) && length(x) == 1L && !is.na(x)) return("boolean")
  if (is.numeric(x) && length(x) == 1L && is.finite(x)) return("number")
  "invalid"
}

# JSON equality distinguishes strings, booleans and numbers, but not 1 from
# 1.0. Object key order is immaterial, while array order is significant.
#' @noRd
serialization_values_equal <- function(x, y) {
  type <- serialization_value_type(x)
  if (!identical(type, serialization_value_type(y))) return(FALSE)
  if (type == "number") return(x == y)
  if (type == "object") {
    if (!setequal(names(x), names(y))) return(FALSE)
    return(all(vapply(names(x), function(nm) {
      serialization_values_equal(x[[nm]], y[[nm]])
    }, logical(1))))
  }
  if (type == "array") {
    if (length(x) != length(y)) return(FALSE)
    return(all(vapply(seq_along(x), function(i) {
      serialization_values_equal(x[[i]], y[[i]])
    }, logical(1))))
  }
  identical(x, y)
}

#' @noRd
serialization_contract_errors <- function(payload, format = payload$format) {
  contract <- serialization_contract()
  root <- contract$roots[[format]]
  if (is.null(root)) stop("No installed contract for this document format.")
  check_serialization_rules(payload, contract$definitions[[root]],
                            contract$definitions, "")
}

#' @noRd
serialization_pointer <- function(path, key) {
  key <- gsub("~", "~0", key, fixed = TRUE)
  paste0(path, "/", gsub("/", "~1", key, fixed = TRUE))
}

# Returning structured problems lets alternatives be tried without catching
# exceptions. Public error classes and the display limit are applied once at
# the document boundary, including legacy allocation and RNG error classes.
#' @noRd
check_serialization_rules <- function(x, rules, definitions, path) {
  errors <- list()
  problem <- function(message, where = path) list(list(path = where, message = message))
  check <- function(value, rule, where = path) {
    check_serialization_rules(value, rule, definitions, where)
  }
  type <- serialization_value_type(x)
  for (rule in rules) {
    op <- rule$op
    found <- switch(op,
      ref = check(x, definitions[[rule$target]]),
      type = {
        matches <- vapply(rule$values, function(want) {
          identical(type, want) ||
            (want == "integer" && type == "number" && x == floor(x))
        }, logical(1))
        if (!any(matches)) {
          return(problem(paste("Expected", paste(unlist(rule$values), collapse = " or "))))
        }
        list()
      },
      choice = {
        if (any(vapply(rule$values, function(value) {
          serialization_values_equal(x, value)
        }, logical(1)))) list() else problem("Value contradicts the declared contract")
      },
      object = {
        out <- list()
        if (type == "object") {
          missing <- setdiff(unlist(rule$required), names(x))
          for (key in missing) {
            out <- c(out, problem("Required field is missing", serialization_pointer(path, key)))
          }
          if (length(x) < rule$minimum ||
              (!is.null(rule$maximum) && length(x) > rule$maximum)) {
            out <- c(out, problem("Object has an invalid number of fields"))
          }
          for (key in names(x)) {
            where <- serialization_pointer(path, key)
            out <- c(out, check(key, rule$names, where))
            if (key %in% names(rule$fields)) {
              out <- c(out, check(x[[key]], rule$fields[[key]], where))
            } else if (identical(rule$extra, FALSE)) {
              out <- c(out, problem("Unknown executable field", where))
            } else if (is.list(rule$extra)) {
              out <- c(out, check(x[[key]], rule$extra, where))
            }
          }
        }
        out
      },
      array = {
        out <- list()
        if (type == "array") {
          if (length(x) < rule$minimum ||
              (!is.null(rule$maximum) && length(x) > rule$maximum)) {
            out <- c(out, problem("Array has an invalid number of items"))
          }
          for (i in seq_along(x)) {
            where <- paste0(path, "/", i - 1L)
            out <- c(out, check(x[[i]], rule$items, where))
            if (isTRUE(rule$unique) && i > 1L &&
                any(vapply(x[seq_len(i - 1L)], function(y) {
                  serialization_values_equal(x[[i]], y)
                }, logical(1)))) {
              out <- c(out, problem("Array items must be unique", where))
            }
          }
        }
        out
      },
      length = {
        if (type == "string" && nchar(x, type = "chars") < rule$minimum) {
          problem("String is too short")
        } else list()
      },
      minimum = {
        if (type == "number" && x < rule$value) problem("Number is below its minimum") else list()
      },
      above = {
        if (type == "number" && x <= rule$value) problem("Number must exceed its lower bound") else list()
      },
      any = {
        alternatives <- list()
        matched <- FALSE
        for (branch in rule$branches) {
          candidate <- check(x, branch)
          if (!length(candidate)) {
            matched <- TRUE
            break
          }
          alternatives <- c(alternatives, candidate)
        }
        if (matched) list() else alternatives
      },
      all = {
        out <- list()
        for (branch in rule$branches) out <- c(out, check(x, branch))
        out
      },
      not = {
        if (!length(check(x, rule$rule))) problem("Fields violate an exclusion rule") else list()
      },
      when = {
        if (!length(check(x, rule$condition))) check(x, rule$rule) else list()
      },
      stop("Unknown operation in the installed sampling-document contract: ", op)
    )
    errors <- c(errors, found)
  }
  errors
}
