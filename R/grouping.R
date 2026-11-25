#' Text of one key column's values, independent of getOption("scipen")
#'
#' A whole double up to 2^53 is written in plain digits, as the integer and
#' 64-bit integer of the same value are, so 100000 is "100000" whatever its
#' storage. Other doubles keep 15 significant digits. A value its class
#' declares missing, such as a code of haven's labelled_spss, is missing,
#' and NaN stays a value apart from NA, as vctrs keeps it. Keys built in two
#' calls, or from two frames, then agree.
#' @noRd
key_value_text <- function(x) {
  if (!is.numeric(x) || inherits(x, "integer64")) {
    out <- as.character(x)
    # A class can declare a value missing that as.character() still writes.
    if (is.object(x)) {
      out[is.na(x)] <- NA_character_
    }
    return(out)
  }
  v <- as.double(vctrs::vec_data(x))
  v[v == 0] <- 0
  whole <- is.finite(v) & v == trunc(v) & abs(v) <= 2^53
  out <- sprintf("%.15g", v)
  out[whole] <- sprintf("%.0f", v[whole])
  out[is.na(x) & !is.nan(v)] <- NA_character_
  out
}

#' The strata each label of a numeric stratum column can name
#'
#' A label written for a numeric stratum, by a user or by svyplan, comes from
#' `as.character()`, which writes 100000 as "1e+05" or "100000" depending on
#' `scipen`. A label equal to a stratum's key text names that stratum. Any
#' other label is read as one of the two spellings R can write, and names
#' every stratum written that way. The scientific form keeps 15 significant
#' digits, so it can name several strata: 1000000000000001 and
#' 1000000000000002 are both written "1e+15".
#'
#' A label that names one stratum becomes its key text. A label that names
#' none is returned unchanged, so a misspelled label is still reported as
#' one, and so is every label of a non-numeric or 64-bit integer column.
#' @param labels The labels, as character or factor.
#' @param values The stratum column the labels name.
#' A key text can also be the spelling R writes for another stratum, as
#' "9.00719925474099e+15" is both the key of 2^53 + 2 and the scientific form
#' of 2^53 - 1. It names its own stratum, and `contested` keeps the others,
#' so a reader that sees only some of the strata, such as one parent's, can
#' read the label there.
#' @return A list with `labels`, the labels resolved where they name one
#'   stratum, `ambiguous`, one element per label holding the key texts of
#'   the strata it names when they are several, and `NULL` otherwise, and
#'   `contested`, one element per label resolved to its own key holding
#'   that key and the other strata it spells, and `NULL` otherwise.
#' @noRd
stratum_label_candidates <- function(labels, values) {
  unresolved <- list(
    labels = labels,
    ambiguous = vector("list", length(labels)),
    contested = vector("list", length(labels))
  )
  if (!is.numeric(values) || inherits(values, "integer64") ||
      !(is.character(labels) || is.factor(labels))) {
    return(unresolved)
  }
  labels <- as.character(labels)
  unresolved$labels <- labels
  v <- vctrs::vec_unique(values)
  v <- vctrs::vec_slice(v, !is.na(v))
  if (length(v) == 0L) {
    return(unresolved)
  }
  num <- as.double(vctrs::vec_data(v))
  key <- key_value_text(v)
  # R takes scipen from -9 to 9999, and the two ends give the two forms.
  fixed <- withr::with_options(list(scipen = 9999), as.character(num))
  sci <- withr::with_options(list(scipen = -9), as.character(num))

  # A key text names its own stratum, whatever another stratum is written as.
  exact <- match(labels, key)
  resolved <- labels
  resolved[!is.na(exact)] <- key[exact[!is.na(exact)]]
  pending <- which(is.na(exact) & !is.na(labels))
  ambiguous <- vector("list", length(labels))
  contested <- vector("list", length(labels))
  # Each spelling with the strata it names, each stratum counted once.
  spelled <- vctrs::vec_unique(
    data.frame(text = c(fixed, sci), key = c(key, key))
  )
  named <- split(spelled$key, factor(spelled$text, unique(spelled$text)))
  if (length(pending) > 0L) {
    candidates <- unname(named[labels[pending]])
    count <- lengths(candidates)
    one <- count == 1L
    resolved[pending[one]] <- unlist(candidates[one], use.names = FALSE)
    several <- which(count > 1L)
    ambiguous[pending[several]] <- candidates[several]
  }
  own <- which(!is.na(exact))
  if (length(own) > 0L) {
    others <- lapply(own, function(i) {
      setdiff(named[[labels[i]]] %||% character(0), resolved[i])
    })
    spells <- which(lengths(others) > 0L)
    contested[own[spells]] <- Map(c, resolved[own[spells]], others[spells])
  }
  list(labels = resolved, ambiguous = ambiguous, contested = contested)
}

#' Whether each label names the stratum of each key
#'
#' The labels are read as `stratum_label_candidates()` reads them against
#' the stratum column `values`, so "1e+05" names the stratum 100000 whatever
#' `scipen` is now, and compared with `keys`, the key text of the stratum
#' each label should name.
#' @noRd
labels_name_strata <- function(labels, values, keys = key_value_text(values)) {
  resolved <- stratum_label_candidates(labels, values)$labels
  vctrs::vec_equal(as.character(resolved), keys, na_equal = TRUE)
}

#' A label of a numeric stratum, written as that stratum's key text
#'
#' Resolves the labels as `stratum_label_candidates()` does, and refuses a
#' label that names several strata rather than choosing one.
#' @param var The stratum column, for the refusal.
#' @noRd
resolve_stratum_labels <- function(labels, values, var = NULL,
                                   call = caller_env()) {
  resolved <- stratum_label_candidates(labels, values)
  several <- which(lengths(resolved$ambiguous) > 0L)
  if (length(several) > 0L) {
    i <- several[1L]
    abort_ambiguous_stratum_label(
      labels[[i]], resolved$ambiguous[[i]], var, call = call
    )
  }
  resolved$labels
}

#' The stratum columns of a table, resolved against the frame's columns
#'
#' Each column is resolved as `resolve_stratum_labels()` does. A label that
#' names several strata of its column is read with the rest of its row: the
#' row is kept when exactly one stratum of the frame has those values in
#' every column, and refused when several have. A row that names no stratum
#' of the frame is left as written, for the caller's rules on unknown strata
#' or on rows of a parent the stage did not select.
#' @param scope The parent of each row of `frame`, or NULL for one scope. A
#'   later stage draws within each parent, so a row whose strata lie in
#'   different parents, at most one in each, names one stratum wherever it is
#'   read. Such a row is left as written, and marked in the `"per_scope"`
#'   attribute of the result.
#' @param by_column `TRUE` to keep the column-wise resolution of a row that
#'   names no stratum of the frame, as the preflight does to tell which
#'   parent such a row is for. By default the row is left as written, since
#'   a column resolved alone can make a row of another parent look like a
#'   row of a stratum here.
#' @noRd
resolve_stratum_columns <- function(table, frame, vars, scope = NULL,
                                    by_column = FALSE, call = caller_env()) {
  vars <- intersect(vars, intersect(names(table), names(frame)))
  per_scope <- logical(nrow(table))
  done <- function() {
    if (is_null(scope)) table else structure(table, per_scope = per_scope)
  }
  if (length(vars) == 0L || nrow(table) == 0L) {
    return(done())
  }
  resolved <- lapply(vars, function(v) {
    stratum_label_candidates(table[[v]], frame[[v]])
  })
  names(resolved) <- vars
  written <- table
  for (v in vars) {
    table[[v]] <- resolved[[v]]$labels
  }
  open <- Reduce(`|`, lapply(resolved, function(r) {
    lengths(r$ambiguous) > 0L | lengths(r$contested) > 0L
  }))
  if (length(vars) > 1L && !by_column) {
    # A row names a stratum only by its whole tuple.
    keys <- function(df) {
      make_group_key(as.data.frame(lapply(df[vars], key_value_text)), vars)
    }
    alien <- !open & !keys(table) %in% keys(frame)
    for (v in vars) {
      if (any(alien) && is.character(table[[v]])) {
        table[[v]][alien] <- as.character(written[[v]])[alien]
      }
    }
  }
  if (!any(open)) {
    return(done())
  }

  # The strata of the frame, as the key text of each column.
  texts <- as.data.frame(lapply(frame[vars], key_value_text),
                         stringsAsFactors = FALSE)
  strata <- vctrs::vec_unique(texts)
  # Each parent with the strata its rows hold, once.
  held <- if (is_null(scope)) {
    data.frame(scope = 1L, stratum = seq_len(nrow(strata)))
  } else {
    vctrs::vec_unique(data.frame(
      scope = scope, stratum = vctrs::vec_match(texts, strata)
    ))
  }
  for (r in which(open)) {
    in_row <- rep(TRUE, nrow(strata))
    for (v in vars) {
      allowed <- resolved[[v]]$ambiguous[[r]] %||%
        resolved[[v]]$contested[[r]] %||%
        key_value_text(table[[v]][r])
      in_row <- in_row & strata[[v]] %in% allowed
    }
    # Within each parent a key text names its own stratum where that
    # stratum is, and the stratum it spells otherwise. A stratum the row
    # names exactly in more of its columns wins, whatever the column order.
    own <- Filter(function(v) length(resolved[[v]]$contested[[r]]) > 0L, vars)
    prefer <- function(idx) {
      if (length(own) == 0L || length(idx) < 2L) {
        return(idx)
      }
      exact <- vapply(own, function(v) {
        strata[[v]][idx] == resolved[[v]]$labels[r]
      }, logical(length(idx)))
      exact <- matrix(exact, nrow = length(idx))
      beaten <- vapply(seq_along(idx), function(i) {
        any(vapply(seq_along(idx), function(j) {
          all(exact[j, ] >= exact[i, ]) && any(exact[j, ] > exact[i, ])
        }, logical(1)))
      }, logical(1))
      idx[!beaten]
    }
    within <- held[held$stratum %in% which(in_row), , drop = FALSE]
    picks <- lapply(split(within$stratum, within$scope), prefer)
    crowded <- Find(function(idx) length(idx) > 1L, picks)
    if (!is_null(crowded)) {
      found <- strata[crowded, , drop = FALSE]
      if (length(vars) > 1L) {
        abort_ambiguous_stratum_key(
          paste(vapply(vars, function(v) as.character(written[[v]][r]), ""),
                collapse = "/"),
          do.call(paste, c(unname(as.list(found)), sep = "/")),
          contested = any(vapply(vars, function(v) {
            length(resolved[[v]]$contested[[r]]) > 0L
          }, logical(1))),
          call = call
        )
      }
      shared <- Filter(
        function(v) length(unique(found[[v]])) > 1L &&
          (length(resolved[[v]]$ambiguous[[r]]) > 0L ||
             length(resolved[[v]]$contested[[r]]) > 0L),
        vars
      )[1L]
      abort_ambiguous_stratum_label(
        as.character(table[[shared]][r]), unique(found[[shared]]), shared,
        call = call
      )
    }
    chosen <- unique(unlist(picks, use.names = FALSE))
    if (length(chosen) == 1L) {
      for (v in vars) {
        if (length(resolved[[v]]$ambiguous[[r]]) > 0L ||
              length(resolved[[v]]$contested[[r]]) > 0L) {
          table[[v]][r] <- strata[[v]][chosen]
        }
      }
    } else if (length(chosen) > 1L) {
      # Different parents read the row as different strata.
      per_scope[r] <- TRUE
    }
  }
  done()
}

#' Does each row of a table name at least one stratum of a frame?
#'
#' Read as `resolve_stratum_columns()` reads a row, without refusing a row
#' that names several.
#' @noRd
stratum_rows_named <- function(table, frame, vars) {
  if (nrow(table) == 0L) {
    return(logical(0))
  }
  resolved <- lapply(vars, function(v) {
    stratum_label_candidates(table[[v]], frame[[v]])
  })
  strata <- vctrs::vec_unique(
    as.data.frame(lapply(frame[vars], key_value_text), stringsAsFactors = FALSE)
  )
  vapply(seq_len(nrow(table)), function(r) {
    in_row <- rep(TRUE, nrow(strata))
    for (k in seq_along(vars)) {
      allowed <- resolved[[k]]$ambiguous[[r]] %||%
        resolved[[k]]$contested[[r]] %||%
        key_value_text(resolved[[k]]$labels[r])
      in_row <- in_row & strata[[vars[k]]] %in% allowed
    }
    any(in_row)
  }, logical(1))
}

#' The rows of a stage's stratum tables each parent can read
#'
#' A later stage reads its size, threshold and allocation tables within each
#' parent. A table keyed by the parent's identifier holds rows for every
#' parent, and reading it whole in each one costs the number of parents times
#' the number of rows. The parent columns are resolved once over the stage's
#' frame, and a parent then reads the rows that can name it: those whose
#' parent labels name it, name several parents, or are missing, which the
#' reader still refuses.
#' @param ancestors The parent columns of the stage.
#' @return A function of a parent's rows that returns `draw` and `strata`,
#'   the specs with their tables narrowed.
#' @noRd
parent_table_reader <- function(draw_spec, strata_spec, frame, ancestors) {
  fields <- list(
    draw = c("n", "frac", "certainty_size", "certainty_prop", "min_n",
             "max_n"),
    strata = c("variance", "cost", "cv", "importance")
  )
  plans <- list()
  for (where in names(fields)) {
    spec <- if (where == "draw") draw_spec else strata_spec
    for (field in fields[[where]]) {
      table <- spec[[field]]
      if (!is.data.frame(table)) next
      parent_vars <- intersect(intersect(ancestors, names(table)),
                               names(frame))
      if (length(parent_vars) == 0L) next
      # The key text a row's label names, or NA where it can name several.
      named <- lapply(parent_vars, function(v) {
        found <- stratum_label_candidates(table[[v]], frame[[v]])
        text <- key_value_text(found$labels)
        text[lengths(found$ambiguous) > 0L |
               lengths(found$contested) > 0L] <- NA_character_
        text
      })
      plans[[length(plans) + 1L]] <- list(
        where = where, field = field, vars = parent_vars, named = named
      )
    }
  }
  function(rows) {
    out <- list(draw = draw_spec, strata = strata_spec)
    for (plan in plans) {
      keep <- rep(TRUE, length(plan$named[[1L]]))
      for (k in seq_along(plan$vars)) {
        here <- key_value_text(vctrs::vec_unique(rows[[plan$vars[k]]]))
        keep <- keep & (is.na(plan$named[[k]]) | plan$named[[k]] %in% here)
      }
      table <- out[[plan$where]][[plan$field]]
      out[[plan$where]][[plan$field]] <- table[keep, , drop = FALSE]
    }
    out
  }
}

#' Refuse a label that names several strata
#' @noRd
abort_ambiguous_stratum_label <- function(label, candidates, var = NULL,
                                          call = caller_env()) {
  column <- if (is_null(var)) {
    "its column"
  } else {
    cli::format_inline("{.field {var}}")
  }
  abort_samplyr(
    c(
      "Stratum label {.val {label}} names more than one stratum of {column}.",
      "x" = "R writes {format_pool_sample(candidates)} as {.val {label}}.",
      "i" = "Write each stratum as its value in full, such as
             {.val {candidates[1]}}."
    ),
    class = "samplyr_error_stratum_label_ambiguous",
    call = call
  )
}

#' Refuse a row of several stratum columns that names several strata
#'
#' @param label The row's values as written, joined by "/".
#' @param candidates The whole keys of the strata it names.
#' @param contested `TRUE` when a value is one stratum's key text and another
#'   stratum's spelling, which no text can write more fully.
#' @noRd
abort_ambiguous_stratum_key <- function(label, candidates, contested,
                                        call = caller_env()) {
  abort_samplyr(
    c(
      "Stratum key {.val {label}} names more than one stratum.",
      "x" = "It can name {format_pool_sample(candidates)}.",
      "i" = if (contested) {
        "Give the stratum columns as numbers, which name each stratum
         exactly."
      } else {
        "Write each value of the key in full, such as
         {.val {candidates[1]}}, or give the columns as numbers."
      }
    ),
    class = "samplyr_error_stratum_label_ambiguous",
    call = call
  )
}

#' @noRd
make_group_key <- function(df, vars, call = caller_env()) {
  if (length(vars) == 0L) {
    return(rep.int("0|", nrow(df)))
  }
  for (v in vars) {
    check_key_precision(df[[v]], v, call = call)
  }
  if (length(vars) == 1L) {
    return(key_value_text(df[[vars]]))
  }

  encode_component <- function(x) {
    value <- key_value_text(x)
    # NaN is a value apart from NA, as in a single-column key.
    missing <- is.na(value)
    # Numbers and logicals print in ASCII. Anything else may not.
    if (!is.numeric(x) && !is.logical(x)) {
      value <- enc2utf8(value)
    }
    value[missing] <- ""
    encoded <- paste0(
      "V",
      nchar(value, type = "bytes"),
      ":",
      value
    )
    encoded[missing] <- "N"
    encoded
  }

  components <- lapply(vars, function(v) encode_component(df[[v]]))
  key <- paste0(length(vars), "|")
  for (component in components) {
    key <- paste0(key, component)
  }
  key
}

#' Render a length-prefixed compound key for diagnostics
#' @noRd
display_path_key <- function(key) {
  invalid_key <- function(bytes) {
    if (is.null(bytes)) {
      return("<invalid key NA>")
    }
    paste0("<invalid key 0x", paste(format(bytes), collapse = ""), ">")
  }
  escape_component <- function(value) {
    value <- encodeString(value, quote = "")
    gsub("/", "\\/", value, fixed = TRUE)
  }
  decode_one <- function(value) {
    if (is.na(value)) {
      return(invalid_key(NULL))
    }
    bytes <- charToRaw(enc2utf8(value))
    bar <- which(bytes == as.raw(0x7c))[1]
    if (is.na(bar) || bar == 1L) {
      return(invalid_key(bytes))
    }
    n_text <- rawToChar(bytes[seq_len(bar - 1L)])
    if (!grepl("^[0-9]+$", n_text)) {
      return(invalid_key(bytes))
    }
    n_components <- suppressWarnings(as.double(n_text))
    if (
      !is.finite(n_components) || n_components != floor(n_components) ||
        n_components > length(bytes) + 1L
    ) {
      return(invalid_key(bytes))
    }

    components <- character(n_components)
    pos <- bar + 1L
    for (i in seq_len(n_components)) {
      if (pos > length(bytes)) {
        return(invalid_key(bytes))
      }
      marker <- bytes[[pos]]
      if (marker == as.raw(0x4e)) {
        components[[i]] <- "NA"
        pos <- pos + 1L
        next
      }
      if (marker != as.raw(0x56)) {
        return(invalid_key(bytes))
      }

      pos <- pos + 1L
      if (pos > length(bytes)) {
        return(invalid_key(bytes))
      }
      colon_rel <- which(bytes[pos:length(bytes)] == as.raw(0x3a))[1]
      if (is.na(colon_rel) || colon_rel == 1L) {
        return(invalid_key(bytes))
      }
      colon <- pos + colon_rel - 1L
      length_text <- rawToChar(bytes[pos:(colon - 1L)])
      if (!grepl("^[0-9]+$", length_text)) {
        return(invalid_key(bytes))
      }
      n_bytes <- suppressWarnings(as.double(length_text))
      value_start <- colon + 1L
      available <- length(bytes) - value_start + 1L
      if (
        !is.finite(n_bytes) || n_bytes != floor(n_bytes) ||
          n_bytes < 0 || n_bytes > available
      ) {
        return(invalid_key(bytes))
      }
      component_bytes <- if (n_bytes == 0) {
        raw(0)
      } else {
        bytes[value_start:(value_start + n_bytes - 1L)]
      }
      component <- rawToChar(component_bytes)
      if (!validUTF8(component)) {
        return(invalid_key(bytes))
      }
      Encoding(component) <- "UTF-8"
      components[[i]] <- escape_component(component)
      pos <- value_start + n_bytes
    }
    if (pos != length(bytes) + 1L) {
      return(invalid_key(bytes))
    }
    paste(components, collapse = "/")
  }

  vapply(key, decode_one, character(1), USE.NAMES = FALSE)
}

#' Collision-free integer identifiers for compound rows
#' @noRd
group_ids <- function(df, vars) {
  if (nrow(df) == 0L) {
    return(integer())
  }
  if (length(vars) == 0L) {
    return(rep.int(1L, nrow(df)))
  }

  loc <- vec_group_loc(df[, vars, drop = FALSE])
  out <- integer(nrow(df))
  out[unlist(loc$loc, use.names = FALSE)] <- rep.int(
    seq_len(nrow(loc$key)),
    lengths(loc$loc)
  )
  out
}

#' @noRd
split_row_indices <- function(df, vars) {
  rlang::local_error_call(caller_env())
  key_df <- df[, vars, drop = FALSE]
  loc <- vec_group_loc(key_df)

  if (nrow(loc) == 0L) {
    return(list(keys = character(), key_df = loc$key, indices = list()))
  }

  # `vec_group_loc()` preserves first-appearance order.
  keys <- make_group_key(loc$key, vars)

  # Keep encoded identity separate from display values.
  list(
    keys = keys,
    key_df = loc$key,
    indices = loc$loc
  )
}

#' @noRd
first_row_indices_by_group <- function(indices_list) {
  if (length(indices_list) == 0L) {
    return(integer(0))
  }
  sizes <- lengths(indices_list)
  if (any(sizes == 0L)) {
    cli_abort(
      "Internal error: a row group has no rows.",
      call = NULL,
      class = "samplyr_error_internal"
    )
  }
  rows <- unlist(indices_list, use.names = FALSE)
  first <- rows[cumsum(c(1L, sizes[-length(sizes)]))]
  names(first) <- names(indices_list)
  first
}

#' Two distinct values of a grouping column that have the same key text
#'
#' Group keys are built with `key_value_text()`, which keeps 15 significant
#' digits of a fractional or very large double and writes a date or a
#' difftime as it prints, so two values that differ beyond that would merge
#' two strata or clusters. Every double-backed column is checked, on its
#' distinct values, which is cheap. Missing values are not a clash.
#' Returns the first two such values as text, or `NULL`.
#' @noRd
key_precision_clash <- function(x, loc = NULL) {
  if (!is.double(x) || inherits(x, "integer64")) {
    return(NULL)
  }
  values <- if (is_null(loc)) vctrs::vec_unique(x) else vctrs::vec_slice(x, loc)
  text <- key_value_text(values)
  dup <- duplicated(text, incomparables = NA_character_)
  if (!any(dup)) {
    return(NULL)
  }
  merged <- values[which(text == text[dup][1])]
  sprintf("%.17g", vctrs::vec_data(merged)[1:2])
}

#' Refuse a numeric grouping column whose distinct values print alike
#' @noRd
check_key_precision <- function(x, var, call = caller_env(), loc = NULL) {
  shown <- key_precision_clash(x, loc)
  if (is_null(shown)) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.field {var}} has distinct values that would have the same grouping
       key.",
      "x" = "{.val {shown[1]}} and {.val {shown[2]}} would be read as one
             group.",
      "i" = "Round {.field {var}}, or store its values as integer or
             character ids."
    ),
    class = c("samplyr_error_key_precision", "samplyr_error_frame_invalid"),
    call = call
  )
}

#' First-row locations of key columns, shared within one execution
#'
#' Execution deduplicates the same strata and cluster columns of a frame in
#' several checks, each over every row. `local_key_loc_cache()` keeps, for
#' the rest of the calling frame, the locations each column set was found
#' at, keyed by the column vectors themselves. A narrowed frame that shares
#' them shares the entry, and the entry holds the vectors, so their memory
#' cannot be reused while it lives. A subset of cached columns is read from
#' the cached rows: its first appearances are among them, in the same order.
#' Inactive, every call deduplicates the frame.
#' @noRd
key_loc_cache <- new.env(parent = emptyenv())

#' @noRd
local_key_loc_cache <- function(frame = caller_env()) {
  previous <- key_loc_cache$entries
  key_loc_cache$entries <- list()
  withr::defer(key_loc_cache$entries <- previous, envir = frame)
}

#' What `vctrs::vec_unique_loc(df[vars])` returns, from the cache if it can
#' @noRd
unique_key_loc <- function(df, vars) {
  # A column named twice keys the rows as once, and a cached set of the same
  # length is then the same set.
  vars <- unique(vars)
  cols <- .subset(df, vars)
  n <- nrow(df)
  entries <- key_loc_cache$entries
  if (is_null(entries)) {
    return(vctrs::vec_unique_loc(vctrs::new_data_frame(cols, n = n)))
  }
  for (entry in entries) {
    covers <- entry$n == n && all(vars %in% names(entry$cols)) &&
      all(vapply(
        vars,
        function(v) rlang::is_reference(entry$cols[[v]], cols[[v]]),
        logical(1)
      ))
    if (!covers) {
      next
    }
    if (length(vars) == length(entry$cols)) {
      return(entry$loc)
    }
    rows <- vctrs::new_data_frame(
      lapply(cols, vctrs::vec_slice, entry$loc),
      n = length(entry$loc)
    )
    loc <- entry$loc[vctrs::vec_unique_loc(rows)]
    remember_key_loc(cols, n, loc)
    return(loc)
  }
  loc <- vctrs::vec_unique_loc(vctrs::new_data_frame(cols, n = n))
  remember_key_loc(cols, n, loc)
  loc
}

#' @noRd
remember_key_loc <- function(cols, n, loc) {
  entries <- c(list(list(cols = cols, n = n, loc = loc)), key_loc_cache$entries)
  # A replicated execution links a new frame per replicate. Keep the latest.
  key_loc_cache$entries <- entries[seq_len(min(length(entries), 16L))]
}
