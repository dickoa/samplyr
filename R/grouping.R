#' @noRd
make_group_key <- function(df, vars, call = caller_env()) {
  if (length(vars) == 0L) {
    return(rep.int("0|", nrow(df)))
  }
  for (v in vars) {
    check_key_precision(df[[v]], v, call = call)
  }
  if (length(vars) == 1L) {
    return(as.character(df[[vars]]))
  }

  encode_component <- function(x) {
    missing <- is.na(x)
    value <- as.character(x)
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

#' Two distinct values of a numeric grouping column that print alike
#'
#' Group keys are built with `as.character()`, which keeps 15 significant
#' digits, so two values that differ only beyond them would merge two
#' strata or clusters. Checked on the distinct values, which is cheap.
#' Returns the first two such values as text, or `NULL`.
#' @noRd
key_precision_clash <- function(x, loc = NULL) {
  if (!is.double(x)) {
    return(NULL)
  }
  values <- if (is_null(loc)) vctrs::vec_unique(x) else vctrs::vec_slice(x, loc)
  text <- as.character(values)
  if (!anyDuplicated(text)) {
    return(NULL)
  }
  merged <- values[text == text[duplicated(text)][1]]
  sprintf("%.17g", unclass(merged)[1:2])
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
      "{.field {var}} has distinct values that are equal to 15 significant
       digits.",
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
