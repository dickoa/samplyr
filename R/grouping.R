#' @noRd
make_group_key <- function(df, vars) {
  if (length(vars) == 0L) {
    return(rep.int("0|", nrow(df)))
  }
  if (length(vars) == 1L) {
    return(as.character(df[[vars]]))
  }

  encode_component <- function(x) {
    missing <- is.na(x)
    value <- enc2utf8(as.character(x))
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
  key_df <- df[, vars, drop = FALSE]
  loc <- vec_group_loc(key_df)

  if (nrow(loc) == 0L) {
    return(list(keys = character(), indices = list()))
  }

  # vec_group_loc returns groups in first-appearance order,
  # so no reordering is needed.
  keys <- make_group_key(loc$key, vars)

  list(
    keys = keys,
    indices = loc$loc
  )
}

#' @noRd
first_row_indices_by_group <- function(indices_list) {
  vapply(indices_list, function(idx) idx[[1L]], integer(1))
}
