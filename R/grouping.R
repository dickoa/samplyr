#' @noRd
make_group_key <- function(df, vars) {
  if (length(vars) == 1L) {
    return(as.character(df[[vars]]))
  }

  if (length(vars) == 2L) {
    return(paste0(
      as.character(df[[vars[[1L]]]]),
      "\x01",
      as.character(df[[vars[[2L]]]])
    ))
  }

  key <- as.character(df[[vars[[1L]]]])
  for (v in vars[-1L]) {
    key <- paste(key, as.character(df[[v]]), sep = "\x01")
  }
  key
}

#' @noRd
split_row_indices <- function(df, vars) {
  grouped <- group_by(df, across(all_of(vars)))
  indices <- dplyr::group_rows(grouped)

  if (length(indices) == 0L) {
    return(list(keys = character(), indices = list()))
  }

  key_df <- dplyr::group_keys(grouped)
  keys <- make_group_key(key_df, vars)

  # Keep first-appearance group order for deterministic downstream behavior.
  first_rows <- first_row_indices_by_group(indices)
  ord <- order(first_rows)

  list(
    keys = keys[ord],
    indices = indices[ord]
  )
}

#' @noRd
first_row_indices_by_group <- function(indices_list) {
  vapply(indices_list, function(idx) idx[[1L]], integer(1))
}
