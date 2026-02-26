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
