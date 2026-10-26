# Extracted from test-performance-paths.R:844

# test -------------------------------------------------------------------------
frame <- data.frame(
    m = rep(c(3, 1, 2, 1), each = 3),
    h = rep(c("b", "a", "a", "a"), each = 3),
    r = rep(c("x", "y"), 6),
    stringsAsFactors = TRUE
  )
frame$m[5] <- NA
direct <- function(vars) vctrs::vec_unique_loc(frame[vars])
sets <- list("m", "h", c("m", "h"), c("h", "m"), c("m", "h", "r"), "r")
for (vars in sets) {
    expect_identical(unique_key_loc(frame, vars), direct(vars))
  }
for (order in list(sets, rev(sets))) {
    local({
      local_key_loc_cache()
      for (vars in order) {
        expect_identical(unique_key_loc(frame, vars), direct(vars),
                         info = paste(vars, collapse = "+"))
      }
    })
  }
