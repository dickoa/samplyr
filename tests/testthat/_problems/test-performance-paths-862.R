# Extracted from test-performance-paths.R:862

# test -------------------------------------------------------------------------
local_key_loc_cache()
frame <- data.frame(m = c(1, 1, 2, 2, 3), h = c("a", "a", "b", "b", "c"))
expect_identical(unique_key_loc(frame, c("m", "h")), c(1L, 3L, 5L))
narrow <- vctrs::new_data_frame(.subset(frame, "m"), n = nrow(frame))
expect_identical(unique_key_loc(narrow, "m"), c(1L, 3L, 5L))
changed <- frame
changed$m[2] <- 9
expect_identical(unique_key_loc(changed, c("m", "h")), c(1L, 2L, 3L, 5L))
