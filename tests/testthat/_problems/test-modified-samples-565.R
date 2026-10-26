# Extracted from test-modified-samples.R:565

# test -------------------------------------------------------------------------
record <- attr(fix_srs, "metadata")$integrity
expect_named(record, c("n_rows", "cols", "hash", "col_hashes"))
