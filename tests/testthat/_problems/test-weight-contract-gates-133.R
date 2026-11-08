# Extracted from test-weight-contract-gates.R:133

# prequel ----------------------------------------------------------------------
tbl_sample_verdicts <- c(
  "[" = "preserve",
  "names<-" = "preserve",
  "as_tbl_sample" = "preserve",
  "dplyr_col_modify" = "preserve",
  "dplyr_reconstruct" = "preserve",
  "dplyr_row_slice" = "preserve",
  "filter" = "preserve",
  "group_by" = "preserve",
  "ungroup" = "preserve",
  "vec_restore" = "preserve",
  "summary" = "support",
  "tbl_sum" = "support",
  "design_effect" = "support",
  "effective_n" = "support",
  "as_svydesign" = "support",
  "as_survey_design" = "support",
  "as_svrepdesign" = "support",
  "as_survey_rep" = "support",
  "varcomp" = "refuse"
)

# test -------------------------------------------------------------------------
master <- occasion_share_master()
occasion <- execute(master, period = 1)
targets <- data.frame(
    person_id = paste0("p", seq_len(nrow(master))),
    hh_id = paste0("h", seq_len(nrow(master))),
    stringsAsFactors = FALSE
  )
links <- data.frame(
    ea_id = master$ea_id,
    person_id = paste0("p", seq_len(nrow(master))),
    stringsAsFactors = FALSE
  )
share <- function(x) {
    share_weights(
      x,
      targets = targets, links = links,
      by = c(ea_id = "ea_id"), to = c(person_id = "person_id"),
      within = hh_id, multiplicity = complete_links()
    )
  }
