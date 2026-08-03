# Shared fixtures for the normalized stage-register work.
#
# A four-school hierarchy held as three separate registers plus the
# equivalent single hierarchical frame. Class and student identifiers are
# deliberately local: "C1" repeats under every school and student_no repeats
# under every class, so any linkage that ignores ancestry is visible.
#
# Row order is fixed and must stay fixed: the golden equivalence test compares
# seeded selections across frame shapes, and frame order drives selection.

mf_schools <- function() {
  data.frame(
    school_id = paste0("S", 1:4),
    school_type = rep(c("Public", "Private"), each = 2),
    enrollment = c(60, 90, 75, 105),
    stringsAsFactors = FALSE
  )
}

mf_classes <- function() {
  schools <- mf_schools()
  data.frame(
    school_id = rep(schools$school_id, each = 2),
    class_id = rep(c("C1", "C2"), times = nrow(schools)),
    stringsAsFactors = FALSE
  )
}

mf_students <- function() {
  classes <- mf_classes()
  data.frame(
    school_id = rep(classes$school_id, each = 3),
    class_id = rep(classes$class_id, each = 3),
    student_no = rep(1:3, times = nrow(classes)),
    stringsAsFactors = FALSE
  )
}

# The same population as one denormalized frame. left_join keeps the student
# row order, unlike merge().
mf_hierarchy <- function() {
  as.data.frame(dplyr::left_join(
    mf_students(), mf_schools(), by = "school_id"
  ))
}

# One school per school_type stratum, one class per school, two students per
# class. Stage weights 2, 2 and 3/2 give a final weight of 6; four students at
# weight 6 reproduce the population count of 24.
mf_design <- function() {
  sampling_design() |>
    add_stage("Schools") |>
    stratify_by(school_type) |>
    cluster_by(school_id) |>
    draw(n = 1) |>
    add_stage("Classes") |>
    cluster_by(class_id) |>
    draw(n = 1) |>
    add_stage("Students") |>
    draw(n = 2)
}

# Ancestry-qualified identity of the selected elements.
mf_keys <- function(x) paste(x$school_id, x$class_id, x$student_no)

# A class register that omits S4 entirely. Seed 1 selects S4 and realizes the
# gap; seed 2 selects S1 and S3 and leaves it a candidate-only gap.
mf_classes_without_s4 <- function() {
  classes <- mf_classes()
  classes[classes$school_id != "S4", , drop = FALSE]
}
