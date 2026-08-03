# the capping message names the strata and the units moved

    Code
      invisible(execute(draw(stratify_by(sampling_design(), h, variance = variance,
      alloc = "neyman"), n = 300), frame, seed = 1))
    Message
      Allocation capped at the stratum population in 1 of 3 strata.
      i Capped strata: "A".
      i 17 units redistributed across the remaining strata.

