# the message for a stage taken whole names its cause

    Code
      invisible(execute(subsample_households(frac = 0.5, method = "systematic",
        round = "nearest"), one_household_pools(), seed = 1))
    Condition
      Warning:
      Stage 3: selected every unit available in the pools it executed.
      x Requested 6 units, selected all 6 available.
      i Every pool holds a single unit, so each was taken whole.
      i If this stage was meant to subsample units an earlier stage selected, execute that stage on its own and start a new design on the result, which records a second phase.
      i This stage contributes no sampling variance. Earlier stages are unaffected: the design as a whole is a census only if every stage is.
    Code
      invisible(execute(draw(stratify_by(sampling_design(), g), frac = 0.2, min_n = 3),
      data.frame(g = rep(1:5, each = 3), id = 1:15), seed = 1))
    Condition
      Warning:
      Stage 1: selected every unit available in the pools it executed.
      x Requested 15 units, selected all 15 available.
      i The fraction is below 1, but rounding it or raising it to `min_n` gave every pool its whole size.
      i This stage contributes no sampling variance. Earlier stages are unaffected: the design as a whole is a census only if every stage is.

# the aggregated messages read as intended

    Code
      invisible(execute(short_psu_design(), short_psu_frame(), seed = 1))
    Condition
      Warning:
      Stage 2: sample size exceeded the pool population in 3 of 6 pools.
      x Requested 60 units, selected 39.
      i Capped pools: "c1", "c2", and "c3".
      i Inspect with `frame_summary(sample, detail = "pool")` and the capped column.
    Code
      invisible(execute(many_pool_design(60), many_pool_frame(n_pools = 60, small = 40),
      seed = 1))
    Condition
      Warning:
      Stage 2: sample size exceeded the pool population in 40 of 60 pools.
      x Requested 600 units, selected 280.
      i Capped pools: "c001", "c002", "c003", "c004", "c005", and 35 more.
      i Inspect with `frame_summary(sample, detail = "pool")` and the capped column.

# the census and nominal-cap messages read as intended

    Code
      invisible(execute(draw(stratify_by(sampling_design(), stratum, alloc = "proportional"),
      n = 20), strat, seed = 1))
    Condition
      Warning:
      Stage 1: selected every unit available in the pools it executed.
      x Requested 20 units, selected all 10 available.
      i Exhausted pools: "A" and "B".
      i This stage contributes no sampling variance. Earlier stages are unaffected: the design as a whole is a census only if every stage is.
    Code
      invisible(execute(draw(add_stage(draw(cluster_by(add_stage(sampling_design(),
      "psu"), psu), n = 2), "unit"), n = 10), data.frame(psu = rep(c("c1", "c2", "c3"),
      each = 4), id = 1:12), seed = 1))
    Condition
      Warning:
      Stage 2: selected every unit available in the pools it executed.
      x Requested 20 units, selected all 8 available.
      i Exhausted pools: "c1" and "c2".
      i This stage contributes no sampling variance. Earlier stages are unaffected: the design as a whole is a census only if every stage is.
    Code
      invisible(execute(draw(sampling_design(), n = 20, method = "bernoulli"),
      data.frame(id = 1:10), seed = 1))
    Condition
      Warning:
      Stage 1: target sample size exceeded the pool population in 1 of 1 pool.
      x Requested 20 units, nominal target capped at 10.
      i This is a random-size design: the realized size can still fall below the capped target.

