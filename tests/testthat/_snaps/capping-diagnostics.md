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

