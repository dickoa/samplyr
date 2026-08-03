# the shortfall message reads as intended

    Code
      invisible(execute(poisson_design(n = 40), skewed_pps_frame(), seed = 1))
    Condition
      Warning:
      Stage 1: PPS Poisson expected sample size fell short of the reachable target in 1 pool.
      x Reachable 40 units, expected 6.9.
      x 3 units have an inclusion probability clipped at 1.
      i Handle dominant units explicitly with `certainty_size` or `certainty_prop`.
      i See `?selection-methods` for the "pps_poisson" contract.

