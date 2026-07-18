# tbl_sum shows one coverage line

    Code
      print(tbl_sum(s))
    Output
                   A tbl_sample                  Sampling                   Weights 
                      "24 × 13" "2 stages | 24/120 units"        "4.46 [3.1, 6.39]" 

# summary prints the realization report from the digest

    Code
      summary(s)
    Output
      -- Sample Summary --------------------------------------------------------------
      
      i n = 24 | stages = 2/2 | seed = 8
      
      -- Design: Clusters ------------------------------------------------------------
      * Strata: stratum
      * Cluster: cluster
      * Method: pps_brewer
      * MOS: mos
      
      -- Design: Units ---------------------------------------------------------------
      * Method: srswor
      
      -- Allocation: Clusters --------------------------------------------------------
        stratum  N_h  n_h  f_h   
        A        6    2    0.3333
        B        6    2    0.3333
        C        6    2    0.3333
        D        6    2    0.3333
                 ───  ───  ──────
        Total    24   8    0.3333
      i Inclusion probabilities are unit-specific.
      
      -- Allocation: Units -----------------------------------------------------------
      i Universe: 24 pools, 120 units (16 pools resolved from the design, not reached by this realization).
        cluster  N_h  n_h  f_h   
        cl01     5    3    0.6000
        cl03     5    3    0.6000
        cl09     5    3    0.6000
        cl11     5    3    0.6000
        cl15     5    3    0.6000
        cl16     5    3    0.6000
        cl19     5    3    0.6000
        cl23     5    3    0.6000
                 ───  ───  ──────
        Total    40   24   0.6000
      
      -- Weights ---------------------------------------------------------------------
      * Range: [3.1, 6.39]
      * Mean:  4.46 · CV: 0.27
      * DEFF:  1.07 · n_eff: 22
      

# summary shows replicate ranges for random-size designs

    Code
      summary(r)
    Output
      -- Sample Summary --------------------------------------------------------------
      
      i n = 43 | stages = 1/1 | seed = 3 | reps = 3 | n/rep ~ 14
      
      -- Design: Stage 1 -------------------------------------------------------------
      * Strata: stratum
      * Method: bernoulli
      
      -- Allocation: Stage 1 ---------------------------------------------------------
      i Realized sizes vary by replicate. Ranges shown.
        stratum  N_h  expected  n_h
        A        30   3         1-6
        B        30   3         2-4
        C        30   3         4-7
        D        30   3         1-2
      
      -- Weights ---------------------------------------------------------------------
      i Weight diagnostics omitted for stacked replicated sample.
      i Filter to one replicate: x |> filter(.replicate == 1)
      

