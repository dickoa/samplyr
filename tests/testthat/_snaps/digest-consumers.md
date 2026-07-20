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
      
      i n = 24 of 120 | stages = 2/2 | seed = 8
      
      -- Stage 1: Clusters -----------------------------------------------------------
      * pps_brewer, MOS mos, cluster cluster, by stratum
      * 4 strata: N_h 6, n_h 2, f_h 0.3333
      
      -- Stage 2: Units --------------------------------------------------------------
      * srswor
      * 8/24 pools: N_h 5, n_h 3, f_h 0.6000
      
      -- Weights ---------------------------------------------------------------------
      * Mean 4.46 [3.1, 6.39] | CV 0.27 | DEFF 1.07 | n_eff 22
      

# summary shows replicate ranges for random-size designs

    Code
      summary(r)
    Output
      -- Sample Summary --------------------------------------------------------------
      
      i n = 43 of 120 | stages = 1/1 | seed = 3 | reps = 3 | n/rep ~ 14
      
      -- Stage 1 ---------------------------------------------------------------------
      * bernoulli, by stratum
      * 4 strata: N_h 30, n_h 1-7 across replicates, n_expected 12
      
      -- Weights ---------------------------------------------------------------------
      i Weight diagnostics omitted for stacked replicates. Filter to one first: x |> filter(.replicate == 1)
      

