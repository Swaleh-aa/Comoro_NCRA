##
##  Name:       create_criterion_d_biotic_disruption_key_fish_consumers.R
##
##  Objective:  Standardise & format data for analysing criterion D:
##                key fish consumers
##
##  Approach:   Import fish data for Western Indian Ocean monitoring,
##                groom and evaluate criterion with the following
##                steps:
##               1. Randomly baseline value of coral cover for
##                    each Ecoregion from range
##               2. Calculate relative severity for each Ecoregion
##               3. For each station: calculate the relative severity:
##                    relative severity = 100 * (baseline - current) /
##                                         (baseline - threshold)
##               4. Determine the extent for each Ecoregion and
##                    calculate the proportion of sites with relative
##                    severity above 30, 50 , 80
##               5. Assign max threat category from each iteration
##                    for each Ecoregion for the following:
##                  extent >= 80% & relative_severity >= 80% -> (CR)
##                  extent >= 80% & relative_severity >= 50% -> (EN)
##                  extent >= 80% & relative_severity >= 30% -> (VU)
##                  extent >= 50% & relative_severity >= 80% -> (EN)
##                  extent >= 50% & relative_severity >= 50% -> (VU)
##                  extent >= 30% & relative_severity >= 80% -> (VU)
##
##                For each iteration, the max threat status for each Ecoregion
##                  stored.
##                Runs for different number of iterations are used to
##                  determine at what number of iterations the results stabilise
##
##              Output saved as *.rda
##
##
##  Authors:    Swaleh Aboud
##              CORDIO East Africa
##
##  Date:       2025-05-24
##

##  Notes:      1. Only evaluating two of the threshold models:
##                   ref max and model max
##              2. Need to validate the threshold_ser_model_max &
##                   threshold_ser_ref_max values    [ fs: 2024-04-29 ]

##
## 1. Set up
##
 ## -- call to fish baseline reference min & max -- ##

  load("data_intermediate/Criteria_D_fish_baseline_ref.rda")
  load("data_intermediate/Criteria_D_fish_data_table.rda")

    ##
## 2. Groom data
##
  # review reference data
  baseline_comoros <- baseline_comoros %>%
    dplyr::mutate(fish = dplyr::recode(fish,
                           "Scarinae" = "Parrotfish",
                           "Epinephelidae" = "Groupers"))
      
    baseline_comoros
    # # A tibble: 8 × 4
    # eco_rgn             fish       baseline_mean baseline_sd
    # <chr>               <fct>              <dbl>       <dbl>
    # 1 Moheli Island       Parrotfish          334.        307.
    # 2 Moheli Island       Groupers            139.        141.
    # 3 Grand Comore Island Parrotfish          334.        307.
    # 4 Grand Comore Island Groupers            139.        141.
    # 5 Anjoun Island       Parrotfish          334.        307.
    # 6 Anjoun Island       Groupers            139.        141.
    # 7 National            Parrotfish          334.        307.
    # 8 National            Groupers            139.        141.

  # review data table
    fish_current_values
    # # A tibble: 85 × 9
    # eco_rgn site_id             first_year recent_year current_biom recent_biom
    # <fct>   <chr>                    <dbl>       <dbl>        <dbl>       <dbl>
    #   1 Mwali   Comoros_Mwali_Mohe…       2016        2016         74          74  
    # 2 Mwali   Comoros_Mwali_Mohe…       2016        2016         12          12  
    # 3 Mwali   Comoros_Mwali_Mohe…       2016        2016         44          44  
    # 4 Mwali   Comoros_Mwali_Mohe…       2016        2016         64          64  
    # 5 Mwali   Comoros_Mwali_Mohe…       2016        2016         20          20  
    # 6 Mwali   Comoros_Mwali_Mohe…       2016        2016         38          38  
    # 7 Mwali   Comoros_Mwali_Mohe…       2016        2018        167.        292. 
    # 8 Mwali   Comoros_Mwali_Mohe…       2016        2018         45.8        71.5
    # 9 Mwali   Comoros_Mwali_Mohe…       2016        2016         86          86  
    # 10 Mwali   Comoros_Mwali_Mohe…       2016        2016         13          13  
    # # ℹ 75 more rows
    # # ℹ 3 more variables: threshold_ref_max <dbl>, threshold_ref_min <lgl>,
    # #   fish <fct>
    # # ℹ Use `print(n = ...)` to see more rows

  # review data table
    fish_current_values
# # A tibble: 170 × 9
   # eco_rgn       site_id     first_year recent_year current_biom
   # <chr>         <chr>            <dbl>       <dbl>        <dbl>
 # 1 Moheli Island Comoros_Mo…       2016        2016         74  
 # 2 Moheli Island Comoros_Mo…       2016        2016         12  
 # 3 Moheli Island Comoros_Mo…       2016        2016         44  
 # 4 Moheli Island Comoros_Mo…       2016        2016         64  
 # 5 Moheli Island Comoros_Mo…       2016        2016         20  
 # 6 Moheli Island Comoros_Mo…       2016        2016         38  
 # 7 Moheli Island Comoros_Mo…       2016        2018        167. 
 # 8 Moheli Island Comoros_Mo…       2016        2018         45.8
 # 9 Moheli Island Comoros_Mo…       2016        2016         86  
# 10 Moheli Island Comoros_Mo…       2016        2016         13  
# # ℹ 160 more rows
# # ℹ 4 more variables: recent_biom <dbl>,
# #   threshold_ref_max <dbl>, threshold_ref_min <dbl>,
# #   fish <fct>
# # ℹ Use `print(n = ...)` to see more rows


##
## 3. Evaluate criterion
##
  # set iteration levels
    i_min <- 1
    i_max <- 750

  # set iteration interval
    i_interval <- 1

 ## -- Loop repeats four times to allow for the         ##
 ##      calculations of relative severity using        ##
 ##      the four different threshold values. Each      ##
 ##      time it calls the appropriate baseline file    ##
 ##      based on the threshold column used.            ##
 ##    The fish data table contains both the parrot     ##
 ##      and grouper data, and fish is used as a        ##
 ##      grouping variable to ensure that they can      ##
 ##      be analysed independently.                  -- ##

  # create empty object to hold results
    criterion_d_biotic_disruption_key_fish_consumers <- tibble()

  # set seed for reproducibility
    set.seed(66)

  # loop through iterations # i=1  ## -- for testing -- ##
    for(i in seq(from = i_min,
                 to   = i_max,
                 by   = i_interval)){

     ## -- first with baseline model -- ##
      # randomly assign baseline values
        baseline_bio <-
          baseline_comoros %>%
            dplyr::filter(!baseline_mean %>% is.na()) %>%
           group_by(eco_rgn) %>%
           mutate(baseline_random = rnorm(1, mean = baseline_mean,
                                               sd = baseline_sd))

      # set negative values to zero
        baseline_bio %<>%
          mutate(baseline_random = ifelse(baseline_random < 0, 0, baseline_random))

           ## -- harvest results -- ##
      # bind
        baseline_bio <- fish_current_values %>%
          dplyr::left_join(
            baseline_bio %>%
              dplyr::ungroup() %>%
              dplyr::select(eco_rgn, fish, baseline_mean, baseline_sd, baseline_random),
            by = c("eco_rgn", "fish")
          )

      # calculate rel severity max
        baseline_bio_max <-
          baseline_bio %>%
            mutate(relative_severity = 100 * (baseline_random - current_biom) /
                                       (baseline_random - threshold_ref_max)) %>%
            mutate(Method = "ref_max")

      # calculate rel severity min
        baseline_bio_min <-
          baseline_bio %>%
            mutate(relative_severity = 100 * (baseline_random - current_biom) /
                                   (baseline_random - threshold_ref_min)) %>%
            mutate(Method = "ref_min")


     ## -- combine objects -- ##
        baseline_bio <- baseline_bio %>%
          mutate(
            relative_severity = NA_real_,
            Method = NA_character_
          )
      # link
        baseline_bio <- baseline_bio %>%
          bind_rows(baseline_bio_max, baseline_bio_min)
        
        baseline_bio <- baseline_bio %>%
          filter(Method %in% c("ref_max", "ref_min"))

      # bound by 0 and 100
        baseline_bio %<>%
          mutate(relative_severity = relative_severity %>% scales::rescale(to = c(0, 100)))
        
        
        # set iteration & method
        baseline_bio %<>%
          mutate(Iteration = i)

     ## -- determine severity status -- ##
      # set proportion of stations in each category of relative severity
        baseline_bio %<>%
          rename(Ecoregion = eco_rgn) %>%
          group_by(Ecoregion,
                   fish,
                   Method,
                   Iteration) %>%
          summarise(rel_sev_30 = 100 * sum(relative_severity >= 30 &
                                          relative_severity <= 50) /  length(relative_severity),
                    rel_sev_50 = 100 * sum(relative_severity >= 50 &
                                          relative_severity <= 80) /  length(relative_severity),
                    rel_sev_80 = 100 * sum(relative_severity >= 80 &
                                          relative_severity <= 100) / length(relative_severity))

     ## -- assign threat status -- ##
      # set status
        baseline_bio %<>%
          mutate(status_30 = ifelse(rel_sev_30 >= 80 & rel_sev_30 <= 100, 2,        NA),
                 status_50 = ifelse(rel_sev_50 >= 80 & rel_sev_50 <= 100, 3,        NA),
                 status_50 = ifelse(rel_sev_50 >= 50 & rel_sev_50 < 80,   2, status_50),
                 status_80 = ifelse(rel_sev_80 >= 50 & rel_sev_80 < 80,   3,        NA),
                 status_80 = ifelse(rel_sev_80 >= 80 & rel_sev_80 <= 100, 4, status_80),
                 status_80 = ifelse(rel_sev_80 >= 30 & rel_sev_80 < 50,   2, status_80),
                 status_80 = ifelse(rel_sev_80 >= 27 & rel_sev_80 < 30,   1, status_80),
                 status_50 = ifelse(rel_sev_50 >= 45 & rel_sev_50 < 50,   1, status_50),
                 status_30 = ifelse(rel_sev_30 >= 72 & rel_sev_30 < 80,   1, status_30))

      # set nas to zero
        baseline_bio %<>%
          mutate(status_30 = ifelse(is.na(status_30), 0, status_30),
                 status_50 = ifelse(is.na(status_50), 0, status_50),
                 status_80 = ifelse(is.na(status_80), 0, status_80))

     ## -- pick most severe categories -- ##
      # set max from status categories
        baseline_bio %<>%
          mutate(max_threat = pmax(status_30, status_50, status_80))

      # create conversion object for threat values
        threat_conversions <-
          tribble(~threat_value, ~status,
                              0,    "LC",
                              1,    "NT",
                              2,    "VU",
                              3,    "EN",
                              4,    "CR",
                              5,    "CO")

      # convert threat values
        baseline_bio %<>%
          left_join(threat_conversions %>%
                      rename(max_threat = threat_value))

     ## -- harvest results -- ##
      # combine
        criterion_d_biotic_disruption_key_fish_consumers %<>%
          bind_rows(baseline_bio)


   }


##
## 4. Review results
##
  # have a look
    criterion_d_biotic_disruption_key_fish_consumers
# # A tibble: 12,000 × 12
   # Ecoregion        fish  Method Iteration rel_sev_30 rel_sev_50
   # <chr>            <fct> <chr>      <dbl>      <dbl>      <dbl>
 # 1 Anjoun Island    Parr… ref_m…         1        0         5.26
 # 2 Anjoun Island    Parr… ref_m…         1        0        15.8 
 # 3 Anjoun Island    Grou… ref_m…         1        0         0   
 # 4 Anjoun Island    Grou… ref_m…         1        0         5.56
 # 5 Grand Comore Is… Parr… ref_m…         1        0        18.2 
 # 6 Grand Comore Is… Parr… ref_m…         1        0        54.5 
 # 7 Grand Comore Is… Grou… ref_m…         1        0        27.3 
 # 8 Grand Comore Is… Grou… ref_m…         1       18.2      27.3 
 # 9 Moheli Island    Parr… ref_m…         1       15.4      53.8 
# 10 Moheli Island    Parr… ref_m…         1       30.8      46.2 
# # ℹ 11,990 more rows
# # ℹ 6 more variables: rel_sev_80 <dbl>, status_30 <dbl>,
# #   status_50 <dbl>, status_80 <dbl>, max_threat <dbl>,
# #   status <chr>
# # ℹ Use `print(n = ...)` to see more rows

  # summarise
    a <- 
    criterion_d_biotic_disruption_key_fish_consumers %>%
      group_by(Ecoregion,
               fish,
               Method,
               # Iteration,
               status) %>%
      summarise(n_categories = n()) %>%
      mutate(percent = 100 * n_categories / sum(n_categories))
# `summarise()` has grouped output by 'Ecoregion', 'fish',
# 'Method'. You can override using the `.groups` argument.
# # A tibble: 122 × 6
# # Groups:   Ecoregion, fish, Method [39]
   # Ecoregion fish    Method  status n_categories percent
   # <chr>     <chr>   <chr>   <chr>         <int>   <dbl>
 # 1 Comoros   grouper ref_max CR              525  70
 # 2 Comoros   grouper ref_max EN                7   0.933
 # 3 Comoros   grouper ref_max LC              211  28.1
 # 4 Comoros   grouper ref_max VU                7   0.933
 # 5 Comoros   grouper ref_min CR              523  69.7
 # 6 Comoros   grouper ref_min EN                7   0.933
 # 7 Comoros   grouper ref_min LC              211  28.1
 # 8 Comoros   grouper ref_min VU                9   1.2
 # 9 Comoros   grouper <NA>    LC              750 100
# 10 Comoros   parrot  ref_max CR              523  69.7
# # ℹ 112 more rows
# # ℹ Use `print(n = ...)` to see more rows


##
## 5. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/criteria/"

  # save to file
    save(criterion_d_biotic_disruption_key_fish_consumers,
      file = paste0(save_locale, "criterion_d_biotic_disruption_key_fish_consumers.rda"))


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(i_min,
       i_max,
       i_interval,
       threat_conversions)

  # remove core objects
    rm(criterion_d_biotic_disruption_key_fish_consumers)

