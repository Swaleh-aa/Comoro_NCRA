##
##  Name:       create_criterion_d_biotic_disruption_algae-cover-method-b.R
##
##  Objective:  Standardise & format data for analysing criterion D:
##                using coral cover method b
##
##  Approach:   Method a uses average hard coral cover for each 
##                Ecoregion to calculate relative severity.
##
##              Import raw data from gcrmn regional surveys from 
##                the Western Indian Ocean, groom and
##                apply criteria from collapse value:
##
##              Output saved as *.rda
##
##
##  Authors:    Swaleh Aboud
##              CORDIO East Africa
##
##  Date:       2025-05-23
##

##  Notes:      1. Suspect there is an error with the baseline sd
##                   for coral cover.  Should test with recalculation. [ fs: 2024-04-30 ]
##              2. Need to evaluate relative severity corrections and
##                   threat assignments with mishal's update    [fs: 2024-05-01 ]


##
## 1. Set up
##
 ## -- call to criterion d data table -- ##
load("creation_code/Exploration/Crit_D_MaCoR_data_table.RDA")
load("creation_code/Exploration/Criterion_D_hard_coral_baseline_comoros.RDA")


##
## 2. Groom data
## 
  # review criterion d data table

      MaCoR_current <- MaCoR_current %>%
        mutate(ecoregion = recode(ecoregion,
                            "Moheli island" = "Moheli Island",
                            "Grand Comore" = "Grand Comore Island"))
      # Add National value
      # Create new rows with ecoregion changed to "National"
      national_rows <- MaCoR_current %>%
        mutate(ecoregion = "National")
      
      # Combine original data with national rows
      MaCoR_current <- bind_rows(MaCoR_current, national_rows)
      
      MaCoR_current
    # # A tibble: 52 × 11
    # # Groups:   ecoregion, Location [13]
    # ecoregion      Location Site   first_year recent_year no_years recent_cover
    # <chr>          <chr>    <chr>  <chr>      <chr>          <int>        <dbl>
    #   1 Anjouan Island Domoni   Bamba… 2017       2017               1         23.8
    # 2 Anjouan Island Moya     Mpoun… 2021       2024               4         54.0
    # 3 Anjouan Island Moya     Mutsa… 2021       2023               3         40.8
    # 4 Anjouan Island Moya     Nkoha… 2021       2023               3         71.8
    # 5 Anjouan Island Moya     Plage… 2021       2024               4         57.1
    # 6 Anjouan Island Ouani    Ouani… 2015       2017               2         64.8
    # 7 Anjouan Island Pomoni   Dzind… 2017       2024               8         45.4
    # 8 Anjouan Island Pomoni   Hamar… 2018       2024               7         24.3
    # 9 Anjouan Island Pomoni   Hamar… 2017       2024               8         52.0
    # 10 Anjouan Island Pomoni   Mabam… 2017       2023               7         35.0
    # # ℹ 42 more rows
    # # ℹ 4 more variables: current_cover <dbl>, lat <chr>, long <dbl>,
    # #   threshold_cover <dbl>
    # # ℹ Use `print(n = ...)` to see more rows

  # review of hard coral baseline data
    # Select for HC
    # Baseline
    baseline_MaCoR_selected <- 
      baseline_comoros %>%
      filter(area %in% c("regional")) %>%
      filter(group %in% c("MaCoR")) %>%
      dplyr::select(ecoregion,
                    group, 
                    area, 
                    baseline_cover = mean,
                    baseline_sd = sd)
    
    baseline_MaCoR_selected
    # A tibble: 4 × 5
    # ecoregion           group area     baseline_cover baseline_sd
    # <chr>               <chr> <chr>             <dbl>       <dbl>
    # 1 Anjouan Island      MaCoR regional            0.2         0.1
    # 2 Grand Comore Island MaCoR regional            0.2         0.1
    # 3 Moheli Island       MaCoR regional            0.2         0.1
    # 4 National            MaCoR regional            0.2         0.1

##
## 3. Evaluate criterion
##
 # set iteration levels
    i_min <- 10
    i_max <- 1e3

  # set iteration interval
    i_interval <- 10

  # create empty object to hold results
    criterion_d_biotic_disruption_MaCoR_method_b <- tibble()

  # loop through iterations # i=10  ## -- for testing -- ##
        for(i in seq(from = i_min,
                 to   = i_max,
                 by   = i_interval)){
  
     ## -- calculate severity -- ##
      # set seed for reproducibility
        set.seed(i + 81)

      # randomly assign baseline values
          dat <- MaCoR_current %>%
            dplyr::mutate(Ecoregion = trimws(tolower(ecoregion))) %>%
            dplyr::left_join(
              baseline_MaCoR_selected %>%
                dplyr::mutate(Ecoregion = trimws(tolower(ecoregion))) %>%
                dplyr::select(Ecoregion, baseline_cover, baseline_sd),
              by = "Ecoregion"
            ) %>%
           group_by(Ecoregion) %>%
           mutate(baseline_cover = rnorm(1, mean = baseline_cover,
                                               sd = baseline_sd))

      # calculate relative severity
        dat %<>%
          mutate(relative_severity = 100 * (baseline_cover - current_cover) /
                                           (baseline_cover - threshold_cover))

      # bound by 0 and 100
        dat %<>%
          mutate(relative_severity = relative_severity %>% scales::rescale(to = c(0, 100)))

     ## -- determine extent -- ##
      # get proportion of stations for relative severity classes
        dat2 <- dat %>%
          group_by(Ecoregion) %>%
            summarise(rel_sev_30 = 100 * sum(relative_severity >= 30 & 
                                             relative_severity < 50) /  
                                               length(relative_severity),
                      rel_sev_50 = 100 * sum(relative_severity >= 50 &  
                                             relative_severity < 80) / 
                                               length(relative_severity),
                      rel_sev_80 = 100 * sum(relative_severity >= 80 &
                                             relative_severity <= 100) / 
                                               length(relative_severity))
# # A tibble: 3 × 4
  # Ecoregion           rel_sev_30 rel_sev_50 rel_sev_80
  # <chr>                    <dbl>      <dbl>      <dbl>
  # 1 anjouan island            13.6       31.8      22.7 
  # 2 grand comore island       13.3       33.3      40   
  # 3 moheli island             13.3       33.3      6.67 


 ## -- correction from mishal 2024-04-04 -- ##
  # need to re-evaluate from updated script
# # correct rel severity levels
# t_coral2$rel_30 <- rowSums(t_coral2[, c("rel_sev_30", "rel_sev_50", "rel_sev_80")])
# t_coral2$rel_50 <- rowSums(t_coral2[, c("rel_sev_50", "rel_sev_80")])

      # correct rel severity levels
        dat2 %>%
          mutate(rel_30 = (rel_sev_30 + rel_sev_50 + rel_sev_80),
                 rel_50 = (rel_sev_50 + rel_sev_80),
                 rel_80 = (rel_sev_80))
# # A tibble: 3 × 6
  # Ecoregion           rel_sev_30 rel_sev_50 rel_sev_80 rel_30 rel_50
  # <chr>                    <dbl>      <dbl>      <dbl>  <dbl>  <dbl>
  # 1 anjouan island            13.6       31.8      22.7    68.2   54.5
  # 2 grand comore island       13.3       33.3      40      86.7   73.3
  # 3 moheli island             13.3       33.3      6.67    53.3   40  


     ## -- assign threat status -- ##
      # set status
        dat3 <- dat2 %>%
          mutate(status_30 = ifelse(rel_sev_30 >= 80 & rel_sev_30 <= 100, 2,        NA),
                 status_50 = ifelse(rel_sev_50 >= 80 & rel_sev_50 <= 100, 3,        NA),
                 status_50 = ifelse(rel_sev_50 >= 50 & rel_sev_50 < 80,   2, status_50),
                 status_80 = ifelse(rel_sev_80 >= 50 & rel_sev_80 < 80,   3,        NA),
                 status_80 = ifelse(rel_sev_80 >= 80 & rel_sev_80 <= 100, 4, status_80),  
                 status_80 = ifelse(rel_sev_80 >= 30 & rel_sev_80 < 50,   2, status_80))

      # set nas to 1
        dat3 %<>%
          mutate(status_30 = ifelse(is.na(status_30), 1, status_30),
                 status_50 = ifelse(is.na(status_50), 1, status_50),
                 status_80 = ifelse(is.na(status_80), 1, status_80))

     ## -- pick most severe categories -- ##
      # set max from status categories
        dat3 %<>%
         mutate(max_threat = pmax(status_30, 
                                  status_50, 
                                  status_80))

      # create conversion object for threat values
        threat_conversions <-
          tribble(~threat_value, ~status,
                              # 0,    "LC",
                              # 1,    "NT",
                              1, "NT/LC",
                              2,    "VU",
                              3,    "EN",
                              4,    "CR",
                              5,    "CO")

      # convert threat values
        dat3 %<>%
          left_join(threat_conversions %>%
                      rename(max_threat = threat_value))


      # set iteration
        dat3 %<>%
          mutate(Iteration = i)

      # harvest results
        criterion_d_biotic_disruption_MaCoR_method_b %<>%
          bind_rows(dat3)


      }


##
## 4. Review results
##
  # summarise
    criterion_d_biotic_disruption_MaCoR_method_b %>%
      group_by(Ecoregion,
               status) %>%
      summarise(n_categories = n()) %>%
      mutate(percent = 100 * n_categories / sum(n_categories))
    # `summarise()` has grouped output by 'Ecoregion'. You can override using the
    # `.groups` argument.
    # # A tibble: 4 × 4
    # # Groups:   Ecoregion [3]
    # Ecoregion           status n_categories percent
    # <chr>               <chr>         <int>   <dbl>
    #   1 anjouan island      NT/LC           100     100
    # 2 grand comore island NT/LC             6       6
    # 3 grand comore island VU               94      94
    # 4 moheli island       NT/LC           100     100
    
    # Summary for ecoregion - criteria D
    criterion_d_biotic_disruption_MaCoR <- criterion_d_biotic_disruption_MaCoR_method_b %>%
      filter(Iteration == 1000) %>%
      distinct()
    
    criterion_d_biotic_disruption_MaCoR <- criterion_d_biotic_disruption_MaCoR %>%
      mutate(Ecoregion = str_to_title(Ecoregion))
    
    # Summary for sites - RS
    #Capitalise ecoregion naming for first letters
    dat <- dat %>%
      mutate(Ecoregion = str_to_title(Ecoregion))
    
    # Add RS values status
    dat <- dat %>%
      mutate(
        RS_status = case_when(
          relative_severity < 30 ~ "NT/LC",  # Green
          relative_severity >= 30 & relative_severity < 50 ~ "VU",  # Yellow
          relative_severity >= 50 & relative_severity < 80 ~ "EN",  # Orange
          relative_severity >= 80 & relative_severity <= 100 ~ "CR",  # Red
          TRUE ~ NA_character_
        )
      )
    
    # add ecoregion criteria D results
    dat <- dat %>%
      dplyr::left_join(
        criterion_d_biotic_disruption_MaCoR %>%
          dplyr::select(Ecoregion, status) %>%
          dplyr::rename(status_ecoregion = status),
        by = "Ecoregion"
      )
    
    # correct coordinates
    geofile <- read.csv("../Ecological-Research-Data-Analysis/1. Data processing/Template files/Overall_Geo-file_Updated.csv")
    
    anti_join(dat, geofile, by = c("Site" = "Proposed_Site")) %>%
      distinct(Site)
    
    # Correct issue
    dat <- dat %>%
      mutate(Site = recode(Site,
                           "Memboimboini" = "Memboiboini",
                           "Mitsamiuli"   = "Mitsamiouli"))
    
    dat <- dat %>%
      dplyr::left_join(
        geofile %>%
          dplyr::select(Proposed_Site, Latitude_old, Longitude_old),
        by = c("Site" = "Proposed_Site")
      ) %>%
      dplyr::mutate(
        lat = Latitude_old,
        long = Longitude_old
      ) %>%
      dplyr::select(-Latitude_old, -Longitude_old)
    
    # add missing
    dat <- dat %>%
      dplyr::mutate(
        lat = ifelse(Site == "Itsoundzou", -11.873187, lat),
        long = ifelse(Site == "Itsoundzou", 43.38665, long)
      )
    
    
    

##
## 4. Generate outputs
##
  
  # save to file
    save(criterion_d_biotic_disruption_MaCoR,
         file = "data_intermediate/Criteria_D_RS/criterion_d_biotic_disruption_MaCoR.rda")
    
    save(dat,
         file = "data_intermediate/Criteria_D_RS/RS_biotic_disruption_MaCoR.rda")



