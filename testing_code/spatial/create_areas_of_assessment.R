##
##  Name:       create_areas_of_assessment.R
##
##  Objective:  Create data object for areas of assessment
##              for Comoros region
##
##  Approach:   Import data table with site coordinates &
##              coastline to link to islands and create
##              areas of assessment.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Swaleh Aboud
##              CORDIO East Africa
##
##  Date:       2025-06-03
##

##  Notes:      1. Need to link with coastline object [ fs: 2025-06-03 ]
##

##
## 1. Set up
##
 ## -- call to coastline -- ##
  # point to data locale
    data_locale <- "data_intermediate/geophysical/coastline/"

  # load coastline
    load(paste0(data_locale, "regional_coastline.rda"))


 ## -- use sites based on fish sites -- ##
  # point to data locale
    data_locale <- "data_raw/"

  # set data file name
    data_file <- "Benthic_Comoros_site_summary_leve1_all-years.csv"

  # call to data
    fish_sites <-
      paste0(data_locale, data_file) %>%
      read_csv()


##
## 2. Groom data
##
  # have a look
    fish_sites
# # A tibble: 1,343 × 20
   # Country Year      Sector Location Site              Reef_type
   # <chr>   <chr>     <chr>  <chr>    <chr>             <chr>    
 # 1 Kenya   1987/1988 Kenya  WIO_ALG  Bamburi           <NA>     
 # 2 Kenya   1987/1988 Kenya  WIO_ALG  Leven             <NA>     
 # 3 Kenya   1987/1988 Kenya  WIO_ALG  Mpunguti          <NA>     
 # 4 Kenya   1987/1988 Kenya  WIO_ALG  Uyombo            <NA>     
 # 5 Kenya   1993      Kenya  WIO_ALG  Kisite Marine Na… <NA>     
 # 6 Kenya   1993      Kenya  WIO_ALG  Kisite Marine Na… <NA>     
 # 7 Kenya   1993      Kenya  WIO_ALG  Kisite Marine Na… <NA>     
 # 8 Kenya   1993      Kenya  WIO_ALG  Kisite Marine Na… <NA>     
 # 9 Kenya   1994      Kenya  WIO_ALG  Kisite Marine Na… <NA>     
# 10 Kenya   1994      Kenya  WIO_ALG  Kisite Marine Na… <NA>     
# # ℹ 1,333 more rows
# # ℹ 14 more variables: Management_level <chr>,
# #   Management_type <chr>, Depth..m. <dbl>, Latitude <dbl>,
# #   Longitude <dbl>, number_replicates <dbl>, Method <chr>,
# #   Observer <chr>, level1_code <chr>, mean_cover <dbl>,
# #   sd <dbl>, se <dbl>, Organization <chr>, Source <chr>
# # ℹ Use `print(n = ...)` to see more rows

  # get sectors
    fish_sites %>% pull(Sector) %>% unique()
# [1] "Kenya"          "Tanzania"       "Grand Comore"  
# [4] "Moheli island"  "Sychelles"      "Anjouan Island"

 ## -- create spatial object -- ##
  # create sector list
    sectors_of_interest <-
      c("Grand Comore",
        "Moheli island",
        "Anjouan Island")

  # create spatial object
    fish_sites_comoros <-
      fish_sites %>%
        dplyr::filter(Sector %in% sectors_of_interest) %>%
        dplyr::filter(!Longitude %>% is.na(),
                      !Latitude  %>% is.na()) %>%
        dplyr::select(Site,
                      Year,
                      Longitude,
                      Latitude) %>%
        st_as_sf(coords = c("Longitude", "Latitude"),
                 crs = 4326)

 ## -- link to coastline for areas of assessment -- ##
  # create object
    # areas_of_assessment <-



##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/spatial/"

  # save ecoregions to file
    save(areas_of_assessment,
      file = paste0(save_locale, "areas_of_assessment.rda"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(fish_sites,
       sectors_of_interest,
       fish_sites_comoros)

  # remove core objects
    rm(areas_of_assessment)

