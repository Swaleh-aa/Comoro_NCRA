##
##  Name:       create_regional_coastline.R
##
##  Objective:  Create coastline for region of assessment
##              for Red List of Ecosystems evaluation
##
##  Approach:   Identify ecoregions of interest for
##              region and use univ. of hawaii coastline
##              data from global self-consistent,
##              hierarchical, high-resolution geography (gshhg)
##              data base to extract.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-03-11
##

##  Notes:      1. Coastline available through `rnaturalearth`
##                 lacks resolution. Currently 
##                 downloading gshhg version 2.3.7 data
##                 direct from http.  R packages with
##                 gshhg download functions rely on 
##                 `maptools` which is not available for
##                 R version 4.5.0 [ fs: 2025-05-01 ]
##              2. Should evaluate other sources and R
##                 package functions if available [ fs: 2025-05-01 ]
##

##
## 1. Set up
##
 ## -- call to global ecoregions -- ##
  # point to data locale
    # data_locale <- "data_raw/spatial/shp/marine_ecoregions/"
    data_locale <- "data_raw/spatial/"

  # point to data file
    data_file <- "meow_ecos.shp"

  # import ecoregions
    meow_ecos <-
      paste0(data_locale, data_file) %>%
      read_sf()

 ## -- call to ecoregion list -- ##
  # point to data locale
    data_locale <-  "data_intermediate/spatial/ecoregions/"

  # call to ecoregions
    load(paste0(data_locale, "ecoregion_list.rda"))


 ## -- call to gshhg coastline -- ##
  # point to https locale
    https_locale <-
      paste0("http://www.soest.hawaii.edu/pwessel/gshhg/",
             "gshhg-shp-2.3.7.zip")

  # create temporary file for downloading
    temp_file <- tempfile()

  # set temporary file directory
    # temp_directory <- tempfile()
    temp_directory <- "data_raw/geophysical/coastline/"

  # set timeout
    options(timeout = max(300, getOption("timeout")))

  # download coastline
    https_locale %>% download.file(temp_file)

  # extract files
    temp_file %>%
        unzip(exdir = temp_directory)

  # point to data locale
    data_locale <-
      paste0(temp_directory, "/GSHHS_shp/f/")

 ## -- import coastline polygons -- ##
  # point to data file
    data_file <- "GSHHS_f_L1.shp"

  # import coral reef polygons
    coastline_gshhs <-
      paste0(data_locale, data_file) %>%
      read_sf()

 ## -- remove coastline downloads -- ##
  # remove temp directory
    paste0(temp_directory, "GSHHS_shp") %>%
      fs::dir_delete()

  # remove temp directory
    paste0(temp_directory, "WDBII_shp") %>%
      fs::dir_delete()


##
## 2. Groom data
##
  # have a look
    ecoregion_list
 # [1] "Central Somali Coast"            
 # [2] "Northern Monsoon Current Coast"  
 # [3] "East African Coral Coast"        
 # [4] "Seychelles"                      
 # [5] "Cargados Carajos/Tromelin Island"
 # [6] "Mascarene Islands"               
 # [7] "Southeast Madagascar"            
 # [8] "Western and Northern Madagascar" 
 # [9] "Bight of Sofala/Swamp Coast"     
# [10] "Delagoa"  

  # extract relevant ecoregions
    regional_ecoregions <-
      meow_ecos %>%
        dplyr::filter(ECOREGION %in% ecoregion_list)

 ## -- clip to regional ecoregions -- ##
  # set s2 to false
    sf_use_s2(FALSE)

  # set ecoregional boundary
    regional_coastline <-
      coastline_gshhs %>%
        st_make_valid() %>%
        st_crop(regional_ecoregions) %>%
        st_geometry() %>%
        st_union()
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
  # Loop 0 edge 746 has duplicate near loop 1 edge 4

##
## 3. Generate outputs
##
 ## -- save coastline -- ##
  # point to save locale
    save_locale <- "data_intermediate/geophysical/coastline/"

  # save regional coastline to file
    save(regional_coastline,
      file = paste0(save_locale, "regional_coastline.rda"))


 # ## -- export for ecoregional planning -- ##
  # # point to save locale
    # save_locale <- "data/geophysical/regional_coastline/"
 
  # # save to shape
    # regional_coastline %>%
      # st_write(paste0(save_locale, "regional_coastline.shp"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(meow_ecos,
       ecoregion_list,
       coastline_gshhs,
       regional_ecoregions)

  # remove core objects
    rm(regional_coastline)

