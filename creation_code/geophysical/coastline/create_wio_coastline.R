##
##  Name:       create_wio_coastline.R
##
##  Objective:  Create coastline for west indian ocean
##              region
##
##  Approach:   Point to african continent shapefile,
##              import and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-03-11
##

##  Notes:      1. Should modify to extract general open-source
##                 coastline data to apply to other regions

##
## 1. Set up
##
 ## -- call to africa continent -- ##
  # point to data locale
    data_locale <- "data_raw/spatial/shp/Africa_land/"

  # point to data file
    data_file <- "Africa_land.shp"

  # import coastline
    wio_coastline <-
      paste0(data_locale, data_file) %>%
      read_sf()


##
## 2. Groom data
##
  # review object
    wio_coastline
# Simple feature collection with 59 features and 24 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -25.35875 ymin: -46.98138 xmax: 63.50265 ymax: 37.56095
# Geodetic CRS:  WGS 84
# # A tibble: 59 × 25
   # ADM0_CODE ADM0_NAME     CONTINENT ISO3  ISO2    UNI UNDP  FAOSTAT  GAUL
       # <int> <chr>         <chr>     <chr> <chr> <dbl> <chr>   <dbl> <dbl>
 # 1         6 Sudan         Africa    SDN   SD      729 SUD       276     6
 # 2         8 Angola        Africa    AGO   AO       24 ANG         7     8
 # 3        29 Benin         Africa    BEN   BJ      204 BEN        53    29
 # 4        35 Botswana      Africa    BWA   BW       72 BOT        20    35
 # 5        42 Burkina Faso  Africa    BFA   BF      854 BKF       233    42
 # 6        45 Cameroon      Africa    CMR   CM      120 CMR        32    45
 # 7        47 Cape Verde    Africa    CPV   CV      132 CVI        35    47
 # 8        49 Central Afri… Africa    CAF   CF      140 CAF        37    49
 # 9        50 Chad          Africa    TCD   TD      148 CHD        39    50
# 10        58 Comoros       Africa    COM   KM      174 COI        45    58
# # ℹ 49 more rows
# # ℹ 16 more variables: RIC_ISO3 <chr>, REC_ISO3 <chr>, AFR <int>,
# #   CEMAC <int>, CILSS <int>, CRA <int>, ECOWAS <int>, IGAD <int>,
# #   IOC <int>, SADC <int>, CICOS <int>, ICPAC <int>, BDMS <int>,
# #   MOI <int>, Name <chr>, geometry <MULTIPOLYGON [°]>
# # ℹ Use `print(n = ...)` to see more rows


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/geophysical/coastline/"

  # save custom wio ecoregions
    save(wio_coastline,
      file = paste0(save_locale, "wio_coastline.rda"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove core objects
    rm(wio_coastline)

