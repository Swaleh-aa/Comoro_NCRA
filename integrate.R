##
##  Project Name:  National Coral Reef Assessment (NCRA) for Comoros
##
##  Objective:     NCRA analysis for Madagascar using Criteria D and other assessments
##
##  Approach:      
##
##
##  Authors:       Swaleh Aboud, Franz Smith and others
##                 Coastal Oceans Research and Development (CORDIO) East Africa
##
##  Date:          2023-11-22
##

##  Notes:         1. This file is intended to provide a guide to the basic
##                    workflow of the project, attempting to 'integrate' the
##                    different steps necessary to conduct the analyses &
##                    create visual outputs

##
##  1. Set up the core functionality
##
  # clean up
    rm(list=ls())

  # call to core packages for data manipulation
    library(dplyr)
    library(tidyr)
    library(magrittr)
    library(purrr)
    library(lubridate)
    library(hms)
    library(stringr)
    library(forcats)

  # for importing different formats
    library(readr)
    library(readxl)

  # call to visualisation & output generation
    library(ggplot2)
    library(GGally)
    library(Cairo)
    library(extrafont)
    library(RColorBrewer)
    library(viridis)

  # functionality for spatial analyses
    library(raster)
    # library(rgdal)
    library(sf)
    # library(rgeos)

  # point to working directory        ## -- will need to adjust for local copy -- ##
    # setwd("/Users/franzinho/Desktop/research/Coral-Juvenile-Manuscript")

  # # set font for graphical outputs
    # theme_set(theme_bw(base_family = "Helvetica"))
    # CairoFonts(  # slight mod to example in ?CairoFonts page
               # regular    = "Helvetica:style = Regular",
               # bold       = "Helvetica:style = Bold",
               # italic     = "Helvetica:style = Oblique",
               # bolditalic = "Helvetica:style = BoldOblique"
               # )

  # create helper function for reviewing data
    quickview <- function(x, n = 3L) {head(data.frame(x), n = n)}


##
## 2. Generate core data objects
##

 ## -- create spatial objects -- ##
  # point to creation locale
    

  # create site position data
    

  # validate site coordinates
    

  # create location regions
    

##
## 3. Create introductory manuscript figures
##
 ## -- site locations -- ##
  # point to analysis locale
    

  # generate site position figure
    

##
## 4. Perform analyses
##




##
## 9. Clean up workspace
##
  # remove paths
    

