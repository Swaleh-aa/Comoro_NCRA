

#   Script Descriptions --------------------------------------------------

##  Name:         Data_preparation_separation_for_initial_current.R
##
##  Objective:    This template outlines the process of cleaning, processing and separating benthic data into 
##                current and initial. 
##                
##  Approach:     1. Import files, 
##                2. Check and correct for missing information,
##                  2a. Select targeted categories - Hard coral and Algae groups (AMAC, ATRF and ALG) 
##                  2b. Check and add missing information - Latitude, Longitude, Site, Location, Sector...
##                3. Formatting output file to match the Global GCRMN 2020 data template.
##                4. Separate the data into 4 files
##                  4a. Only Comoros data
##                  4b. Initial values
##                  4c. Current values
##                  4d. Site information with coordinates
##                5. Saving data and intermediate object
##
##  Output files: 1. Object saved to *.rda. - Next script shall use this for further analysis
##                2. File saved to *.csv - Save clean replicate dataset
##
##  Authors:      Swaleh Aboud
##
##  Date:         22nd Nov. 2024
##

##  Notes:        



# 1. Import files ---------------------------------------------------------

Comoros_ncra <- read.csv("Comoro NCRA/data_raw/Benthic_Comoros_site_summary_leve1_all-years.csv") 



# 2. Check and correct for missing information ----------------------------

# 2a. Select targeted categories - Hard coral and Algae groups... --------

Comoros_ncra <- subset(Comoros_ncra, level1_code %in% c("Hard coral","Macro-algae","Turf algae","ALG","Algae"))



# 2b. Check and add missing information - Latitude, Longitude --------

# Separate non Comoros data initial values which majority are not from Comoros
comoros_data <- Comoros_ncra %>% filter(Country == "Comoros")
non_comoros_data <- Comoros_ncra %>% filter(Country != "Comoros")

# Update information for the fields
# Call Geofile
Geo_file <- read.csv("../Ecological-Research-Data-Analysis/1. Data processing/Template files/Overall_Geo-file_Updated.csv")
Geo_file <- Geo_file[Geo_file$Country == "Comoros", ]

# check site names
h<-match(comoros_data$Site,Geo_file$Proposed_Site,nomatch = 0)
unique(comoros_data$Site[h==0]) # All matches

comoros_data$Site[which(comoros_data$Site=="Shindini")] <- "Chindini"

# Re-add all important fields
comoros_data$Latitude<-Geo_file$Latitude_old[match(comoros_data$Site,Geo_file$Proposed_Site)] # Latitude
comoros_data$Longitude<-Geo_file$Longitude_old[match(comoros_data$Site,Geo_file$Proposed_Site)] # Longitude
comoros_data$Sector<-Geo_file$Proposed_Sector[match(comoros_data$Site,Geo_file$Proposed_Site)] # Sector
comoros_data$Location<-Geo_file$Proposed_Location[match(comoros_data$Site,Geo_file$Proposed_Site)] # Location
comoros_data$Reef_type<-Geo_file$Reef_type[match(comoros_data$Site,Geo_file$Proposed_Site)] # Reef_type
comoros_data$Management_level<-Geo_file$Management_level[match(comoros_data$Site,Geo_file$Proposed_Site)] # Management_level
comoros_data$Management_type<-Geo_file$Management_type[match(comoros_data$Site,Geo_file$Proposed_Site)] # Management_type

# Check missing value
columns_with_missing <- colnames(comoros_data)[apply(comoros_data, 2, function(col) any(is.na(col) | col == ""))]

# Print the column names with NA or blank values
if (length(columns_with_missing) > 0) {
  cat("Columns with NA or blank values:\n")
  print(columns_with_missing)
} else {
  cat("No columns have NA or blank values.\n")
}

# There are still some issues with missing values for - Reef type, Latitude and Longitude

# Test for correctness of coordinates
library(dplyr)

# Define bounds
latitude_min <- -12.37
latitude_max <- -11.35
longitude_min <- 43.22
longitude_max <- 44.54

# Filter rows with coordinates outside the bounds
invalid_coords <- comoros_data %>%
  filter(Latitude < latitude_min | Latitude > latitude_max |
           Longitude < longitude_min | Longitude > longitude_max)

# View rows with invalid coordinates
print(invalid_coords)



# 4. Separate the data into 4 files ---------------------------------------


# 4a. Only Comoros data ---------------------------------------------------

comoros_data

# 4b. Initial values ------------------------------------------------------

initial_all_alg <- read_excel("Comoro NCRA/data_raw/Algae-coral ratio_LATEST_280421.xlsx", 
                          sheet = "data") 

initial_all_hc <- read_excel("Comoro NCRA/data_raw/HC data_Regional-LATEST_270421.xlsx", 
                         sheet = "Regional") 

# Filter rows to be included as initial values

initial_all_alg <- initial_all_alg[initial_all_alg$`Include/Exclude` == "yes", ] #Algae

initial_all_alg <- initial_all_alg[rowSums(is.na(initial_all_alg)) < ncol(initial_all_alg), ] # Delete NA rows Algae
initial_all_alg$Country[which(initial_all_alg$`data source/owner`=="Obura - BSc thesis 1989")] <- "Kenya"
initial_all_alg$Country[which(initial_all_alg$`data source/owner`=="Obura - Phd thesis 1995")] <- "Kenya"

initial_all_hc <- initial_all_hc[initial_all_hc$`Include/Exclude` == "yes", ] #Hard coral

initial_all_hc <- initial_all_hc[rowSums(is.na(initial_all_hc)) < ncol(initial_all_hc), ] # Delete NA rows Hard coral

# Select specific columns by name
# Algae file
initial_alg <- initial_all_alg %>%
  dplyr::select(`data source/owner`, Country, Year, Sector, Site, 
         Station, `Benthic category`, `Benthic code`, all)
names(initial_alg)[names(initial_alg)=="all"] <- "Alg"

initial_alg <- initial_alg[!is.na(initial_alg$Alg), ]
initial_alg <- initial_alg[initial_alg$`Benthic code` != "HC", ]


initial_alg_hc <- initial_all_alg %>%
  dplyr::select(`data source/owner`, Country, Year, Sector, Site, 
                Station, `Benthic category`, `Benthic code`, HC)

initial_alg_hc <- initial_alg_hc[!is.na(initial_alg_hc$HC), ]


# HC file
initial_all_hc <- initial_all_hc %>%
  dplyr::select(`data source/owner`, Country, Year, Sector, Site, 
                Station, `Benthic category`, `Benthic code`, `mean cover (%)`)

initial_all_hc <- initial_all_hc[!is.na(initial_all_hc$`mean cover (%)`), ]

initial_hc <- initial_all_hc[initial_all_hc$`Benthic code` == "HC", ]
initial_hc_alg <- initial_all_hc[initial_all_hc$`Benthic code` == "AMAC", ]

# Merge for complete initial values
# HC
names(initial_hc)[names(initial_hc)=="mean cover (%)"] <- "HC"
initial_hc_regional <- rbind(initial_hc,initial_alg_hc)

# ALG
names(initial_hc_alg)[names(initial_hc_alg)=="mean cover (%)"] <- "Alg"
initial_alg_regional <- rbind(initial_alg,initial_hc_alg)

# Combined
initial_alg_regional2 <- initial_alg_regional
names(initial_alg_regional2)[names(initial_alg_regional2)=="Alg"] <- "mean_cover"

initial_hc_regional2 <- initial_hc_regional
names(initial_hc_regional2)[names(initial_hc_regional2)=="HC"] <- "mean_cover"

initial_values_regional <- rbind(initial_hc_regional2, initial_alg_regional2)

initial_values_regional <- initial_values_regional %>%
  arrange(Year, Country)


# 4c. Current values ------------------------------------------------------

# Select for 2015-2024
comoros_current <- Comoros_ncra %>% filter(Year >= 2014 & Year <= 2024)

# Subset where level1_code is "Hard coral"
current_HC <- comoros_current[comoros_current$level1_code == "Hard coral", ]

# Subset where level1_code is "ALG", "Macro-algae", or "Turf algae"
current_ALG <- comoros_current[comoros_current$level1_code %in% c("ALG", "Macro-algae", "Turf algae"), ]


# 4d. Site information with coordinates -----------------------------------

# Subset columns
site_info <- comoros_data[, c("Country", "Sector", "Location", "Site", "Reef_type", "Management_level", "Management_type", "Latitude", "Longitude")]

# Remove duplicates
site_info <- site_info[!duplicated(site_info), ]




# 5. Saving data and intermediate object ----------------------------------

# Initial values

save(initial_values_regional, file = "Comoro NCRA/data_intermediate/Initial_regional_HC_ALG.RDA")

save(initial_hc_regional, file = "Comoro NCRA/data_intermediate/Initial_regional_HC.RDA")

save(initial_alg_regional, file = "Comoro NCRA/data_intermediate/Initial_regional_ALG.RDA")

# Current value

save(current_HC, file = "Comoro NCRA/data_intermediate/Current_regional_HC_ALG.RDA")

save(current_HC, file = "Comoro NCRA/data_intermediate/Current_regional_HC.RDA")

save(current_ALG, file = "Comoro NCRA/data_intermediate/Current_regional_ALG.RDA")

# Comoros data
save(comoros_data, file = "Comoro NCRA/data_intermediate/Complete_comoros_data.RDA")









