################################################################
################################################################
#######
####### National Map of Pharmacy Deserts
####### Dec 2022
#######
################################################################
################################################################

## Steps:
## 1. Load packages and pharmacy address data
## 2. Read in shape files for all states
## 3. Geocode pharmacy data
## 4. Load map with pharmacy dots + census tract lines
## 5. Define low income (census variables)
## 6. Define low access
## 7. Define pharmacy deserts in table
## 8. Create map of pharmacy deserts


# 1111111111111111111111111111111111111111111111111111111111111111111111111
# 1111111111111111111111111111111111111111111111111111111111111111111111111
# Load packages
  library(tidycensus) # interface with census API to pull data tables
  library(acs) # also use this to get data from census
  library(tidyverse) # data manipulation
  library(ggmap) # maps using the tidyverse
  library(tigris) # read in tiger shapefiles
  library(ggmap) # use to register google API key for geocoding
  library(leaflet) # can use with open street map and Rshiny to make interactive maps
  library(mapview) # another map-making package
  library(sf) # spatial features package
  library(rgdal) # reading in shapefiles
  library(choroplethr) #making choropleth maps easily
  library(choroplethrMaps) # goes with choroplethr
  library(broom) # updated version of fortify function
  library(rgeos) # use point in polygon function
  library(htmlwidgets) # save interactive leaflet maps as html
  library(shiny) # make interactive html site
  library(RColorBrewer) # for R color scales in ggplot and leaflet
# Install Census and API keys
  # # install census API key (only need to do this once per computer ever)
  # my_api_key <- "083ae7aa6363d62361c694445ce61298cd1c2825"
  # census_api_key(my_api_key, install = TRUE)
  Sys.getenv("CENSUS_API_KEY") # to check if the api key is loaded
  # # install Google API key (new key for this proj vs my old one)
  register_google(key = "AIzaSyCHb9LTmOgEswBpLGgEt594kIua5NQLxKo", write = TRUE)

# Set working drive and files
  rootDir <- "~/OneDrive/Documents/School Stuff/Dissertation/Pharmacy-Deserts-Analysis/"
  inputDir <- paste0(rootDir, "Input/")
  outputDir <- paste0(rootDir, "Output/")
  figuresDir <- paste0(rootDir, "Figures/")
  # shapefilesDir <- paste0(rootDir, "shapefiles/")
  setwd(rootDir)
  # tigris_cache_dir(shapefilesDir)

# Load files needed:
# NCPDP files: provider info and services
  providerinfo_df <- readxl::read_excel(paste0(inputDir,"Provider Information Table.xlsx"))
  services_df <- readxl::read_excel(paste0(inputDir,"Services Information Table.xlsx"))
# RUCA codes from USDA https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes.aspx . 
  # As of Dec 2022 only 2010 RUCA codes are available- check again in a few months
  ruca_df <- readxl::read_excel(paste0(inputDir,"ruca2010revised.xlsx"), sheet = 1, skip = 1) %>% 
    rename(county_FIPS = `State-County FIPS Code`,
           state = `Select State`,
           tract_FIPS = `State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`,
           ruca_1 = `Primary RUCA Code 2010`,
           ruca_2 = `Secondary RUCA Code, 2010 (see errata)`) %>% 
    select(-`Tract Population, 2010`,-`Land Area (square miles), 2010`,-`Population Density (per square mile), 2010`)


# 2222222222222222222222222222222222222222222222222222222222222222222222222
# 2222222222222222222222222222222222222222222222222222222222222222222222222
# Read in shape files. Do a cache = TRUE
# use Tigris to read in shapefiles directly using Census API (no local files needed)
counties_c <- tigris::counties(cb = TRUE, year = 2020)
tract_c <- tigris::tracts(cb = TRUE, year = 2020) # default format is sf but can also set class = "sp" if needed. see help function for gg mapping
groups_c <- tigris::block_groups(cb = TRUE, year = 2020)

# 3333333333333333333333333333333333333333333333333333333333333333333333333
# 3333333333333333333333333333333333333333333333333333333333333333333333333
# # DONT NEED TO DO THESE STEPS AGAIN -------------------------------------

# # Merge NCPDP data tables
#   pharmacy_df <- full_join(providerinfo_df, services_df, by = "NCPDP Provider ID")
# # Drop territories and pharmacies that are not community pharmacies:
#   pharmbystate_df <- pharmacy_df %>% group_by(`Physical Address State`) %>% 
#     dplyr::summarise(n_pharm = n()) %>% 
#     filter(!`Physical Address State` %in% c("PR", "GU", "VI", "MP", "NA")) # removing territories except DC. (PR 1041; GU 24; VI 20; MP 9).
#   table(pharmacy_df$`Primary Provider Type Code`, useNA = "always") # get numbers of pharmacy by type 
#   pharmacy_df <- pharmacy_df %>% filter(`Primary Provider Type Code` == 1) # then filter to just include community pharmacies

# Geocode all community pharmacies in 50 US states:
#   # break into 2 chunks: 
#   # need to be under 40k addresses in order to stay under GoogleAPI geocoding budget limit for the month
#   pharmbystate_df1 <- pharmbystate_df[1:25,] # contains 31510 pharmacies
#   pharmbystate_df2 <- pharmbystate_df[26:52,] %>% filter(!is.na(`Physical Address State`)) #contains 30549 pharmacies
#   # going to start with geocoding the second half alphabetically because includes WA so can compare to previous analysis
#   states1 <- pharmbystate_df1$`Physical Address State`
#   states2 <- pharmbystate_df2$`Physical Address State`
#   pharmacy_df1 <- pharmacy_df %>% filter(`Physical Address State` %in% states1)
#   pharmacy_df2 <- pharmacy_df %>% filter(`Physical Address State` %in% states2)
#   
#   # # concatenate address info into 1 column
#   pharmacy_df2$addresses <- paste(pharmacy_df2$`Physical Address 1`,",",
#                                   pharmacy_df2$`Physical Address City`,",",
#                                   pharmacy_df2$`Physical Address State`,
#                                   pharmacy_df2$`Physical Address ZIP`)
#   pharmacy_df1$addresses <- paste(pharmacy_df1$`Physical Address 1`,",",
#                                   pharmacy_df1$`Physical Address City`,",",
#                                   pharmacy_df1$`Physical Address State`,
#                                   pharmacy_df1$`Physical Address ZIP`)
# 
#   # find lat and long of addresses using geocode and Google API (for 30k addresses will take a while to run)
#   # df2 first chunk of addresses
#     for (i in 1:nrow(pharmacy_df2)) {
#       result <- geocode(pharmacy_df2$addresses[i], output = "latlon", source = "google")
#       pharmacy_df2$lon[i] <- as.numeric(result[1])
#       pharmacy_df2$lat[i] <- as.numeric(result[2])
#     }
#     
#     View(pharmacy_df2)
#     table(pharmacy_df2$`Physical Address State`[is.na(pharmacy_df2$lat)], useNA = "always") # there are 11 out of 30549 that have NA for lat/lon
#     table(pharmacy_df2$`Legal Business Name`[is.na(pharmacy_df2$lon)], useNA = "always") # some of these seem like legit pharmacies- investigate
#   
#   # df1 second chunk of addresses
#     for (i in 1:nrow(pharmacy_df1)) {
#       result <- geocode(pharmacy_df1$addresses[i], output = "latlon", source = "google")
#       pharmacy_df1$lon[i] <- as.numeric(result[1])
#       pharmacy_df1$lat[i] <- as.numeric(result[2])
#     }
#     
#     View(pharmacy_df1)
#     table(pharmacy_df1$`Physical Address State`[is.na(pharmacy_df1$lat)], useNA = "always") # there are 24 out of 38772 that have NA for lat/lon
#     table(pharmacy_df1$`Legal Business Name`[is.na(pharmacy_df1$lon)], useNA = "always") # some of these seem like legit pharmacies- investigate
# 
#   write.csv(pharmacy_df2, file = "pharmacy_df2_geo.csv")
#   write.csv(pharmacy_df1, file = "pharmacy_df1_geo.csv")
#   # read in csv that I saved earlier
#   pharmacy_df1 <- read.csv("pharmacy_df1_geo.csv")[,-1]
#   pharmacy_df2 <- read.csv("pharmacy_df2_geo.csv")[,-1]
#   # check column order:
#   identical(colnames(pharmacy_df1), colnames(pharmacy_df2)) #TRUE
#   pharmgeo_df <- rbind(pharmacy_df1, pharmacy_df2)
#   write.csv(pharmgeo_df, "pharmacy_geo.csv")
  # # ^^ DONT NEED TO DO THESE STEPS AGAIN -------------------------------------
  
  # read in df with geocoding complete: work with this data from now on
  pharmgeo_df <- read.csv("pharmacy_geo.csv") 
  # crosstab columns, rename variables, drop unnecessary variables
  pharmgeo_df2 <- pharmgeo_df %>% 
    filter(Closed.Door.Facility.Indicator %in% "N", # removed 1156 closed door facilities here as well
           Dispenser.Class.Code %in% "7") %>%       # removed 288 alternate dispensing sites as well
    mutate(ID = as.character(ID.x),
           ncpdp_id = as.character(NCPDP.Provider.ID),
           legal_name = as.character(Legal.Business.Name),
           dba_name = as.character(DBA.Name),
           address1 = as.character(Physical.Address.1),
           address2 = as.character(Physical.Address.2),
           city = as.character(Physical.Address.City),
           state = as.character(Physical.Address.State),
           zip = as.character(Physical.Address.ZIP),
           county_fips = as.character(County.FIPS),
           msa = as.character(MSA),
           open24h_bin = as.character(Open.24.Hours),
           lang1 = as.character(Language.Code.1),                   # Language Codes indicate if the language is spoken at the pharmacy.
           lang2 = as.character(Language.Code.2),
           lang3 = as.character(Language.Code.3),
           lang4 = as.character(Language.Code.4),
           lang5 = as.character(Language.Code.5),  # Eventually, make binary # english = , # spanish = , # chinese = ,# arabic = ,# french = , # hindi = , # korean = ,# italian = , # russian = , # vietnamese = 
           dispenserclass_cat = factor(Dispenser.Class.Code,
                                       levels = c("1","2","5","6"),
                                       labels = c("Independent", "Chain", "Franchise","Government")),
           medicare_id = as.character(Medicare.Provider.ID),
           npi_id = as.character(National.Provider.ID..NPI.),
           maildelivery_bin = as.character(Delivery.Service.Indicator),
           maildelivery_cat = factor(Delivery.Service.Code,
                                     levels = c("3", "4", "5"),
                                     labels = c("No Rx Delivery", "Rx Delivery For A Fee", "Free Rx Delivery")),
           compounding_bin = as.character(Compounding.Service.Indicator),
           compounding_cat = factor(Compounding.Service.Code,
                                    levels = c("6", "7", "8", "9", "10"),
                                    labels = c("No compounding services", "Basic non-sterile compounding", "Complex non-sterile compounding",
                                           "Low complexity sterile compounding", "High complexity sterile compounding")),
           driveup_bin = as.character(Driveup.Window.Indicator),
           dme_bin = as.character(DME.Indicator),
           dme_cat = factor(DME.Code,
                            levels = c("13", "14", "15", "16", "17", "18"),
                            labels = c("No DME offered", "DME off-the-shelf, non-custom, unaccredited", "DME full range and custom, unaccredited",
                                   "DME for pharmaceuticals and diabetic testing, accredited", "DME off-the-shelf, non-custom, accredited", 
                                   "DME full range and custom, accredited")),
           walkinclinic_bin = as.character(Walkin.Clinic.Indicator),
           walkinclinic_cat = factor(Walkin.Clinic.Code,
                                     levels = c("19", "20", "21", "22"),
                                     labels = c("No walk-in clinic", "Walk-in clinic with limited services, mid-level professional",
                                            "Walk-in clinic with limited services, licensed physician",
                                            "Onsite emergency room")),
           emerg24h_bin = as.character(X24h.Emergency.Service.Indicator),
           emerg24h_cat = factor(X24h.Emergency.Service.Code,
                                 levels = c("23", "24", "25", "26", "27"),
                                 labels = c("No 24h emergency service", "24h emergency remote pharmacist (call center)", "24h emergency remote pharmacist (local)",
                                            "24h emergency pharmacist with in-person access", "24h in-person emergency pharmacist service")),
           multidosepkg_bin = as.character(Multidose.Compliance.Indicator),
           multidosepkg_cat = factor(Multidose.Compliance.Code,
                                     levels = c("28", "29", "30"),
                                     labels = c("No multidose compliance packaging", "Multidose compliance packaging to assisted living facilities only", 
                                                "Multidose compliance packaging to all")),
           immunizations_bin = as.character(Immunizations.Provided.Indicator),
           immunizations_cat = factor(Immunizations.Provided.Code,
                                      levels = c("31", "32", "33"),
                                      labels = c("No on-site immunizations", "Immunization services at select dates and times",
                                                 "Immunization services during business hours")),
           handicapaccess_bin = as.character(Handicapped.Accessible.Indicator),
           is340b_bin = as.character(X340B.Status.Indicator),
           is340b_cat = factor(X340B.Status.Code,
                               levels = c("36", "37", "38", "39"),
                               labels = c("No 340B relationships", "Not owned by 340B entity but contracts to covered entities",
                                          "Owned by 340B entity but also serves non-eligible patients", 
                                          "Owned by 340B entity and only serves eligible patients"))) %>% 
    select(-ID,-NCPDP.Provider.ID, -Primary.Provider.Type.Code, -Secondary.Provider.Type.Code, -Tertiary.Provider.Type.Code, -Dispenser.Class.Code, -Medicare.Provider.ID, National.Provider.ID..NPI.,
           -DBA.Name, -Physical.Address.1, -Physical.Address.2, -Physical.Address.City, -Physical.Address.State, -Physical.Address.ZIP, -County.FIPS, -MSA, -Open.24.Hours,
           -Language.Code.1, -Language.Code.2, -Language.Code.3, -Language.Code.4, -Language.Code.5, -Closed.Door.Facility.Indicator, -Closed.Door.Facility.Status.Code,
           -Accepts.ePrescriptions.Code, -Accepts.ePrescriptions.Indicator, -Delivery.Service.Indicator, -Delivery.Service.Code, -Compounding.Service.Indicator,
           -Driveup.Window.Code, -Driveup.Window.Indicator, -DME.Indicator, -DME.Code, -Walkin.Clinic.Indicator, -Walkin.Clinic.Indicator, -Multidose.Compliance.Indicator, -Multidose.Compliance.Code,
           -Immunizations.Provided.Indicator, -Immunizations.Provided.Code, -Handicapped.Accessible.Indicator, -Handicapped.Accessible.Code, -X340B.Status.Indicator, -X340B.Status.Code,
           -ID.x, -Legal.Business.Name, -National.Provider.ID..NPI., -Compounding.Service.Code, -Compounding.Service.Indicator, -Walkin.Clinic.Code,
           -X24h.Emergency.Service.Indicator, -X24h.Emergency.Service.Code, -Doctors.Name, -Store.Number, -Phone.Number, -Phone.Extension, 
           -Fax.Number, -Email.Address, -Cross.Streets, -PMSA, -Provider.Hours,
           -Congressional.Voting.District, -Store.Open.Date, - Store.Close.Data,
           -DEA.Registration.ID, - DEA.Expiration.Date, -Federal.Tax.ID.Number,
           -State.Income.Tax.ID.Number, -Deactivation.Code, -Reinstatement.Code,
           -Reinstatement.Date, -Transaction.Code, -Transaction.Date, -ID.y) # Drop unnecessary columns

  ############ STOPPED HERE ON Feb 8th. Make the old code more efficient with how I name dataframes.
  # Assign each pharmacy to a census tract (by GEOID10)
  ## transform pharmacy lat/long dataframe to SF object
  mypoints_sf <- st_as_sf(pharmgeo_df2,
                          coords = c("lon", "lat"),
                          crs = 4326) # set with WSG84 which is what Google maps uses for geocoding
  ## transform the pharmaddress SF to the same CRS as census data uses (NAD83 not WSG84)
  mypoints_sf2 <- st_transform(mypoints_sf, crs = st_crs(tract_c))
  ## check that they are identical
  identical(st_crs(tract_c),st_crs(mypoints_sf2)) # output is TRUE
  ### will use this mypoints_sf2 for mapping of points, as well as matching a pharmacy to a tract
  ## add column for GEOID to original DF version of pharm_address
  pharmgeo_df3 <- pharmgeo_df2
  ## use st_intersects & apply function to assign each pharmacy point to a tract polygon
  pharmgeo_df3$GEOID <- apply(st_intersects(tract_c, mypoints_sf2, sparse = FALSE), 2,
                                function(col) {
                                  tract_c[which(col),]$GEOID
                                })
  
  # Group by census tract and count how many pharmacies there are per tract
  
#################
### Read in census variables
  
  

  
  
  # Define criteria for pharmacy deserts: urban/rural, income, etc.
  
  