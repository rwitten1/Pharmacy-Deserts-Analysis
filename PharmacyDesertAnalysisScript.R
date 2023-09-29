################################################################
################################################################
#######
####### National Map of Pharmacy Deserts
####### Dec 2022
#######
################################################################
################################################################

## Steps:
## 1. Load packages and data files needed
## 2. Geocode pharmacy data (do this only once)
## 3. Finalize dataset for each row = pharmacy
## 4. Create dataset for each row = census tract
## 5. Define low income (census variables)
## 6. Define low access
## 7. Define pharmacy deserts in table
## 8. Create map of pharmacy deserts


# 1111111111111111111111111111111111111111111111111111111111111111111111111
# 1111111111111111111111111111111111111111111111111111111111111111111111111
# 1 Load packages and data files needed for analysis ######################
# 1111111111111111111111111111111111111111111111111111111111111111111111111

# Load packages
  library(tidycensus) # interface with census API to pull data tables
  library(censusapi) # use to get decennial data
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
  library(table1) # make summary data tables
# Install Census and API keys
  # # install census API key (only need to do this once per computer ever)
  my_api_key <- "083ae7aa6363d62361c694445ce61298cd1c2825"
  # # census_api_key(my_api_key, install = TRUE)
  # Sys.getenv("CENSUS_API_KEY") # to check if the api key is loaded
  # # install Google API key (new key for this proj vs my old one)
  # register_google(key = "AIzaSyCHb9LTmOgEswBpLGgEt594kIua5NQLxKo", write = TRUE)

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
  # providerinfo_df <- readxl::read_excel(paste0(inputDir,"Provider Information Table.xlsx"))
  # services_df <- readxl::read_excel(paste0(inputDir,"Services Information Table.xlsx"))
  
# County to MSA crosswalk: Needed for median income thresholds
  cty_msa_df <- read_csv(paste0(inputDir,"cty_cbsa_msa_list.csv")) # from March 2020 https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html 

# Reference list of state names, codes, and counties
  fipscodes <- tidycensus::fips_codes # list of state and county names and fips codes
  statefips_remove <- paste(c(60, 66, 69, 70, 72, 74, 78), collapse = '|') # remove american samoa, guam, northern mariana islands, palau, puerto rico, us minor outlying islands, virgin islands
  mystates <- fipscodes %>% filter(!str_detect(as.numeric(state_code), statefips_remove)) %>% select(state_code) %>% unique() # vector of all state fips codes for later looping
  mystates <- mystates$state_code
# Read in shape files (polygons used for assigning points to geos and for mapping). Default format is sf but can also set class = "sp" if needed
  # use Tigris to read in shapefiles directly using Census API (no local files needed)
  states_c <- tigris::states(year = 2020) %>% filter(!str_detect(STATEFP, statefips_remove))
  counties_c <- tigris::counties(cb = TRUE, year = 2020) %>% filter(!str_detect(STATEFP, statefips_remove))
  tract_c <- tigris::tracts(cb = TRUE, year = 2020) %>% filter(!str_detect(STATEFP, statefips_remove)) # goes from 85187 to 84122 (drops 1065)
  groups_c <- tigris::block_groups(cb = TRUE, year = 2020) %>% filter(!str_detect(STATEFP, statefips_remove)) #242298 ish? to 239380
  x <- readRDS("groups_c.RDS")
  groups_c <- x
# CURRENT CODE STATUS. load these final files for analysis/map manipulation:
  # As of Sept 17th:
  tractdata_df <- readRDS("tractdata_df.rds") #dataframe
  tractdata_sf <- readRDS("tractdata_sf.rds") #geo on google CRS
  pharmgeo_df7_df <- readRDS("pharmgeo_df7_df.rds") #dataframe
  pharmgeo_df7_sf <- readRDS("pharmgeo_df7_sf.rds") #geo on google CRS
  
  # OLD - delete once confirmed
  # # FIX THIS TOMORROW W DECENNIAL TRACTS
  # load("datafull_df.rda")
  # load("datafull_geo4326.rda")
  # datafull_df <- readRDS("datafull_df") # dataframe version w no geocoding
  # datafull_geo4326 <- readRDS("datafull_geo4326") # WSG84 google maps crs
  # datafull_geo4269 <- readRDS("datafull_geo4269") # NAD83 the census crs 
  # # save ones of pharmacies too
  # pharmacies_df <- readRDS("pharmacies_df")
  # pharmacies_geo4326 <- readRDS("pharmacies_geo4326") # WSG84 google maps crs 
  # pharmacies_geo4269 <- readRDS("pharmacies_geo4269") # NAD83 the census crs

# 2222222222222222222222222222222222222222222222222222222222222222222222222
# 2222222222222222222222222222222222222222222222222222222222222222222222222
# 2 Geocode pharmacy addresses -- DONE, CAN SKIP ##########################
# 2222222222222222222222222222222222222222222222222222222222222222222222222

# Merge NCPDP data tables
pharmacy_df <- full_join(providerinfo_df, services_df, by = "NCPDP Provider ID")
# Drop territories and pharmacies that are not community pharmacies:
pharmbystate_df <- pharmacy_df %>% group_by(`Physical Address State`) %>%
  dplyr::summarise(n_pharm = n()) %>%
  filter(!`Physical Address State` %in% c("PR", "GU", "VI", "MP", "NA")) # removing territories except DC. (PR 1041; GU 24; VI 20; MP 9).
table(pharmacy_df$`Primary Provider Type Code`, useNA = "always") # get numbers of pharmacy by type
pharmacy_df <- pharmacy_df %>% filter(`Primary Provider Type Code` == 1) # then filter to just include community pharmacies
  
# # Geocode all community pharmacies in 50 US states:
#   # break into 2 chunks:
#   # need to be under 40k addresses in order to stay under GoogleAPI geocoding budget limit for the month
  pharmbystate_df1 <- pharmbystate_df[1:25,] # contains 31510 pharmacies
  pharmbystate_df2 <- pharmbystate_df[26:52,] %>% filter(!is.na(`Physical Address State`)) #contains 30549 pharmacies
#   # going to start with geocoding the second half alphabetically because includes WA so can compare to previous analysis
  states1 <- pharmbystate_df1$`Physical Address State`
  states2 <- pharmbystate_df2$`Physical Address State`
  pharmacy_df1 <- pharmacy_df %>% filter(`Physical Address State` %in% states1)
  pharmacy_df2 <- pharmacy_df %>% filter(`Physical Address State` %in% states2)
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
  pharmacy_df1 <- read.csv("pharmacy_df1_geo.csv")[,-1]
  pharmacy_df2 <- read.csv("pharmacy_df2_geo.csv")[,-1]
#   # check column order:
#   identical(colnames(pharmacy_df1), colnames(pharmacy_df2)) #TRUE
#   pharmgeo_df <- rbind(pharmacy_df1, pharmacy_df2)
#   write.csv(pharmgeo_df, "pharmacy_geo.csv")

# 3333333333333333333333333333333333333333333333333333333333333333333333333
# 3333333333333333333333333333333333333333333333333333333333333333333333333
# 3 Finalize dataset for each row = pharmacy ##############################
# 3333333333333333333333333333333333333333333333333333333333333333333333333
  # read in df with geocoding complete: work with this data from now on
  pharmgeo_df <- read.csv("pharmacy_geo.csv")[,-1]  #62059 rows

# Clean up names, classes, drop unnecessary columns
  pharmgeo_df2 <- pharmgeo_df %>% 
    filter(Closed.Door.Facility.Indicator != "Y", # removed 1156 closed door facilities here as well. Or did i keep the N's?? 60664.
           Dispenser.Class.Code != "7") %>%       # removed 288 alternate dispensing sites as well (but 104 of these were also Y and 21 NA, so only removed 163 of these). Dropped 1340 here.
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

# Manually fill in the addresses missing lon and lat from geocoding
  # Check the addresses with NA for lon and lat
  # missingadddresses <- pharmgeo_df2 %>% 
  #   filter(is.na(lat)) %>% 
  #   select(addresses, lat, lon, dba_name, legal_name, ncpdp_id,
  #          address1, address2, city, state, zip, county_fips)
  # write.csv(missingadddresses, "missingaddresses1.csv")
  # Manual step here: 
    # look up the individual addresses for these 34 observations in Google Maps
    # add lat and lon to csv, read it back in
    missingaddresses2 <- read.csv("missingaddresses1.csv")
    missingaddresses2$latnew <- missingaddresses2$lat
    missingaddresses2$lonnew <- missingaddresses2$lon
    missingaddresses2$ncpdp_id <- as.character(missingaddresses2$ncpdp_id)
    missingaddresses2 <- missingaddresses2 %>% select(ncpdp_id, latnew, lonnew)
  # Add lat and lon to pharmgeo_df for the addresses we identified manually (should be 0 NA lat/lon remaining)
    pharmgeo_df3 <- full_join(pharmgeo_df2, missingaddresses2, by = "ncpdp_id")
    pharmgeo_df3$lon <- ifelse(is.na(pharmgeo_df3$lon), pharmgeo_df3$lonnew, pharmgeo_df3$lon)
    pharmgeo_df3$lat <- ifelse(is.na(pharmgeo_df3$lat), pharmgeo_df3$latnew, pharmgeo_df3$lat)
    pharmgeo_df3 <- pharmgeo_df3 %>% select(-lonnew, -latnew)
  # Remove objects from our envt that we no longer need to maximize space/memory for the next step
    rm(providerinfo_df, pharmgeo_df, pharmgeo_df2)
    write.csv(pharmgeo_df3, "pharmgeo_df3.csv")
    pharmgeo_df3 <- read.csv("pharmgeo_df3.csv")[,-1]
    
# Assign each pharmacy to a census tract (by GEOID10)
  # transform pharmacy lat/long dataframe to SF object
    mypoints_sf <- st_as_sf(pharmgeo_df3,
                            coords = c("lon", "lat"),
                            crs = 4326) # set with WSG84 which is what Google maps uses for geocoding

  # transform the pharmaddress SF to the same CRS as census data uses (census is NAD83 not WSG84)
    # will use this mypoints_sf2 for mapping of points, as well as matching a pharmacy to a tract
    mypoints_sf2 <- st_transform(mypoints_sf, crs = st_crs(tract_c))
    # check that they are identical
    identical(st_crs(tract_c),st_crs(mypoints_sf2)) # output is TRUE

  # Assign each pharmacy point to a tract polygon use st_intersects & apply function
    # This function takes a lot of memory, need to do it in a loop state by state
    pharmbystate <- pharmgeo_df3 %>% group_by(state) %>% dplyr::summarise(n_pharm = n()) # check count of pharmacies by state to see if small enough
    # Vector of state abbreviations
    statelist <- pharmbystate$state
    # goal of loop = create a dataframe of just ncpdp ids and GEOID of the tract. Do Rbind by state. Then will join this df by ncpdp_id to pharmgeo3 to make the full dataset
      # Create temporary storage dataframe to store output of each state loop
      tempstorage <- data.frame()
      # Note: loop takes about 2 minutes to run
      for (state_i in statelist) {
        # subset the pharmacy points and tracts to dfs of the state we are looping for
        mypoints_sf2_temp <- mypoints_sf2 %>% filter(state %in% state_i)
        tract_c_temp <- tract_c %>% filter(STUSPS %in% state_i)
        # create an output df of the ncpdpd_id's for this state we are looping for
        pharmgeo_df3_temp <- pharmgeo_df3 %>% filter(state %in% state_i) %>% select(ncpdp_id) 
        # function to assing GEOID to each ncpdp_id
        pharmgeo_df3_temp$GEOID <- as.character(base::apply(st_intersects(x = tract_c_temp, y = mypoints_sf2_temp, sparse = FALSE), 2,
                                                            FUN = function(col) {
                                                              tract_c_temp[which(col),]$GEOID
                                                              }))
        # bind the result of each iteration together as the consolidated output
        tempstorage <-rbind(tempstorage,pharmgeo_df3_temp)
      }
    # Join result of loop (GEOIDs) to pharmgeo_df to get our main dataframe of pharmacies now with GEOID of tract
    pharmgeo_df4 <- left_join(pharmgeo_df3, tempstorage, by = "ncpdp_id") 
    colnames(pharmgeo_df4)
    # # save as a csv so don't have to run this loop every time
    write.csv(pharmgeo_df4, "pharmgeo_df4.csv")
    # pharmgeo_df4 <- read.csv("pharmgeo_df4.csv")[,-1]

# 4444444444444444444444444444444444444444444444444444444444444444444444444
# 4444444444444444444444444444444444444444444444444444444444444444444444444
# 4 Create dataset for each row = census tract ############################
# 4444444444444444444444444444444444444444444444444444444444444444444444444

## Variables from ACS (Sociodemographic) ####
# Read in ACS census variables (from: https://api.census.gov/data/2021/acs/acs5/variables.html)
    census_vars <- c(
        # population by age and gender
        "B01001_001", "B01001_007", "B01001_008", "B01001_009", "B01001_010","B01001_011","B01001_012","B01001_013","B01001_014","B01001_015","B01001_016","B01001_017","B01001_018","B01001_019","B01001_020","B01001_021","B01001_022","B01001_023","B01001_024","B01001_025",
        "B01001_031", "B01001_032", "B01001_033", "B01001_034","B01001_035","B01001_036","B01001_037","B01001_038","B01001_039","B01001_040","B01001_041","B01001_042","B01001_043","B01001_044","B01001_045","B01001_046","B01001_047","B01001_048","B01001_049",
        # household income, below FPL, vehicle ownership:
        "B19013_001", "B17001_001", "B17001_002", "B17001_031", "B08201_001",
        # education level: adults with a high school degree
          "B15001_001", # total adult population for this question
          "B15001_006", # male, high school graduate, aged 18-24
          "B15001_014", # male, high school graduate, aged 25-34
          "B15001_022", # male, high school graduate, aged 35-44
          "B15001_030", # male, high school graduate, aged 45-64
          "B15001_038", # male, high school graduate, aged 65+
          "B15001_047", # female, high school graduate, aged 18-24
          "B15001_055", # female, high school graduate, aged 25-34
          "B15001_063", # female, high school graduate, aged 35-44
          "B15001_071", # female, high school graduate, aged 45-64
          "B15001_079", # female, high school graduate, aged 65+
        # race and ethnicity combined 
          "B03002_001", # total for this question: hispanic/latino by race
          "B03002_003", # non-hispanic white
          "B03002_004", # non-hispanic black
          "B03002_005", # non-hispanic AIAN
          "B03002_006", # non-hispanic Asian
          "B03002_007", # non-hispanic NHPI
          "B03002_008", # non-hispanic other race
          "B03002_009", # non-hispanic 2+ races
          "B03002_013", # hispanic white
          "B03002_014", # hispanic black
          "B03002_015", # hispanic AIAN
          "B03002_016", # hispanic Asian
          "B03002_017", # hispanic NHPI
          "B03002_018", # hispanic other race
          "B03002_019", # hispanic 2+ races
        # health insurance
          "B27001_009", # total population: male 19-25
          "B27001_012", # total population: male 26-34
          "B27001_015", # total population: male 35-44
          "B27001_018", # total population: male 45-54
          "B27001_021", # total population: male 55-64
          "B27001_024", # total population: male 65-74
          "B27001_027", # total population: male 75+
          "B27001_011", # has no health insurance coverage: male 19-25
          "B27001_014", # has no health insurance coverage: male 26-34
          "B27001_017", # has no health insurance coverage: male 35-44
          "B27001_020", # has no health insurance coverage: male 45-54
          "B27001_023", # has no health insurance coverage: male 55-64
          "B27001_026", # has no health insurance coverage: male 65-74
          "B27001_029", # has no health insurance coverage: male 75+
          "B27003_010", # has public health insurance coverage: male 19-25
          "B27003_013", # has public health insurance coverage: male 26-34
          "B27003_016", # has public health insurance coverage: male 35-44
          "B27003_019", # has public health insurance coverage: male 45-54
          "B27003_022", # has public health insurance coverage: male 55-64
          "B27003_025", # has public health insurance coverage: male 65-74
          "B27003_028", # has public health insurance coverage: male 75+
          "B27001_037", # total population: female 19-25
          "B27001_040", # total population: female 26-34
          "B27001_043", # total population: female 35-44
          "B27001_046", # total population: female 45-54
          "B27001_049", # total population: female 55-64
          "B27001_052", # total population: female 65-74
          "B27001_055", # total population: female 75+
          "B27001_039", # has no health insurance coverage: female 19-25
          "B27001_042", # has no health insurance coverage: female 26-34
          "B27001_045", # has no health insurance coverage: female 35-44
          "B27001_048", # has no health insurance coverage: female 45-54
          "B27001_051", # has no health insurance coverage: female 55-64
          "B27001_054", # has no health insurance coverage: female 65-74
          "B27001_057", # has no health insurance coverage: female 75+
          "B27003_038", # has public health insurance coverage: female 19-25
          "B27003_041", # has public health insurance coverage: female 26-34
          "B27003_044", # has public health insurance coverage: female 35-44
          "B27003_047", # has public health insurance coverage: female 45-54
          "B27003_050", # has public health insurance coverage: female 55-64
          "B27003_053", # has public health insurance coverage: female 65-74
          "B27003_056", # has public health insurance coverage: female 75+
        # english-speaking
          "B16004_024", # total adults age 18-64 years for this question
          "B16004_046", # total adults age 65 and over for this question
          "B16004_029", "B16004_030", # adults age 18-64 who speak english "not well" or "not at all"
          "B16004_051", "B16004_052", # adults age 65+ who speak english "not well" or "not at all"
        # physical disability status: sex by age by ambulatory difficulty
          "B18105_001", # total adult population for this question
          "B18105_005", "B18105_019", # total pop male and female age 5 - 17 (who we should subtract from the total)
          "B18105_007", # male with ambulatory difficulty 18-34 years
          "B18105_010", # male with ambulatory difficulty 35-64 years
          "B18105_013", # male with ambulatory difficulty 65-74 years
          "B18105_016", # male with ambulatory difficulty aged 75+
          "B18105_023", # female with ambulatory difficulty 18-34 years
          "B18105_026", # female with ambulatory difficulty 35-64 years
          "B18105_029", # female with ambulatory difficulty 65-74 years
          "B18105_032", # female with ambulatory difficulty aged 75+
        # inequality
          "B19083_001" # GINI Index of income inequality
        )
    
    censusdata <- data.frame() # note the loop takes several mins to run
    for (state_i in mystates) {
      censusdata_temp <- tidycensus::get_acs(geography = "tract",                 # gets read in with a GEOID field, so can merge with pharmacy points here
                                               variables = census_vars,           # list of variables in vector above
                                               state = state_i,                   # list of all states
                                               geometry = FALSE,                  # if false, doesnt read in geometry col with lat/long
                                               output = "wide",                   # may need output = tidy if want to use ggplot for static maps later
                                               year = 2021,
                                               survey = "acs5")   
      # bind the result of each iteration together as the consolidated output
      censusdata <-rbind(censusdata,censusdata_temp)
    }
    
    # Create proportions and estimates using the estimates of each variable
    censusdata2 <- censusdata %>% rowwise() %>% 
      mutate(pop_total = B01001_001E,
             pop_adult = sum(c(B01001_007E,B01001_008E,B01001_009E,B01001_010E,B01001_011E,B01001_012E,B01001_013E,B01001_014E,B01001_015E,B01001_016E,B01001_017E,B01001_018E,B01001_019E,B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,
                               B01001_031E,B01001_032E,B01001_033E,B01001_034E,B01001_035E,B01001_036E,B01001_037E,B01001_038E,B01001_039E,B01001_040E,B01001_041E,B01001_042E,B01001_043E,B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E)),
             pop_65up_n = sum(c(B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,
                              B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E)),
             pop_65up_p = pop_65up_n/pop_total,
             fpl_p = B17001_002E/B17001_001E, 
             fpl_percent_bin = ifelse(fpl_p >= .2, 1, 0),
             lowveh_bin = ifelse(B08201_001E < 100, 1, 0),
             median_income = B19013_001E,
             educ_hs_p = sum(c(B15001_006E, B15001_014E, B15001_022E, B15001_030E, B15001_038E, B15001_047E, B15001_055E, B15001_063E, B15001_071E, B15001_079E))/B15001_001E,
             race_nh_white_p = B03002_003E/B03002_001E,
             race_nh_black_p = B03002_004E/B03002_001E,
             race_nh_aian_p = B03002_005E/B03002_001E,
             race_nh_asian_p = B03002_006E/B03002_001E,
             race_nh_nhpi_p = B03002_007E/B03002_001E,
             race_nh_other_p = B03002_008E/B03002_001E,
             race_nh_2p_p = B03002_009E/B03002_001E,
             race_hisp_white_p = B03002_013E/B03002_001E,
             race_hisp_black_p = B03002_014E/B03002_001E,
             race_hisp_aian_p = B03002_015E/B03002_001E,
             race_hisp_asian_p = B03002_016E/B03002_001E,
             race_hisp_nhpi_p = B03002_017E/B03002_001E,
             race_hisp_other_p = B03002_018E/B03002_001E,
             race_hisp_2p_p = B03002_019E/B03002_001E,
             ins_popadult = sum(c(B27001_009E, B27001_012E, B27001_015E,B27001_018E,B27001_021E, B27001_024E,B27001_027E, B27001_037E,B27001_040E,B27001_043E,B27001_046E,B27001_049E,B27001_052E,B27001_055E)),
             ins_none_p = sum(c(B27001_011E,B27001_014E, B27001_017E,B27001_020E,B27001_023E,B27001_026E,B27001_029E,B27001_039E,B27001_042E,B27001_045E,B27001_048E,B27001_051E,B27001_054E,B27001_057E))/ins_popadult,
             ins_public_p = sum(c(B27003_010E,B27003_013E,B27003_016E, B27003_019E,B27003_022E,B27003_025E,B27003_028E,B27003_038E,B27003_041E,B27003_044E, B27003_047E,B27003_050E,B27003_053E,B27003_056E))/ins_popadult,
             notenglspeak_p = sum(c(B16004_029E, B16004_030E, B16004_051E, B16004_052E))/sum(c(B16004_024E,B16004_046E)),
             disability_p = sum(c(B18105_007E,B18105_010E,B18105_013E,B18105_016E,B18105_023E,B18105_026E,B18105_029E,B18105_032E))/(B18105_001E - sum(B18105_005E, B18105_019E)),
             inequality_gini = B19083_001E,
             ) %>% 
      ungroup() %>% as.data.frame() 
    # This has data for 84414 tracts, as opposed to tigris polygons have 84122 tracts? Diff of 292.
    # Get quick check of how many census tracts in US have 0 population (and thus no estimates of these variables)
    nopop <- censusdata2 %>% filter(pop_total == 0) #811 tracts in the US have no pop. Don't drop yet bc may have pharmacies in them!
    # write this csv so we don't have to keep re-running this when we find errors
    # write.csv(censusdata2, "censusdata2.csv")
    
## Variables from Decennial (Population Counts) ####  
# Add decennial data in for population by tract, incl by adult/elderly
    variables_dec <- c("P12_001N", # total population
                       "P12_002N", # total male 
                       "P12_007N", # male 18-19
                       "P12_008N", # male 20
                       "P12_009N", # male 21
                       "P12_010N", # male 22-24
                       "P12_011N", # male 25-29
                       "P12_012N", # male 30-34
                       "P12_013N", # male 35-39
                       "P12_014N", # male 40-44
                       "P12_015N", # male 45-49
                       "P12_016N", # male 50-54
                       "P12_017N", # male 55-59
                       "P12_018N", # male 60-61
                       "P12_019N", # male 62-64
                       "P12_020N", # male 65-66
                       "P12_021N", # male 67-69
                       "P12_022N", # male 70-74
                       "P12_023N", # male 75-79
                       "P12_024N", # male 80-84
                       "P12_025N", # male 85+ years
                       "P12_026N", # total female
                       "P12_031N", # female 18-19
                       "P12_032N", # female 20
                       "P12_033N", # female 21
                       "P12_034N", # female 22-24
                       "P12_035N", # female 25-29
                       "P12_036N", # female 30-34
                       "P12_037N", # female 35-39
                       "P12_038N", # female 40-44
                       "P12_039N", # female 45-49
                       "P12_040N", # female 50-54
                       "P12_041N", # female 55-59
                       "P12_042N", # female 60-61
                       "P12_043N", # female 62-64
                       "P12_044N", # female 65-66
                       "P12_045N", # female 67-69
                       "P12_046N", # female 70-74
                       "P12_047N", # female 75-79
                       "P12_048N", # female 80-84
                       "P12_049N") # female 85+ years
    
    decennialpop_tract <- data.frame()
    for (state_i in mystates) {
      decennialpop_tract_temp <- get_decennial(
        sumfile = "dhc",
        geography = "tract",
        variables = variables_dec, 
        state = state_i,
        geometry = FALSE,
        output = "wide",
        year = 2020)
      # bind the result of each iteration together as the consolidated output
      decennialpop_tract <-rbind(decennialpop_tract, decennialpop_tract_temp)
    }
    rm(decennialpop_tract_temp)
    
    # Calculate various pops from decennial tract data for use in analyses:
    decennialpop_tract2 <- decennialpop_tract %>% rowwise() %>% 
      mutate(decpop_total_n = P12_001N,
             decpop_adult_n = sum(c(P12_007N, P12_008N, P12_009N, P12_010N, P12_011N, P12_012N, P12_013N, P12_014N, P12_015N, P12_016N, P12_017N, P12_018N, P12_019N, P12_020N, P12_021N, P12_022N, P12_023N, P12_024N, P12_025N,
                                    P12_031N, P12_032N, P12_033N, P12_034N, P12_035N, P12_036N, P12_037N, P12_038N, P12_039N, P12_040N, P12_041N, P12_042N, P12_043N, P12_044N, P12_045N, P12_046N, P12_047N, P12_048N, P12_049N)),
             decpop_male_n = P12_002N,
             decpop_female_n = P12_026N,
             decprop_female_p = decpop_female_n/decpop_total_n,
             decpop_65up_n = sum(c(P12_020N, P12_021N, P12_022N, P12_023N, P12_024N, P12_025N,
                                   P12_044N, P12_045N, P12_046N, P12_047N, P12_048N, P12_049N))) %>% 
      ungroup() %>% as.data.frame() %>% 
      select(-contains(variables_dec))

# merge this with the census tract dataset
censusdata2 <- dplyr::full_join(censusdata2, decennialpop_tract2, by = "GEOID")
    # # check: compare tract pops in acs vs decennial
    # summary(censusdata2$pop_adult)
    # summary(censusdata2$decpop_adult)
    # summary(censusdata2$pop_65up_p)
    # summary(censusdata2$decpop_65up_n/censusdata2$decpop_total_n)
# write.csv(censusdata2, "censusdata2_line566.csv")
saveRDS(censusdata2, "censusdata2_line566.rds")
    
# 5555555555555555555555555555555555555555555555555555555555555555555
# 5555555555555555555555555555555555555555555555555555555555555555555
# 5 Create Definition Part 1: Low-Income ############################
# 5555555555555555555555555555555555555555555555555555555555555555555
# Low income = Part 1: > 20% of population living below FPL, OR Part 2: median income of tract is <80% of income of nearest metro area
  # Part 1: FPL threshold  already created above in part 4 as: variable = fpl_percent_bin in censusdata2
  # Part 2: Incomes of tracts and nearest metro areas:
     # Get median incomes of tract, metro area, and state
      # Get median income for each tract, too big to call at once so need to do loop
      income_tract <- data.frame() # note the loop takes about 2 mins to run
      for (state_i in mystates) {
        # get the median incomes by tract in each state and store it in a temporary dataframe
        income_tract_temp <- tidycensus::get_acs(geography = "tract",                 # gets read in with a GEOID field, so can merge with pharmacy points here
                                                 variables = "B19013_001E",           # median income
                                                 state = state_i,                     # list of all states
                                                 geometry = FALSE,                    # if false, doesnt read in geometry col with lat/long
                                                 output = "wide",                     # may need output = tidy if want to use ggplot for static maps later
                                                 year = 2021,
                                                 survey = "acs5")   
        # bind the result of each iteration together as the consolidated output
        income_tract <-rbind(income_tract,income_tract_temp)
      }
      # median household income of each MSA
      income_msa <- tidycensus::get_acs(geography = "cbsa",
                                        variables = "B19013_001",
                                        year = 2021,
                                        survey = "acs5") %>% rename(msa_id = GEOID)
      # median household income of each state
      income_state <- tidycensus::get_acs(geography = "state",
                                          variables = "B19013_001",
                                          year = 2021,
                                          survey = "acs5") %>% rename(state_fips = GEOID)
    # Align census tract with MSA. Put MSA income as threshold if available, otherwise (non-MSA) use state median income
      # extract FIPS of each census tract ID to get county (first 5 digits of GEOID) and state for merging purposes
      income_tract$county_fips <- str_sub(income_tract$GEOID, 1, 5)
      income_tract$state_fips <- str_sub(income_tract$GEOID, 1, 2)
      # map counties to MSAs
      cty_msa_df$county_fips <- paste0(cty_msa_df$`FIPS State Code`, cty_msa_df$`FIPS County Code`)
      cty_msa_df$msa_id <- cty_msa_df$`CBSA Code`
      tempdf <- full_join(income_tract, cty_msa_df, by = "county_fips")
      tempdf2 <- full_join(tempdf, income_msa, by = "msa_id") %>% 
        rename(GEOID_tract = GEOID,
               tract_name = NAME.x,
               med_income_tract = B19013_001E,
               med_income_tract_moe = B19013_001M,
               med_income_msa = estimate,
               med_income_msa_moe = moe,
               GEOID_state = `FIPS State Code`) %>% 
        select(-`Metropolitan Division Code`,-`Metropolitan Division Title`, -NAME.y)
      incomedata_df <- full_join(tempdf2, income_state, by = "state_fips") %>% 
        rename(med_income_state = estimate,
               med_income_state_moe = moe) %>% 
        select(-NAME,-variable.y)
      rm(tempdf, tempdf2)
      
      # # Check the crosstabs and NAs of things to confirm things look good
      # incomedata_df$invalid <- ifelse(incomedata_df$med_income_tract_moe > incomedata_df$med_income_tract, 1, 0) # no tracts have moe > estimate, good.
      # incomedata_df$na <- ifelse(is.na(incomedata_df$med_income_tract), 1, 0) #1641 tracts have NA income data --> drop these. NO lets not drop these bc may have pharmacies.
      # incomedata_df <- incomedata_df %>%  filter(!is.na(med_income_tract)) # dropped the tracts with NA median income
      
      # create column for: if a tract is in an MSA, use the MSA income. If not in an MSA, use the state median household income
      incomedata_df$median_income_threshold <- NA
      incomedata_df$median_income_threshold <- ifelse(!is.na(incomedata_df$med_income_msa), incomedata_df$med_income_msa, NA)
      incomedata_df$median_income_threshold <- ifelse(is.na(incomedata_df$med_income_msa), incomedata_df$med_income_state, incomedata_df$median_income_threshold)
      
    # Compare tract income to appropriate threshold and create flag
    incomedata_df$low_income_medincome_flag <- ifelse((!is.na(incomedata_df$med_income_tract) & incomedata_df$med_income_tract < 0.8*incomedata_df$median_income_threshold), 1, 0) #25k yes and 59k no's
    
    # merge it back with the rest of the census tract level data
    dfformerge1 <- incomedata_df %>% select(state_fips, county_fips, GEOID_tract, tract_name, med_income_tract, med_income_tract_moe, low_income_medincome_flag) 
    censusdata2$GEOID_tract <- censusdata2$GEOID
    censusdata3 <- full_join(dfformerge1, censusdata2, by = "GEOID_tract") # doing this adds 80 rows, all with blank GEOID. they are all from the income data df.happened when merging cty_msa and income_tract dfs. so there must be some MSAs with weird county codes? Invesigating these- they all have state code for Puerto Rico. Can safely drop these.
    censusdata4 <- censusdata3 %>% select(-contains(census_vars), -GEOID) %>% filter(!is.na(GEOID_tract))
    
    # write.csv(censusdata4, "censusdata4_line648.csv")
    saveRDS(censusdata4, "censusdata4_line648.rds")
    # censusdata4 <- read.csv("censusdata4_line648.csv")[,-1]
    # censusdata4$GEOID_tract <- as.character(censusdata4$GEOID_tract)
    
# 6666666666666666666666666666666666666666666666666666666666666666666
# 6666666666666666666666666666666666666666666666666666666666666666666
# 6 Create Definition Part 2: Low-Access ############################
# 6666666666666666666666666666666666666666666666666666666666666666666
# First step, need to creat urban, suburban, rural definitions
# Definition = using population density because 2020 RUCA codes not available until Dec 2023 at earliest
    tract_2021 <- data.frame() # note the loop takes about 2 mins to run
    for (state_i in mystates) {
      tract_2021_temp <- tidycensus::get_acs(geography = "tract",                 # gets read in with a GEOID field, so can merge with pharmacy points here
                                             variables = "B01001_001E",           # total population
                                             state = state_i,                     # list of all states
                                             geometry = FALSE,                    # if false, doesnt read in geometry col with lat/long
                                             output = "wide",                     # may need output = tidy if want to use ggplot for static maps later
                                             year = 2021,
                                             survey = "acs5")   
      # bind the result of each iteration together as the consolidated output
      tract_2021 <-rbind(tract_2021,tract_2021_temp)
    }
  # Merge the 2021 tract population data (above) with the tract_c geo information to get ALAND in m^2
  # Using population density to define urban, suburban, rural. Loosely based on census doc: https://www2.census.gov/geo/pdfs/reference/ua/Defining_Rural.pdf. Other great reference: https://link.springer.com/article/10.1007/s11524-005-9016-3
  tract_2021_geo <- full_join(tract_2021, as.data.frame(tract_c %>% st_drop_geometry() %>% dplyr::select(-NAME)), by = "GEOID") %>% 
    select(-STATEFP, -COUNTYFP, -TRACTCE, -AFFGEOID, -NAME, -STUSPS, -NAMELSAD, -NAMELSADCO, -LSAD, -STATE_NAME, -NAME, -B01001_001M) %>% 
    mutate(pop_density = as.numeric(B01001_001E/(ALAND/2589988.11))) # population density per square mile
           # pop_density = pop_density_m2 / 2589988.11)
  tract_2021_geo <- as.data.frame(tract_2021_geo)
  tract_2021_geo$urbanicity <- ifelse(tract_2021_geo$pop_density >= 5000, 1,
                                      ifelse((tract_2021_geo$pop_density >= 1000 & tract_2021_geo$pop_density < 5000), 2,
                                             ifelse((tract_2021_geo$pop_density > 0 & tract_2021_geo$pop_density < 1000), 3, "XX"))) #320 with population of 0, no land area
  tract_2021_geo$pop_density <- ifelse((is.na(tract_2021_geo$ALAND) | tract_2021_geo$ALAND ==0), "NA: No land area", tract_2021_geo$pop_density)
  tract_2021_geo$pop_density <- ifelse((is.na(tract_2021_geo$B01001_001E) | tract_2021_geo$B01001_001E ==0), "NA: No Population", tract_2021_geo$pop_density)
  tract_2021_geo$urbanicity <- ifelse(tract_2021_geo$pop_density %in% "NA: No Population", "NA: No Population", tract_2021_geo$urbanicity)
  popurbanicitysummary <- tract_2021_geo %>% group_by(urbanicity) %>% dplyr::summarise(poptotal = sum(B01001_001E),
                                                                                       n_tracts = n())
  # the cutoffs of 5000 and 1000 result in 28% urban, 38% suburban, 34% rural population.
  table(tract_2021_geo$urbanicity, useNA = "always")
  tract_2021_geo$accessradius <- ifelse(tract_2021_geo$urbanicity == 1, 1,
                                        ifelse(tract_2021_geo$urbanicity == 2, 5,
                                               ifelse(tract_2021_geo$urbanicity == 3, 10,
                                                      ifelse(tract_2021_geo$urbanicity %in% "NA: No Population", "NA: No Population", NA))))
  table(tract_2021_geo$accessradius, tract_2021_geo$urbanicity, useNA = "always")
  tract_2021_geo <- tract_2021_geo %>% 
    mutate(GEOID_tract = as.character(GEOID)) %>% 
    select(-GEOID, -B01001_001E, -ALAND, -AWATER)
  censusdata5 <- full_join(censusdata4, tract_2021_geo, by = "GEOID_tract") 
  censusdata5$accessradius <- ifelse(censusdata5$lowveh_bin == 1 & !is.na(censusdata5$pop_adult), 0.5, censusdata5$accessradius)
  saveRDS(censusdata5, file =  "censusdata5.rds")
  # write.csv(censusdata5, "censusdata5-backuponly-padtheGEOS.csv") # if read this in, make sure to pad the GEODIS with a leading 0 if not 11 characters! messes up the merge.
  
# After this we deal with pharmacies and census tracts
  # Group pharmgeo by by GEOID for census tract, count number of pharmacies per census tract
  # group by census tracts and count how many pharmacies there are
  pharm_to_tract <- pharmgeo_df4 %>% # confirmed that pharmgeodf4 is still preserved w the 11-char GEOIDs
    select(GEOID, ncpdp_id) %>%
    group_by(GEOID) %>%
    summarise(ph_per_tract = n()) %>%
    arrange(desc(ph_per_tract)) %>%
    rename(GEOID_tract = GEOID)
  pharm_to_tract <- as.data.frame(pharm_to_tract)
  # 35,785 tracts have pharmacies, out of 84,414 tracts in the US total

  # join pharmacies dataframe w urban/rural and codes and census dataframe 
  censusdata6 <- left_join(censusdata5, pharm_to_tract, by = "GEOID_tract") # 24 pharmacies were not able to be assigned a tract. do a left join to drop these.
  # 1065 have 0 pharmacies and 0 population (keep for now)
 
  # create cols for n pharmacies per tract, pharmacies per 1000 adult population, pharmacies per 65+ population
  censusdata6$ph_per_ktotalpop <- censusdata6$ph_per_tract/censusdata6$decpop_total_n*1000
  censusdata6$ph_per_kadultpop <- censusdata6$ph_per_tract/censusdata6$decpop_adult_n*1000
  censusdata6$ph_per_k65pop <- censusdata6$ph_per_tract/censusdata6$decpop_65up_n*1000

# Back to the pharmacy dataset: take the pharmacy dataset and add the urban/rural radius information to it so we can calculate the buffers
  # add rural/urban status by GEOID to each pharmacy
  ruralurbankey <- censusdata6 %>% select(GEOID_tract, urbanicity, accessradius) %>% unique() %>% rename(GEOID = GEOID_tract)
  pharmgeo_df5 <- left_join(pharmgeo_df4, ruralurbankey, by = "GEOID") # pharmgeodf4 is in crs = 4326 =  wgs84 this is the google maps geocode
  
  # # There are 26 pharmacies not assigned a GEOID. Come back to these later to manually add them in if time.
      # possibly can use call_geolocator for each address to geocode it? page 11 here: https://cran.r-project.org/web/packages/tigris/tigris.pdf
      # test <- pharmgeo_df5 %>% filter(is.na(accessradius))
      # # These 25 pharmacies were not assigned a GEOID. Add manually (then move back up above). so note that the lat/lon in here are for google maps, transform this before using census's geocoder.
      # missinggeos <- st_transform(pharmgeo_df5, crs = 4269) %>% filter(is.na(accessradius))# transform it to census crs
      # write.csv(missinggeos, "missingpharmgeos2.csv") # use these lat/long to put into census geocoder and add GEOIDs manually 
      # when read it back in, convert to sf type not df
  
  pharmgeo_df6 <- st_as_sf(pharmgeo_df5,
                           coords = c("lon", "lat"),
                           crs = 4326) # wgs84 this is the google maps geocode
  
  # Define the buffers
  radius_km <- swfscMisc::convert.distance(c(0.5, 1, 5, 10), from = c("mi"), to = c("km"))
  radius_km*1000
  
  buffers <- st_transform(pharmgeo_df6, crs = 3857) #transform to a different projection that uses meters as unit of distance
  buffers_0.5mi <- buffers %>% filter(accessradius == 0.5) %>% st_buffer(dist = 804.672) #804.672 is 0.5 mile in meters, as calculated in line 500 above
  buffers_1mi <- buffers %>% filter(accessradius == 1) %>% st_buffer(dist = 1609.344) #1609.34 is 1 mile in meters
  buffers_5mi <- buffers %>% filter(accessradius == 5) %>% st_buffer(dist = 8046.720) #8046.720 is 5 miles in meters
  buffers_10mi <- buffers %>% filter(accessradius == 10) %>% st_buffer(dist = 16093.440) #10mi in meters
  buffers2 <- rbind(buffers_0.5mi, buffers_1mi, buffers_5mi, buffers_10mi)
  buffers2 <- st_transform(buffers2, crs = 4326) # transform back to wsg84 / google maps version to use with leaflet
  saveRDS(buffers2, file = "buffers2.rds")
  
  # calculate proportion in radius with blocks instead of block groups for more geographic granularity
  # Error with maryland (24) and new york (36) for some reason- try again later and rbind it to centroidsblocks_1_23, centroidsblocks 25_35
  # For now, skipping and trying 25 - 56.
  # mystates1_23 <- c("1","2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
  # mystates25_356 <- c("25","26","27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")
  # mystates41_56 <- c("41", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")
  mystatesmissing_4042 <- c("40", "42")
  blocks_c <- data.frame()
  centroidsblocks <- data.frame()
  for (state_i in mystatesmissing_4042) {
    blocks_temp <- tigris::blocks(year = 2020,
                                 state = 42)  
    blocks_trans <- st_transform(blocks_temp, crs = st_crs(buffers2))
    centroidsblocks_temp <- st_centroid(blocks_trans)
    centroidsblocks <- rbind(centroidsblocks, centroidsblocks_temp)
    blocks_c <- data.frame()
  }

  # centroidsblocks_1_39 <- centroidsblocks
  # centroidsblocks_41_56 <- centroidsblocks
  # centroidsblocks_40 <- centroidsblocks
  centroidsblocks_42 <- centroidsblocks_temp

  # # original steps
  # blocks_c_trans <- st_transform(blocks_c, crs = st_crs(buffers2)) # make census block group polys are same crs as the buffer df
  # identical(st_crs(blocks_c_trans),st_crs(buffers2)) # output is TRUE
  # centroidsblocks <- st_centroid(blocks_c_trans) # create a df of the points that are the centroids of each block
  # centroidsblocks$inbuffer_bin <- st_within(centroidsblocks, buffers2) %>% lengths > 0 # define: is the block centroid in any buffer? Likely will have to do this in loops again

  # run to create column for whether is in any of the buffers
  centroidsblocks_1_39$inbuffer_bin <- st_within(centroidsblocks_1_39, buffers2) %>% lengths > 0 #took 90mins
  centroidsblocks_41_56$inbuffer_bin <- st_within(centroidsblocks_41_56, buffers2) %>% lengths > 0 
  centroidsblocks_40$inbuffer_bin <- st_within(centroidsblocks_40, buffers2) %>% lengths > 0 
  centroidsblocks_42$inbuffer_bin <- st_within(centroidsblocks_42, buffers2) %>% lengths > 0 

    # rename to blocks_join. column for the population of each block group whose centroid is in a pharmacy buffer:
    colnames(centroidsblocks_1_39)
    centroidsblocks_1_39 <- centroidsblocks_1_39 %>% dplyr::select(-MTFCC20, -UACE20, -UATYPE20, -FUNCSTAT20, -ALAND20, -AWATER20)
    # saveRDS(centroidsblocks_1_39, file = "centroidsblocks1_39_buffbin.RDS")
    # centroidsblocks_1_39 <- readRDS("centroidsblocks1_39_buffbin.RDS")
    centroidsblocks_1_39$in_pop <- ifelse(centroidsblocks_1_39$inbuffer_bin == TRUE, centroidsblocks_1_39$POP20, 0)
    head(centroidsblocks_1_39) # want to group by census tract, may have to make a GEOID_tract column
    centroidsblocks_1_39$GEOID_tract <- substr(centroidsblocks_1_39$GEOID20, 1, 11)
    centroidsblocks_1_39_tract <- centroidsblocks_1_39 %>% 
      group_by(GEOID_tract) %>% 
      summarise(in_pop_total = sum(in_pop)) %>% 
      select(GEOID_tract, in_pop_total)
    centroidsblocks_1_39_tract <- st_drop_geometry(centroidsblocks_1_39_tract) %>% as.data.frame()
    
    # chunk2
    colnames(centroidsblocks_41_56)
    centroidsblocks_41_56 <- centroidsblocks_41_56 %>% 
      st_drop_geometry() %>% 
      as.data.frame() %>% 
      dplyr::select(-MTFCC20, -UACE20, -UATYPE20, -FUNCSTAT20, -ALAND20, -AWATER20)
    saveRDS(centroidsblocks_41_56, file = "centroidsblocks_41_56_buffbin.RDS")
    # centroidsblocks_41_56 <- readRDS("centroidsblocks_41_569_buffbin.RDS")
    centroidsblocks_41_56$in_pop <- ifelse(centroidsblocks_41_56$inbuffer_bin == TRUE, centroidsblocks_41_56$POP20, 0)
    head(centroidsblocks_41_56) # want to group by census tract, may have to make a GEOID_tract column
    centroidsblocks_41_56$GEOID_tract <- substr(centroidsblocks_41_56$GEOID20, 1, 11)
    centroidsblocks_41_56_tract <- centroidsblocks_41_56 %>% 
      group_by(GEOID_tract) %>% 
      summarise(in_pop_total = sum(in_pop)) %>% 
      select(GEOID_tract, in_pop_total)
    
    # chunk 3
    colnames(centroidsblocks_42)
    centroidsblocks_42 <- centroidsblocks_42 %>% 
      st_drop_geometry() %>% 
      as.data.frame() %>% 
      dplyr::select(-MTFCC20, -UACE20, -UATYPE20, -FUNCSTAT20, -ALAND20, -AWATER20)
    saveRDS(centroidsblocks_42, file = "centroidsblocks_40_buffbin.RDS")
    # centroidsblocks_40 <- readRDS("centroidsblocks_40_buffbin.RDS")
    centroidsblocks_42$in_pop <- ifelse(centroidsblocks_42$inbuffer_bin == TRUE, centroidsblocks_42$POP20, 0)
    head(centroidsblocks_40) # want to group by census tract, may have to make a GEOID_tract column
    centroidsblocks_42$GEOID_tract <- substr(centroidsblocks_42$GEOID20, 1, 11)
    centroidsblocks_42_tract <- centroidsblocks_42 %>% 
      group_by(GEOID_tract) %>% 
      summarise(in_pop_total = sum(in_pop)) %>% 
      select(GEOID_tract, in_pop_total)
    
    saveRDS(centroidsblocks_1_39_tract, file = "centroidsblocks_1_39_tract.RDS")
    saveRDS(centroidsblocks_41_56_tract, file = "centroidsblocks_41_56_tract.RDS")
    saveRDS(centroidsblocks_40_tract, file = "centroidsblocks_40_tract.RDS")
    saveRDS(centroidsblocks_42_tract, file = "centroidsblocks_40_tract.RDS")
    # centroidsblocks_1_39 <- readRDS("centroidsblocks1_39_buffbin.RDS")
  
# then RBIND the tract-level versions of this for 1-39, 41-56, and 40
    blockstract2 <- rbind(centroidsblocks_1_39_tract, centroidsblocks_41_56_tract, centroidsblocks_40_tract, centroidsblocks_42_tract) #...... etc once ready
    write.csv(blockstract2, "blockstract2.csv")
    blockstract3 <- read.csv("blockstract2.csv")[,-1]
    blockstract3$GEOID_tract <- str_pad(blockstract3$GEOID_tract, width = 11, side = "left", pad = "0") #pad w zeros to make full GEOID
    # join to census data for final low-access proportion analysis
    datafull <- full_join(censusdata6, blockstract3, by = "GEOID_tract") 
    datafull <- datafull %>% select(-NAME.x, -NAME.y)
    
    
    
    
#### NEW APPROACH FOR RADIUSES: ####
    ### New structure for radius. ppl in urban tracts need to be 1 mile from any pharmacy, even if that pharmacy is suburban and has a different radius.
    # new flow: do buffer calc 4 times (just remove the filter part up there):
    # still get the centroid of each block (group), and add up like before but 4 times with the 4 radiuses
    # 1 col each with inpop_1mibuffer, inpop_5mibuffer, inpop_10mibuffer, inpop_halfmilebuffer
    # then inpop_urbanicity is an ifelse with if accessradius = 0.5, then put inpop_halfmilebuffer
    # then inpop_p is the inpopurbanicity/tractpoptotal.
    
    # Define the buffers
    radius_km <- swfscMisc::convert.distance(c(0.5, 1, 5, 10), from = c("mi"), to = c("km"))
    radius_km*1000
    
    buffers <- st_transform(pharmgeo_df6, crs = 3857) #transform to a different projection that uses meters as unit of distance
    buffers_0.5mi <- buffers %>% st_buffer(dist = 804.672) #804.672 is 0.5 mile in meters, as calculated in line above
    buffers_1mi <- buffers %>% st_buffer(dist = 1609.344) #1609.34 is 1 mile in meters
    buffers_5mi <- buffers %>% st_buffer(dist = 8046.720) #8046.720 is 5 miles in meters
    buffers_10mi <- buffers %>% st_buffer(dist = 16093.440) #10mi in meters
   
    groups_c <- tigris::block_groups(cb = TRUE, year = 2021) %>% filter(STATEFP %in% 53) #242298 ish? to 239380
    # groups_c_trans <- st_transform(groups_c, crs = st_crs(buffers2)) # make census block group polys are same crs as the buffer df
    # identical(st_crs(groups_c_trans),st_crs(buffers2)) # checking, output is TRUE
    # centroidsgroups <- st_centroid(groups_c_trans) # create a df of the points that are the geometric centroids of each block group
    # centroidsgroups$inbuffer_bin <- st_within(centroidsgroups, buffers2) %>% lengths > 0 # define: is the blkgrp centroid in any buffer? Likely will have to do this in loops again
    
    # Do this for each of the 4 radii
    groups_c_trans <- st_transform(groups_c, crs = st_crs(buffers_1mi)) # make census block group polys are same crs as the buffer df
    identical(st_crs(groups_c_trans),st_crs(buffers_1mi)) # checking, output is TRUE
    centroidsgroups <- st_centroid(groups_c_trans) # create a df of the points that are the geometric centroids of each block group
    centroidsgroups$inbuffer05_bin <- st_within(centroidsgroups, buffers_0.5mi) %>% lengths > 0 # define: is the blkgrp centroid in any buffer? Likely will have to do this in loops again
    centroidsgroups$inbuffer1_bin <- st_within(centroidsgroups, buffers_1mi) %>% lengths > 0 # define: is the blkgrp centroid in any buffer? Likely will have to do this in loops again
    centroidsgroups$inbuffer5_bin <- st_within(centroidsgroups, buffers_5mi) %>% lengths > 0 # define: is the blkgrp centroid in any buffer? Likely will have to do this in loops again
    centroidsgroups$inbuffer10_bin <- st_within(centroidsgroups, buffers_10mi) %>% lengths > 0 # define: is the blkgrp centroid in any buffer? Likely will have to do this in loops again
    
    groupspop_dec <- data.frame() # note the loop takes about 2 mins to run
    for (state_i in mystates) {
      # get the median incomes by tract in each state and store it in a temporary dataframe
      groupspopdec_temp <- tidycensus::get_decennial(geography = "block group",                 # gets read in with a GEOID field, so can merge with pharmacy points here
                                             variables = "P1_001N",           # total population
                                             state = state_i,                     # list of all states
                                             geometry = FALSE,                    # if false, doesnt read in geometry col with lat/long
                                             output = "wide",                     # may need output = tidy if want to use ggplot for static maps later
                                             year = 2020)
      # bind the result of each iteration together as the consolidated output
      groupspop_dec <-rbind(groupspop_dec,groupspopdec_temp)
    }
    groups_join <- full_join(groupspop_dec, as.data.frame(centroidsgroups), by = "GEOID") # drop ~400 of these bc they are block groups wiht no pop are 100% water
    groups_join2 <- groups_join %>% rowwise() %>%
        mutate(pop_total = P1_001N) %>%
        filter(!is.na(pop_total)) %>%
        ungroup() %>% as.data.frame() %>%
        dplyr::select(GEOID, COUNTYFP, TRACTCE, BLKGRPCE, NAMELSAD, inbuffer05_bin, inbuffer1_bin, inbuffer5_bin, inbuffer10_bin, pop_total)
    
    # create column with population of each block that is in the buffer
    groups_join2$inpop_05mi <- ifelse(groups_join2$inbuffer05_bin == TRUE, groups_join2$pop_total, 0)
    groups_join2$inpop_1mi <- ifelse(groups_join2$inbuffer1_bin == TRUE, groups_join2$pop_total, 0)
    groups_join2$inpop_5mi <- ifelse(groups_join2$inbuffer5_bin == TRUE, groups_join2$pop_total, 0)
    groups_join2$inpop_10mi <- ifelse(groups_join2$inbuffer10_bin == TRUE, groups_join2$pop_total, 0)

    # aggregate to the tract level and sum up populations
    groups_join2$GEOID_tract <- substr(groups_join2$GEOID, 1, 11)
    groups_join3 <- groups_join2 %>% group_by(GEOID_tract) %>%
      summarise(inpop05_total = sum(inpop_05mi),
                inpop1_total = sum(inpop_1mi),
                inpop5_total = sum(inpop_5mi),
                inpop10_total = sum(inpop_10mi)) %>%
      select(GEOID_tract, inpop05_total, inpop1_total, inpop5_total, inpop10_total) %>% 
      as.data.frame()
    
    datafull <- full_join(censusdata6, groups_join3, by = "GEOID_tract") %>% 
      filter(is.na()) %>% 
      as.data.frame()
    
    # create final variables for low-access
    datafull$inpop_urbanicity <- ifelse(datafull$accessradius %in% 0.5, datafull$inpop05_total,
                                        ifelse(datafull$accessradius %in% 1, datafull$inpop1_total,
                                               ifelse(datafull$accessradius %in% 5, datafull$inpop5_total,
                                                      ifelse(datafull$accessradius %in% 10, datafull$inpop10_total, 0)))) #399 being set to 0 because there is 0 pop in there (water only from block groups).
    
    datafull$in_pop_percent <- ifelse(datafull$decpop_total_n == 0, "NA: Zero Population", datafull$inpop_urbanicity/datafull$decpop_total_n) # proportion of ppl who live IN the radius zone
    datafull$out_pop_percent <- 1 - datafull$inpop_urbanicity/datafull$decpop_total_n 
    datafull$out_pop_percent <- ifelse(datafull$in_pop_percent %in% "NA: Zero Population", "NA: Zero Population", datafull$out_pop_percent)
    datafull$low_access_bin <- ifelse(datafull$out_pop_percent > 0.333, 1, 0)
    datafull$low_access_bin <- ifelse(datafull$out_pop_percent %in% "NA: Zero Population", "NA: Zero Population", datafull$low_access_bin) # flag for PD if the prop out of the zone is more than 1/3, akak if the prop IN the zone is < 2/3
  
# 77777777777777777777777777777777777777777777777777777777777777777777
# 77777777777777777777777777777777777777777777777777777777777777777777
# 7 Create Definition of Pharmacy Deserts and Save Final Files #######
# 77777777777777777777777777777777777777777777777777777777777777777777
    # note: of the 85479 -> 2079 tracts have no median income data and FPL is also NA --> can't create low-income flags for these (put NA not 0 for pharmacy desert)
    # also, 552 have NA for median income data but were able to fill out FPL_p. 231 (41.9%) meet low-income flag criteria based on FPL proportion. The others can't be determined.
    
    # Do final cleaning and dropping of columns
    datafull$lowincome_bin <- ifelse(datafull$low_income_medincome_flag == 1 | datafull$fpl_percent_bin == 1, 1, 
                                     ifelse((is.na(datafull$low_income_medincome_flag) & is.na(datafull$fpl_percent_bin)), NA, 0)) # if both income vars are NA, put NA as income flag. if at least one is 1 then put 1. If both are 0 then put 0.
    datafull$ph_per_tract <- ifelse(is.na(datafull$ph_per_tract), 0, datafull$ph_per_tract)
    datafull$ph_per_totalpop <- ifelse(is.na(datafull$ph_per_ktotalpop), 0, datafull$ph_per_ktotalpop)
    datafull$ph_per_adultpop <- ifelse(is.na(datafull$ph_per_kadultpop), 0, datafull$ph_per_kadultpop)
    datafull$ph_per_65pop <- ifelse(is.na(datafull$ph_per_k65pop), 0, datafull$ph_per_k65pop)

    # are there any tracts with no income data but do have ppl? put not determined if so
    # any tracts with no people and 0 pharmacies? put NA
    table(datafull$lowincome_bin, datafull$low_access_bin, useNA = "always")
    # Create pharmacy desert definition: low-income and low-access
    datafull$pharmacydesert_bin <- ifelse((datafull$lowincome_bin %in% 1 & datafull$low_access_bin %in% 1), 1, 0) # this version 12000 fewer pharmacy deserts because many with 
    datafull$pharmacydesert_cat <- datafull$pharmacydesert_bin
    datafull$pharmacydesert_cat <- ifelse(is.na(datafull$lowincome_bin), "NA: No Income Data", datafull$pharmacydesert_cat)
    datafull$pharmacydesert_cat <- ifelse((datafull$low_access_bin %in% "NA: Zero Population"), "NA: Zero Population", datafull$pharmacydesert_cat)
    table(datafull$pharmacydesert_bin, datafull$pharmacydesert_cat, useNA = "always")
    
    # columns of populations living in pharmacy deserts
    datafull$totalpop_phdesert <- ifelse(datafull$pharmacydesert_bin %in% 1, datafull$decpop_total_n, 0)
    datafull$adultpop_phdesert <- ifelse(datafull$pharmacydesert_bin%in%1, datafull$decpop_adult_n, 0)
    datafull$pop65_phdesert <- ifelse(datafull$pharmacydesert_bin%in%1, datafull$decpop_65up_n, 0)
    
    # columns of population for areas that have low access to pharmacies but don't meet the low-income criteria
    datafull$totalpop_lowaccess <- ifelse(datafull$low_access_bin %in% 1, datafull$decpop_total_n, 0)
    datafull$adultpop_lowaccess <- ifelse(datafull$low_access_bin%in%1, datafull$decpop_adult_n, 0)
    datafull$pop65_lowaccess <- ifelse(datafull$low_access_bin%in%1, datafull$decpop_65up_n, 0)
    
    # datafull$urbanicity <- ifelse(datafull$accessradius == 10, 3,
    #                               ifelse(datafull$accessradius == 5, 2,
    #                                      ifelse(datafull$accessradius %in% c(0.5,1), 1, NA)))
    
    table(datafull$urbanicity, datafull$accessradius, useNA = "always")
    # # REMOVE ONCE WERE SET HERE
    # # Check NAs of pharmacy deserts:
    # saveRDS(datafull, file = "datafull_line864.rds")
    # table(datafull$pharmacydesert_bin, useNA = "always") # there are 105 NAs
    #   # all of these have NA for low income, so not able to make a determination. There are 1010 total tracts that have NA for low income, 811 of these have no population, the remaining 199 have populatoin but have no median income or FPL data available.
    write.csv(datafull, "datafull_newradius929.csv") # dont forget to pad zeroes on GEOID if we need to read this in.

# Add labels to the dataset variables
    label(datafull$pop_total) <- "Total Population"
    label(datafull$pop_adult) <- "Adult Population"
    label(datafull$pop_65up_n) <- "Older Adult (65+) Population"
    label(datafull$pop_65up_p) <- "Prop. Older Adult (65+)"
    label(datafull$fpl_p) <- "Prop. Below FPL"
    label(datafull$median_income) <- "Median Income"
    label(datafull$educ_hs_p) <- "Prop. Less Than High School Education"
    label(datafull$race_nh_white_p) <- "Prop. NH, White"
    label(datafull$race_nh_black_p) <- "Prop. NH, Black"
    label(datafull$race_nh_asian_p) <- "Prop. NH, Asian"
    label(datafull$race_nh_aian_p) <- "Prop. NH, AIAN"
    label(datafull$race_nh_2p_p) <- "Prop. NH, 2 or More Races"
    label(datafull$race_nh_other_p) <- "Prop. NH, Other Race"
    label(datafull$race_hisp_white_p) <- "Prop. Hispanic, White Race"
    label(datafull$race_hisp_black_p) <- "Prop. Hispanic, Black Race"
    label(datafull$race_hisp_asian_p) <- "Prop. Hispanic, Asian Race"
    label(datafull$race_hisp_aian_p) <- "Prop. Hispanic, AIAN Race"
    label(datafull$race_hisp_2p_p) <- "Prop. Hispanic, 2 or More Races"
    label(datafull$race_hisp_other_p) <- "Prop. Hispanic, Other Race"
    label(datafull$ins_none_p) <- "Prop. With No Health Insurance"
    label(datafull$ins_public_p) <- "Prop. With Public Health Insurance"
    label(datafull$notenglspeak_p) <- "Prop. Do Not Speak English"
    label(datafull$disability_p) <- "Prop. Ambulatory Disability"
    label(datafull$inequality_gini) <- "GINI Inequality Index"
    label(datafull$pop_density) <- "Population Density"
    datafull$urbanicity_cat <- factor(datafull$urbanicity,
                                             levels = c(1,2,3),
                                             labels = c("Urban",
                                                        "Suburban",
                                                        "Rural"))
    label(datafull$urbanicity_cat) <- "Urbanicity"
    # datafull$pharmacydesert_bin <- factor(datafull$pharmacydesert_bin,
    #                                            levels = c(1,0), # NEED TO CHECK HERE- there should be some PDs that are NA?
    #                                            labels = c("Pharmacy Desert",
    #                                                       "Not Pharmacy Desert"))
    label(datafull$pharmacydesert_bin) <- "Pharmacy Desert Status"
    label(datafull$totalpop_phdesert) <- "Total Population in Pharmacy Desert"
    label(datafull$adultpop_phdesert) <- "Adult Population in Pharmacy Desert"
    label(datafull$pop65_phdesert) <- "Older Adult (65+) Population in Pharmacy Desert"
    
    # Need to get tract polygon data to make a geo
    tract_polygons <- data.frame() # note the loop takes about 2 mins to run
    for (state_i in mystates) {
      tract_polygons_temp <- tidycensus::get_acs(geography = "tract",                 # gets read in with a GEOID field, so can merge with pharmacy points here
                                             variables = "B01001_001E",           # total population
                                             state = state_i,                     # list of all states
                                             geometry = TRUE,                    # if false, doesnt read in geometry col with lat/long
                                             output = "wide",                     # may need output = tidy if want to use ggplot for static maps later
                                             year = 2021,
                                             survey = "acs5")   
      # bind the result of each iteration together as the consolidated output
      tract_polygons <-rbind(tract_polygons, tract_polygons_temp)
    }
    st_crs(tract_polygons) #4269 NAD83 is what census uses
    tract_polygons2 <- st_transform(tract_polygons, # transform this to a geo for use in maps
                                     crs = 4326) %>%  # WSG84 which is what googlemaps uses uses (for mapping)
      mutate(GEOID_tract = GEOID) %>% 
      select(GEOID_tract, geometry)
    
    ### Final saving of census tract data ####
    tractdata_df <- datafull %>% as.data.frame()
    tractdata_sf <- merge(tract_polygons2, (as.data.frame(datafull)))
    
    saveRDS(tractdata_df, file = "tractdata_929_df.rds")
    saveRDS(tractdata_sf, file = "tractdata_929_sf.rds")
    
    # write.csv(tractdata_df, "tractdatadftest.csv")
    
## Final saving of pharmacies data ####    
  # Pharmacies dataset: row = pharmacy but do a  join and add indicator of urbanicity and the pharmacy desert status of that pharmacy
  pdbin_df <- datafull %>% select(GEOID_tract, pharmacydesert_bin) %>% rename(GEOID = GEOID_tract) 
    # NEW STEP HERE save it EXCEPT drop the ones which is.na(urbanicity). werent able to be geocoded
  pharmgeo_df7_df <- left_join(pharmgeo_df5, pdbin_df, by = "GEOID") # merge PD status with the df pharmacies
  pharmgeo_df7_sf <- left_join(pharmgeo_df6, pdbin_df, by = "GEOID") # merge PD status with the sf pharmacies (WGS 84, 4326)

  # Add labels to pharmacy dataset columns
  label(pharmgeo_df7_df$addresses) <- "Address"
  label(pharmgeo_df7_df$ncpdp_id) <- "NCPDP ID"
  label(pharmgeo_df7_df$legal_name) <- "Legal Name"
  label(pharmgeo_df7_df$dba_name) <- "DBA Name"
  label(pharmgeo_df7_df$city) <- "City"
  label(pharmgeo_df7_df$state) <- "State"
  label(pharmgeo_df7_df$county_fips) <- "County FIPS"
  label(pharmgeo_df7_df$msa) <- "MSA ID"
  label(pharmgeo_df7_df$open24h_bin) <- "Open 24 Hours"
  label(pharmgeo_df7_df$dispenserclass_cat) <- "Dispenser Class"
  label(pharmgeo_df7_df$medicare_id) <- "Medicare ID"
  label(pharmgeo_df7_df$npi_id) <- "NPI ID"
  label(pharmgeo_df7_df$maildelivery_bin) <- "Mail Delivery (Binary)"
  label(pharmgeo_df7_df$maildelivery_cat) <- "Mail Delivery Category"
  label(pharmgeo_df7_df$compounding_bin) <- "Compounding Pharmacy (Binary)"
  label(pharmgeo_df7_df$compounding_cat) <- "Compounding Pharmacy Category"
  label(pharmgeo_df7_df$driveup_bin) <- "Drive-up Window Available"
  label(pharmgeo_df7_df$dme_bin) <- "DME Available (Binary)"
  label(pharmgeo_df7_df$dme_cat) <- "DME Availability"
  label(pharmgeo_df7_df$walkinclinic_bin) <- "Walk-in Clinic Available (Binary)"
  label(pharmgeo_df7_df$walkinclinic_cat) <- "Walk-in Clinic Available"
  label(pharmgeo_df7_df$emerg24h_bin) <- "Emergency Services 24 Hours (Binary)"
  label(pharmgeo_df7_df$emerg24h_cat) <- "Emergency Services 24 Hours Availability"
  label(pharmgeo_df7_df$multidosepkg_bin) <- "Multidose Packaging (Binary)"
  label(pharmgeo_df7_df$multidosepkg_cat) <- "Multidose Packaging Availability"
  label(pharmgeo_df7_df$immunizations_bin) <- "Immunization Services (Binary)"
  label(pharmgeo_df7_df$immunizations_cat) <- "Immunization Services Availability"
  label(pharmgeo_df7_df$handicapaccess_bin) <- "ADA Accessibility"
  label(pharmgeo_df7_df$is340b_bin) <- "340b Status (Binary)"
  label(pharmgeo_df7_df$is340b_cat) <- "340b Status Category"
  label(pharmgeo_df7_df$urbanicity) <- "Urbanicity"
  pharmgeo_df7_df$urbanicity_cat <- factor(pharmgeo_df7_df$urbanicity,
                                           levels = c(1,2,3),
                                           labels = c("Urban",
                                                      "Suburban",
                                                      "Rural"))
  label(pharmgeo_df7_df$pharmacydesert_bin) <- "Pharmacy Desert Status"
  pharmgeo_df7_df$pharmacydesert_bin <- factor(pharmgeo_df7_df$pharmacydesert_bin,
                                                levels = c(1,0),
                                                labels = c("Pharmacy Desert",
                                                           "Not Pharmacy Desert"))
  
  # save one version as a data frame and one version as a geo. Same w pharmacy dataframe
  saveRDS(pharmgeo_df7_df, file = "pharmacies_df.rds") # non-geo dataframe for analysis
  saveRDS(pharmgeo_df7_sf, file = "pharmacies_sf.rds") # geo for mapping 4326 / WGS84 (google maps)


# 88888888888888888888888888888888888888888888888888888888888888888
# 88888888888888888888888888888888888888888888888888888888888888888
# 8 Make Summary Tables and Statistical Analyses ##################
# 88888888888888888888888888888888888888888888888888888888888888888

tractdata_df2 <- tractdata_df %>% filter(pharmacydesert_cat %in% c(1,0))

# Table 0: Characteristics of sample
table1::table1(~ pop_adult | urbanicity_cat, data = tractdata_df)

# Table 1: Population living in pharmacy deserts by State  
statenames <- fipscodes %>% select(state_name, state_code) %>% unique() %>% filter(!state_code %in% c(60,66,69,72,74,78))
table1 <- tractdata_df2 %>% group_by(state_fips) %>% 
  rename(state_code = state_fips) %>% 
  full_join(., statenames, by = "state_code") %>% 
  summarise(n_tracts = n(),
            totalpop = sum(decpop_total_n),
            adultpop = sum(decpop_adult_n),
            n_phdeserts = sum(pharmacydesert_bin),
            pop_phdeserts = sum(totalpop_phdesert),
            popadult_phdeserts = sum(adultpop_phdesert),
            p_popdeserts = round(pop_phdeserts/totalpop*100,2),
            padult_popdeserts = round(popadult_phdeserts/adultpop*100,2)) %>% 
  arrange(desc(padult_popdeserts)) %>% 
  right_join(., statenames, by = "state_code") %>% 
  select(state_name, padult_popdeserts, p_popdeserts, pop_phdeserts, n_tracts, n_phdeserts) %>% 
  rename(State = state_name,
         Proportion_Adults_in_Deserts = padult_popdeserts,
         Proportion_Population_in_Deserts = p_popdeserts,
         Number_Population_in_Deserts = pop_phdeserts,
         Number_Census_Tracts = n_tracts,
         Number_Pharmacy_Desert_Tracts = n_phdeserts)
View(table1)
write.csv(table1, "table1.csv")

# Table 2: Characteristics of pharmacy desert neighborhoods
tractdata_df2$pharmacydesert_bin2 <- factor(tractdata_df2$pharmacydesert_bin,
                                          levels = c(1,0), # NEED TO CHECK HERE- there should be some PDs that are NA?
                                          labels = c("Pharmacy Desert",
                                                     "Not Pharmacy Desert"))
table1::table1(~ fpl_p + median_income + educ_hs_p + ins_none_p + ins_public_p + 
                 notenglspeak_p + disability_p + pop_65up_p +
                 urbanicity +
                 race_nh_white_p + race_nh_black_p + race_nh_asian_p + race_nh_aian_p + race_nh_2p_p + race_nh_other_p +
                 race_hisp_white_p + race_hisp_black_p + race_hisp_asian_p + race_hisp_aian_p + race_hisp_2p_p + race_hisp_other_p
               | pharmacydesert_bin, 
               data = tractdata_df2,
               overall = F,
               extra.col=list(`P-value`=pvalue))

# Table 3: Characteristics of pharmacies in pharmacy deserts
pharmacies_df2 <- pharmgeo_df7_df %>% filter(!is.na(pharmacydesert_bin2))
table1::table1(~ urbanicity_cat + dispenserclass_cat + 
               immunizations_cat + handicapaccess_bin + is340b_cat + 
               multidosepkg_cat + emerg24h_cat + walkinclinic_cat + compounding_cat + dme_cat
               | pharmacydesert_bin2,
               data = pharmacies_df2,
               overall = F,
               extra.col=list(`P-value`=pvalue))

pharmtest <- pharmacies_df2 %>% select(pharmacydesert_bin2, immunizations_cat, is340b_cat, handicapaccess_bin) %>% filter(!is.na(pharmacydesert_bin2))
label(pharmtest$pharmacydesert_bin2) <- "Serves Pharmacy Desert"
label(pharmtest$immunizations_cat) <- "Immunization Services"
label(pharmtest$is340b_cat) <- "340b Status"
label(pharmtest$handicapaccess_bin) <- "ADA Accessibility"

table1(~ immunizations_cat + is340b_cat + handicapaccess_bin | 
         pharmacydesert_bin2,
       data = pharmtest)

pvalue <- function(x,...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # for numeric variables, perform t test
    p <- t.test(y~g)$p.value
  } else {
    # for categorical, do chi-squared test
    p <- chisq.test(table(y,g))$p.value
  }
  # Format the p value 
  # The initial empty string places the output on the line below the variable label
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

table1(~ immunizations_cat + is340b_cat + handicapaccess_bin | 
         pharmacydesert_bin2,
       data = pharmtest,
       overall = F,
       extra.col=list(`P-value`=pvalue))

### TODO LIST CODE RUNNING ####
# TODO: Fix labels for things...
# TODO: Add binaries for language spoken codes + speak only english (make supplemental table of this and just mention in the text)
# TODO: Clean up summary table of pharmacies by PD status (should be fine if save as RDS from this currently loaded version)
# TODO: Function for status map images state by state (start w WA)
# TODO: Add BH correction to function for p-values
# TODO: Create function to make table output by state
# TODO: Make step by step list of tract counts and pharmacy counts for flow chart of what data is included
# TODO: Will I use tidycensus to add up all the moe for each estimate as well? What do w this https://walker-data.com/census-r/wrangling-census-data-with-tidyverse-tools.html?q=error#calculating-group-wise-margins-of-error

# 99999999999999999999999999999999999999999999999999999999999999999
# 99999999999999999999999999999999999999999999999999999999999999999
# 9 Maps and visualizations #######################################
# leaflet references: https://rstudio.github.io/leaflet/colors.html
# 99999999999999999999999999999999999999999999999999999999999999999

# Make a map of buffers (DC as a test)
test1 <- leaflet() %>% 
  addTiles() %>% 
  addMeasure(primaryLengthUnit = "meters") %>% 
  addMarkers(data = pharmgeo_df7_sf %>% filter(state %in% "DC")) %>% 
  addPolygons(data = buffers2 %>% 
                filter(state %in% "DC")) %>% 
  addPolygons(data = tract_c %>% 
                filter(STATEFP == 11) %>% 
                st_transform(crs = "+proj=longlat +datum=WGS84"),
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 1) %>% 
  addPolygons(data = counties_c %>% 
                filter(STATEFP == 11) %>% 
                st_transform(crs = "+proj=longlat +datum=WGS84"),
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 1.5)
test1
saveWidget(test1, file = "PharmacyBuffersDC.html", selfcontained = TRUE)

# note: nationwide maps with leaflet take ages to load- practice on just WA first then as a last step have a national one for the paper.
# make a loop of states
popup1 <- paste0(str_extract(pharmgeo_df7_sf$legal_name, "^([^,]*)")) # check that these names and characters are right
# mydata_sf <- tractdata_sf
# mypoints_sf <- pharmgeo_df7_sf

outlinesmap <- leaflet(width = "100%") %>%                     # sets the width of the map
  addProviderTiles(provider = "CartoDB.Positron") %>%        # TODO remind myself what this does
  addPolygons(data = states_c %>% 
                st_transform(crs = "+proj=longlat +datum=WGS84"),
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 1.5) %>% 
  addPolygons(data = counties_c %>%                    
                st_transform(crs = "+proj=longlat +datum=WGS84"), # put my data on WSG84 CRS for mapping in open street maps
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 1) %>% 
  addPolygons(data = tract_c %>% 
                st_transform(crs = "+proj=longlat +datum=WGS84"),
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 0.5)
outlinesmap

# Map to check urban rural distributions:
urbanruralmap <- leaflet(width = "100%") %>%                     # sets the width of the map     
  addProviderTiles(providers$CartoDB.Positron) %>%        
  addPolygons(data = states_c %>% 
                filter(STATEFP == 53) %>% 
                st_transform(crs = "+proj=longlat +datum=WGS84"),
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 1.5) %>% 
  addPolygons(data = counties_c %>%
                filter(STATEFP == 53) %>%
                st_transform(crs = "+proj=longlat +datum=WGS84"), # put my data on WSG84 CRS for mapping in open street maps
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 1) %>% 
  addPolygons(data = tract_c %>%
                filter(STATEFP == 53) %>%
                st_transform(crs = "+proj=longlat +datum=WGS84"),
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 0.5) %>%
  addPolygons(data = tractdata_sf %>%
                filter(state_fips %in% 53) %>% 
                st_transform(crs = "+proj=longlat +datum=WGS84"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              fillColor = ~pal(accessradius)) %>%
  addCircleMarkers(data = mypoints_sf_state$geometry, # is already in WSG84
                   fillColor = "springgreen3",
                   radius = 3,
                   color = "black",
                   weight = 0.1,
                   fillOpacity = 1,
                   popup = mypoints_sf_state$legal_name) %>% 
  addPolygons(data = buffers2 %>% 
                filter(state %in% "WA"),
              weight = 0.3,
              fillColor = "orange",
              fillOpacity = 0.05,
              color = "red")
  # addLegend("bottomright",
  #           pal = pal,
  #           values = ~ accessradius,
  #           title = "Urbanicity",
  #           opacity = 1)
urbanruralmap

## pharmacy desert map by state
tractdata_sf_state <- tractdata_sf %>% filter(state_fips == 53)
pharmgeo_df7_sf_state <- pharmgeo_df7_sf %>% filter(state %in% "WA")
popup2 <- paste0(str_extract(tractdata_sf_state$tract_name, "^([^,]*)"))
pharmacydesertmapWA <- leaflet(width = "100%") %>%                     # sets the width of the map     
  addProviderTiles(providers$CartoDB.Positron) %>%        
  addPolygons(data = states_c %>% 
                filter(STATEFP == 53) %>% 
                st_transform(crs = "+proj=longlat +datum=WGS84"),
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 1.5) %>% 
  addPolygons(data = counties_c %>%
                filter(STATEFP == 53) %>%
                st_transform(crs = "+proj=longlat +datum=WGS84"), # put my data on WSG84 CRS for mapping in open street maps
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 1) %>% 
  addPolygons(data = tract_c %>%
                filter(STATEFP == 53) %>%
                st_transform(crs = "+proj=longlat +datum=WGS84"),
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 0.5) %>%
  addPolygons(data = tractdata_sf %>% filter(state_fips %in% 53) %>% filter(low_access_bin == 1), # change mydata from NAD83 to WSG84 for mapping in OpenStreetMaps
              popup = popup2,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7, 
              color = "lightgrey") %>% 
  addPolygons(data = tractdata_sf %>% filter(state_fips %in% 53) %>% filter(pharmacydesert_bin == 1), # change mydata from NAD83 to WSG84 for mapping in OpenStreetMaps
              popup = popup2,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7, 
              color = "salmon") %>% 
  addCircleMarkers(data = pharmgeo_df7_sf_state$geometry, # is already in WSG84
                   fillColor = "springgreen3",
                   radius = 3,
                   color = "black",
                   weight = 0.1,
                   fillOpacity = 1,
                   popup = paste0(pharmgeo_df7_sf_state$legal_name, 
                                  " (NCPDP ID: ", 
                                  pharmgeo_df7_sf_state$ncpdp_id, ")")) %>% 
  addLegend(position = "bottomright",
            colors = c("lightgrey", "salmon"), opacity = 0.7,
            labels = c("Low Access (But not Low Income)", "Pharmacy Desert"))
pharmacydesertmapWA

# investigating the two in the sodo industrial area:
# METHODIST HOSPITAL (NCPDP ID: 5930284)
p1 <- pharmgeo_df7_df %>% filter(ncpdp_id == 4938378) 
p1

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# X Archive Code ##################################################
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

###### MOE for census ####

# NOT SURE APPROACH FOR MEOs YET- come back to this.
# censusdata2_tidytest <- censusdata %>% rowwise() %>% 
#   mutate(pop_total = B01001_001E,
#          pop_adult_est = sum(c(B01001_007E,B01001_008E,B01001_009E,B01001_010E,B01001_011E,B01001_012E,B01001_013E,B01001_014E,B01001_015E,B01001_016E,B01001_017E,B01001_018E,B01001_019E,B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,
#                                B01001_031E,B01001_032E,B01001_033E,B01001_034E,B01001_035E,B01001_036E,B01001_037E,B01001_038E,B01001_039E,B01001_040E,B01001_041E,B01001_042E,B01001_043E,B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E)),
#          pop_adult_moe = moe_sum(c(B01001_007E,B01001_008E,B01001_009E,B01001_010E,B01001_011E,B01001_012E,B01001_013E,B01001_014E,B01001_015E,B01001_016E,B01001_017E,B01001_018E,B01001_019E,B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,
#                                    B01001_031E,B01001_032E,B01001_033E,B01001_034E,B01001_035E,B01001_036E,B01001_037E,B01001_038E,B01001_039E,B01001_040E,B01001_041E,B01001_042E,B01001_043E,B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E)),
#          race_nh_white_p_est = B03002_003E/B03002_001E,
#          race_nh_white_p_moe = moe_product(B03002_003E,B03002_001E,B03002_003M,B03002_001M))
#          
# 
# censusdata2_tidy <- censusdata %>% rowwise() %>% 
#   mutate(pop_total = B01001_001E,
#          pop_adult_est = sum(c(B01001_007E,B01001_008E,B01001_009E,B01001_010E,B01001_011E,B01001_012E,B01001_013E,B01001_014E,B01001_015E,B01001_016E,B01001_017E,B01001_018E,B01001_019E,B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,
#                            B01001_031E,B01001_032E,B01001_033E,B01001_034E,B01001_035E,B01001_036E,B01001_037E,B01001_038E,B01001_039E,B01001_040E,B01001_041E,B01001_042E,B01001_043E,B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E)),
#          pop_adult_moe = moe_sum(c(B01001_007E,B01001_008E,B01001_009E,B01001_010E,B01001_011E,B01001_012E,B01001_013E,B01001_014E,B01001_015E,B01001_016E,B01001_017E,B01001_018E,B01001_019E,B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,
#                            B01001_031E,B01001_032E,B01001_033E,B01001_034E,B01001_035E,B01001_036E,B01001_037E,B01001_038E,B01001_039E,B01001_040E,B01001_041E,B01001_042E,B01001_043E,B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E)),
#          pop_65up_n = sum(c(B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,
#                             B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E)),
#          pop_65up_p = pop_65up_n/pop_total,
#          fpl_p = B17001_002E/B17001_001E, 
#          fpl_percent_bin = ifelse(fpl_p >= .2, 1, 0),
#          lowveh_bin = ifelse(B08201_001E < 100, 1, 0),
#          median_income = B19013_001E,
#          educ_hs_p = sum(c(B15001_006E, B15001_014E, B15001_022E, B15001_030E, B15001_038E, B15001_047E, B15001_055E, B15001_063E, B15001_071E, B15001_079E))/B15001_001E,
#          race_nh_white_p = B03002_003E/B03002_001E,
#          race_nh_black_p = B03002_004E/B03002_001E,
#          race_nh_aian_p = B03002_005E/B03002_001E,
#          race_nh_asian_p = B03002_006E/B03002_001E,
#          race_nh_nhpi_p = B03002_007E/B03002_001E,
#          race_nh_other_p = B03002_008E/B03002_001E,
#          race_nh_2p_p = B03002_009E/B03002_001E,
#          race_hisp_white_p = B03002_013E/B03002_001E,
#          race_hisp_black_p = B03002_014E/B03002_001E,
#          race_hisp_aian_p = B03002_015E/B03002_001E,
#          race_hisp_asian_p = B03002_016E/B03002_001E,
#          race_hisp_nhpi_p = B03002_017E/B03002_001E,
#          race_hisp_other_p = B03002_018E/B03002_001E,
#          race_hisp_2p_p = B03002_019E/B03002_001E,
#          ins_popadult = sum(c(B27001_009E, B27001_012E, B27001_015E,B27001_018E,B27001_021E, B27001_024E,B27001_027E, B27001_037E,B27001_040E,B27001_043E,B27001_046E,B27001_049E,B27001_052E,B27001_055E)),
#          ins_none_p = sum(c(B27001_011E,B27001_014E, B27001_017E,B27001_020E,B27001_023E,B27001_026E,B27001_029E,B27001_039E,B27001_042E,B27001_045E,B27001_048E,B27001_051E,B27001_054E,B27001_057E))/ins_popadult,
#          ins_public_p = sum(c(B27003_010E,B27003_013E,B27003_016E, B27003_019E,B27003_022E,B27003_025E,B27003_028E,B27003_038E,B27003_041E,B27003_044E, B27003_047E,B27003_050E,B27003_053E,B27003_056E))/ins_popadult,
#          notenglspeak_p = sum(c(B16004_029E, B16004_030E, B16004_051E, B16004_052E))/sum(c(B16004_024E,B16004_046E)),
#          disability_p = sum(c(B18105_007E,B18105_010E,B18105_013E,B18105_016E,B18105_023E,B18105_026E,B18105_029E,B18105_032E))/(B18105_001E - sum(B18105_005E, B18105_019E)),
#          inequality_gini = B19083_001E,
#   ) %>% 
#   ungroup() %>% as.data.frame() 
# 
# 
# # TODO: figure out what to drop- if we drop all tracts/estimates where moe>estimate, drop a lotttt of data because we're requesting things like number of females who are 21 years old in each tract, very detailed.
# censusvarsNAtest <- censusdata %>% select(-geometry) %>% as.data.frame()
# for (var in census_vars) {
#   colname_estimate <- paste0(var,"E")
#   colname_moe <- paste0(var,"M")
#   censusvarsNAtest[,colname_estimate] <- ifelse(censusvarsNAtest[,colname_moe] > censusvarsNAtest[,colname_estimate], 
#                                                 NA,
#                                                 censusvarsNAtest[,colname_estimate])
# }
# 
# censusvarstest <- censusdata2 %>% select(-geometry)
# write.csv(censusvarstest, "censusdfWAonlytest.csv")

# # Check if ZIPs are available so could use florida data
# censusdata_FLzip <- tidycensus::get_acs(geography = "zcta",          # gets read in with a GEOID field, so can merge with pharmacy points here
#                                        variables = "B19013_001",            # income 
#                                        geometry = FALSE,                    # if false, doesnt read in geometry col with lat/long
#                                        output = "wide",                     # may need output = tidy if want to use ggplot for static maps later
#                                        year = 2021,
#                                        survey = "acs5")

###### RUCA Codes ####
# Using RUCA Codes for urban/rural definitions:
# RUCA codes from USDA https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes.aspx . 
# # As of June 2023 only 2010 RUCA codes are available- 2020 RUCAs will be available in Dec 2023 at earliest
# ruca_df <- readxl::read_excel(paste0(inputDir,"ruca2010revised.xlsx"), sheet = 1, skip = 1) %>% 
#   rename(county_FIPS = `State-County FIPS Code`,
#          state = `Select State`,
#          tract_FIPS = `State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`,
#          ruca_1 = `Primary RUCA Code 2010`,
#          ruca_2 = `Secondary RUCA Code, 2010 (see errata)`) %>% 
#   select(-`Tract Population, 2010`,-`Land Area (square miles), 2010`,-`Population Density (per square mile), 2010`)
# Merge the census tract data with RUCA data and define urban, suburban, rural tracts. Ruca 2020 codes not available until December 2023 at earliest: https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes.aspx
ruca_df$GEOID_tract <- as.character(ruca_df$tract_FIPS)
mydata_sf <- full_join(censusdata4, ruca_df, by = "GEOID_tract") %>% 
  mutate(urbanicity = case_when(ruca_1 %in% c(1,2,3) ~ 1,       # urban
                                ruca_1 %in% c(4,5) ~ 2,         # suburban
                                ruca_1 %in% c(6,7,8,9,10) ~ 3,  # rural
                                ruca_1 %in% "99" ~ NA))         # no population / not evaluated
# why are there 22621 tracts that dont have a ruca assigned to them. change from decennial census? Answer (checked below) yes this is the issue

# Check the list of tracts between years of survey.
tract_2010 <- data.frame() # note the loop takes about 2 mins to run
for (state_i in mystates) {
  # get the median incomes by tract in each state and store it in a temporary dataframe
  tract_2010_temp <- tidycensus::get_acs(geography = "tract",                 # gets read in with a GEOID field, so can merge with pharmacy points here
                                         variables = "B01001_001E",           # total population
                                         state = state_i,                     # list of all states
                                         geometry = FALSE,                    # if false, doesnt read in geometry col with lat/long
                                         output = "wide",                     # may need output = tidy if want to use ggplot for static maps later
                                         year = 2011,
                                         survey = "acs5")   
  # bind the result of each iteration together as the consolidated output
  tract_2010 <-rbind(tract_2010,tract_2010_temp)
}
tract_2021 <- data.frame() # note the loop takes about 2 mins to run
for (state_i in mystates) {
  # get the median incomes by tract in each state and store it in a temporary dataframe
  tract_2021_temp <- tidycensus::get_acs(geography = "tract",                 # gets read in with a GEOID field, so can merge with pharmacy points here
                                         variables = "B01001_001E",           # total population
                                         state = state_i,                     # list of all states
                                         geometry = FALSE,                    # if false, doesnt read in geometry col with lat/long
                                         output = "wide",                     # may need output = tidy if want to use ggplot for static maps later
                                         year = 2021,
                                         survey = "acs5")   
  # bind the result of each iteration together as the consolidated output
  tract_2021 <-rbind(tract_2021,tract_2021_temp)
}
tract_2010$GEOID_2010 <- tract_2010$GEOID
tract_2021$GEOID_2021 <- tract_2021$GEOID
tract_compare <- full_join(tract_2010, tract_2021, by= "GEOID")
tract_compare$cat <- ifelse(is.na(tract_compare$GEOID_2010) & !is.na(tract_compare$GEOID_2021), "2021 only", 
                            ifelse(is.na(tract_compare$GEOID_2021) & !is.na(tract_compare$GEOID_2010), "2010 only",
                                   ifelse(!is.na(tract_compare$GEOID_2010) & !is.na(tract_compare$GEOID_2021), "Both", NA)))
table(tract_compare$cat, useNA = "always").
# Result: 11425 are 2010-only, 22783 are 2021-only, 61,631 are both. 
# Conclusion: Need new way to determine urban/rural unless I wait to do this analysis in December --> no

# Decision: Can't use RUCA codes to determine urbanicity. The 2020 ones won't be out until December at the earliest. Think of something else in the meantime.
# Use pop density: if > 5000 person / sq mile then is urban, > 1000 ppsq = suburban, <1000 = rural. Helpful visualization: https://mtgis-portal.geo.census.gov/arcgis/apps/MapSeries/index.html?appid=2566121a73de463995ed2b2fd7ff6eb7

###### Decennial population data ####
# # Getting true pop counts from decennial for census tracts and for block groups. Explore whether they tend to be bigger or smaller.
dectotalpop_tract <- data.frame()
for (state_i in mystates) {
  # get the median incomes by tract in each state and store it in a temporary dataframe
  dectotalpop_tract_temp <- get_decennial(geography = "tract",
                                          variables = "P12_001N",
                                          state = state_i,
                                          geometry = FALSE,
                                          output = "wide",
                                          year = 2020)
  # bind the result of each iteration together as the consolidated output
  dectotalpop_tract <-rbind(dectotalpop_tract, dectotalpop_tract_temp)
}
# merge in the decennial pop tract

# test:
testdectotalpop <- get_decennial(geography = "tract",
                                        variables = "P1_001N",
                                        state = 53,
                                        geometry = FALSE,
                                        output = "wide",
                                        year = 2020)

# trying w other package: https://www.hrecht.com/censusapi/articles/getting-started.html
apis <- listCensusApis()
colnames(apis)
dec_vars <- listCensusMetadata(
  name = "2020/dec/dhc", 
  type = "variables")
listCensusMetadata(
  name = "2020/dec/dhc", 
  type = "geography")



test2 <- getCensus(name = "dec/dhc", vintage = 2020, vars = "P12_001N", region = "state:*", key = my_api_key)
test3 <- get_decennial(sumfile = "dhc",
                       year = 2020,
                       geography = "tract",
                       state = "53",
                       output = "wide",
                       variables = variables_dec,
                       geometry = FALSE)


# Getting true pop counts from decennial for census tracts and for block groups. Explore whether they tend to be bigger or smaller.
decennialpop_tract <- data.frame()
for (state_i in mystates) {
  # get the median incomes by tract in each state and store it in a temporary dataframe
  decennialpop_tract_temp <- get_decennial(geography = "tract",
                                           variables = "P12_001N", 
                                           state = state_i,
                                           geometry = FALSE,
                                           output = "wide",
                                           year = 2020)
  # bind the result of each iteration together as the consolidated output
  decennialpop_tract <-rbind(decennialpop_tract, decennialpop_tract_temp)
}

# join these and see whether in general tracts or decennial has more
head(decennialpop_tract)
nrow(decennialpop_tract)
head(tract_2021)
nrow(tract_2021)

decennialpop_tract <- decennialpop_tract %>% 
  rename(tractname_dec = NAME,
         pop_dec = P1_001N) %>%
  mutate(GEOID_dec = GEOID)
tract_2021 <- tract_2021 %>% 
  rename(tractname_acs = NAME,
         pop_acs = B01001_001E,
         pop_acs_moe = B01001_001M) %>% 
  mutate(GEOID_acs = GEOID)

popcompare <- full_join(decennialpop_tract, tract_2021, by = "GEOID") # the exact number stays the same 84414
summary(popcompare$pop_dec)
summary(popcompare$pop_acs)
popcompare$dec_bigger_bin <- ifelse(popcompare$pop_dec > popcompare$pop_acs, 1, 0)
summary(popcompare$dec_bigger_bin) # has more about half the time, as expected. no NAs


###### In Buffer Population by Block Group ####

# # old version: calculate proportion in radius with block groups
# groups_c_trans <- st_transform(groups_c, crs = st_crs(buffers2)) # make census block group polys are same crs as the buffer df
# identical(st_crs(groups_c_trans),st_crs(buffers2)) # checking, output is TRUE
# centroidsgroups <- st_centroid(groups_c_trans) # create a df of the points that are the geometric centroids of each block group
# centroidsgroups$inbuffer_bin <- st_within(centroidsgroups, buffers2) %>% lengths > 0 # define: is the blkgrp centroid in any buffer? Likely will have to do this in loops again

# groupspop <- data.frame() # note the loop takes about 2 mins to run
# for (state_i in mystates) {
#   # get the median incomes by tract in each state and store it in a temporary dataframe
#   groupspop_temp <- tidycensus::get_acs(geography = "block group",                 # gets read in with a GEOID field, so can merge with pharmacy points here
#                                          variables = "B01001_001E",           # total population
#                                          state = state_i,                     # list of all states
#                                          geometry = TRUE,                    # if false, doesnt read in geometry col with lat/long
#                                          output = "wide",                     # may need output = tidy if want to use ggplot for static maps later
#                                          year = 2021,
#                                          survey = "acs5")   
#   # bind the result of each iteration together as the consolidated output
#   groupspop <-rbind(groupspop,groupspop_temp)
# }
 

# 
# blockspop_dec <- data.frame() # note the loop takes about 2 mins to run. Don't need this loop anymore for block pops
# for (state_i in mystates) {
#   # get the median incomes by tract in each state and store it in a temporary dataframe
#   blockspop_dec_temp <- tidycensus::get_decennial(geography = "block",                 # gets read in with a GEOID field, so can merge with pharmacy points here
#                                         variables = "P1_001N",                         # total population
#                                         state = state_i,                               # list of all states
#                                         geometry = TRUE,                               # if false, doesnt read in geometry col with lat/long
#                                         output = "wide",
#                                         year = 2020)   
#   # bind the result of each iteration together as the consolidated output
#   blockspop_dec <-rbind(blockspop_dec,blockspop_dec_temp)
# }
# 

#   # get population data for 24 and 36
# blockspop_2436 <- data.frame() # note the loop takes about 2 mins to run. Don't need this loop anymore for block pops
# for (state_i in mystates2436) {
#   # get the median incomes by tract in each state and store it in a temporary dataframe
#   blockspop_dec_temp <- tidycensus::get_decennial(geography = "block",                 # gets read in with a GEOID field, so can merge with pharmacy points here
#                                                   variables = "P1_001N",                         # total population
#                                                   state = state_i,                               # list of all states
#                                                   geometry = FALSE,                               # if false, doesnt read in geometry col with lat/long
#                                                   output = "wide",
#                                                   year = 2020)   
#   # bind the result of each iteration together as the consolidated output
#   blockspop_2436 <-rbind(blockspop_2436,blockspop_dec_temp)
# }
# # join the population data with the centroid data for 24 and 36
# 
# # DF for centroid point, GEOID, and population
# #rbind so end up with 1 dataset that has GEOID, centroidpoint, and population
#   # basically can use inbufferbin instead of this groupsjoin step (so start line 704 creating in_pop), then eventually join it with censusdata6 on line 711 there.
#   groups_join <- full_join(groupspop, as.data.frame(centroidsgroups), by = "GEOID") # drop ~400 of these bc they are block groups wiht no pop are 100% water
#   groups_join2 <- groups_join %>% rowwise() %>% 
#     mutate(pop_total = B01001_001E) %>% 
#     filter(!is.na(pop_total)) %>% 
#     ungroup() %>% as.data.frame() %>% 
#     dplyr::select(GEOID, COUNTYFP, TRACTCE, BLKGRPCE, NAMELSAD, inbuffer_bin, pop_total)
# groups_join2$in_pop <- ifelse(groups_join2$inbuffer_bin == TRUE, groups_join2$pop_total, 0)
# 
# # agregate to the tract level and sum up populations
# groups_join2$GEOID_tract <- substr(groups_join2$GEOID, 1, 11)
# groups_join3 <- groups_join2 %>% group_by(GEOID_tract) %>% 
#   summarise(in_pop_total = sum(in_pop)) %>% 
#   select(GEOID_tract, in_pop_total) 

