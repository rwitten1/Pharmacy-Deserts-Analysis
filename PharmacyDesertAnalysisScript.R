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
## 4. Create dataset for each row = census tract <- resume here

## 5. Define low income (census variables)
## 6. Define low access
## 7. Define pharmacy deserts in table
## 8. Create map of pharmacy deserts


# 1111111111111111111111111111111111111111111111111111111111111111111111111
# 1111111111111111111111111111111111111111111111111111111111111111111111111
# Load packages and data files needed for analysis
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

# Read in shape files. Do a cache = TRUE
  # use Tigris to read in shapefiles directly using Census API (no local files needed)
  counties_c <- tigris::counties(cb = TRUE, year = 2020)
  tract_c <- tigris::tracts(cb = TRUE, year = 2020) # default format is sf but can also set class = "sp" if needed. see help function for gg mapping
  groups_c <- tigris::block_groups(cb = TRUE, year = 2020)

# 2222222222222222222222222222222222222222222222222222222222222222222222222
# 2222222222222222222222222222222222222222222222222222222222222222222222222
# Geocode pharmacy addresses (assign lat and lon)
# 2222222222222222222222222222222222222222222222222222222222222222222222222
# # DONT NEED TO DO THESE STEPS AGAIN -------------------------------------

# # Merge NCPDP data tables
  # pharmacy_df <- full_join(providerinfo_df, services_df, by = "NCPDP Provider ID")
# # Drop territories and pharmacies that are not community pharmacies:
#   pharmbystate_df <- pharmacy_df %>% group_by(`Physical Address State`) %>%
#     dplyr::summarise(n_pharm = n()) %>%
#     filter(!`Physical Address State` %in% c("PR", "GU", "VI", "MP", "NA")) # removing territories except DC. (PR 1041; GU 24; VI 20; MP 9).
#   table(pharmacy_df$`Primary Provider Type Code`, useNA = "always") # get numbers of pharmacy by type
#   pharmacy_df <- pharmacy_df %>% filter(`Primary Provider Type Code` == 1) # then filter to just include community pharmacies
# 
# # Geocode all community pharmacies in 50 US states:
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

# 3333333333333333333333333333333333333333333333333333333333333333333333333
# 3333333333333333333333333333333333333333333333333333333333333333333333333
# Finalize dataset for each row = pharmacy
# 3333333333333333333333333333333333333333333333333333333333333333333333333
  # read in df with geocoding complete: work with this data from now on
  pharmgeo_df <- read.csv("pharmacy_geo.csv")[,-1] 

# Clean up names, classes, drop unnecessary columns
  pharmgeo_df2 <- pharmgeo_df %>% 
    filter(Closed.Door.Facility.Indicator != "Y", # removed 1156 closed door facilities here as well
           Dispenser.Class.Code != "7") %>%       # removed 288 alternate dispensing sites as well
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
    # look up the individual addresses for these 34 observationsin Google Maps
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
    # rm(result, providerinfo_df, pharmgeo_df, pharmgeo_df2, pharmacy_df, pharmacy_df1, pharmacy_df2, pharmacy_df2_2, test, missingadddresses, missingaddresses2)
    
    
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
    pharmbystate <- pharmgeo_df3 %>% group_by(state) %>% dplyr::summarise(n_pharm = n()) # count of pharmacies by state
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
    # save as a csv so don't have to run this loop every time
    write.csv(pharmgeo_df4, "pharmgeo_df4.csv")
    pharmgeo_df4 <- read.csv("pharmgeo_df4.csv")[,-1]
  
    
# 4444444444444444444444444444444444444444444444444444444444444444444444444
# 4444444444444444444444444444444444444444444444444444444444444444444444444
# Create dataset for each row = census tract
# 4444444444444444444444444444444444444444444444444444444444444444444444444


 
# Read in census variables (from: https://api.census.gov/data/2021/acs/acs5/variables.html)
    census_vars <- c(
        # population by age and gender
        "B01001_001", "B01001_007", "B01001_008", "B01001_009", "B01001_010","B01001_011","B01001_012","B01001_013","B01001_014","B01001_015","B01001_016","B01001_017","B01001_018","B01001_019","B01001_020","B01001_021","B01001_022","B01001_023","B01001_024","B01001_025",
        "B01001_031", "B01001_032", "B01001_033", "B01001_034","B01001_035","B01001_036","B01001_037","B01001_038","B01001_039","B01001_040","B01001_041","B01001_042","B01001_043","B01001_044","B01001_045","B01001_046","B01001_047","B01001_048","B01001_049",
        # household income, below FPL, vehicle ownership:
        "B19013_001E", "B17001_001E", "B17001_002E", "B17001_031E", "B08201_001E",
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
          # any: coverage by sex by age:
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
    
    # call the census API to get table read in
    censusdata <- tidycensus::get_acs(geography = "tract",        # gets read in with a GEOID field, so can merge with pharmacy points here
                                      variables = census_vars,    # read in list of variables in the vector above, will read the estimate (E) and moe (M)
                                      geometry = TRUE,            # if false, doesnt read in geometry col with lat/long
                                      output = "wide",            # may need output = tidy if want to use ggplot for static maps later
                                      year = 2021)                # using the 2021 ACS which is now available
    
    # create pharmacy desert variables
    # TODO: add in all needed estiamtes calculations
    # TODO: then use tidycensus to add up all the moe for each estimate as well: https://walker-data.com/census-r/wrangling-census-data-with-tidyverse-tools.html?q=error#calculating-group-wise-margins-of-error
    # TODO: then put NA for tracts where MOE > estimates- investigate this more. Replace with county proportions or is that worse?
    censusdata2 <- censusdata %>% rowwise() %>% 
      mutate(pop_total = B01001_001E,
             pop_adult = sum(c(B01001_007E,B01001_008E,B01001_009E,B01001_010E,B01001_011E,B01001_012E,B01001_013E,B01001_014E,B01001_015E,B01001_016E,B01001_017E,B01001_018E,B01001_019E,B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,
                               B01001_031E,B01001_032E,B01001_033E,B01001_034E,B01001_035E,B01001_036E,B01001_037E,B01001_038E,B01001_039E,B01001_040E,B01001_041E,B01001_042E,B01001_043E,B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E)),
             pop_65up = sum(c(B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,
                              B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E))) %>% 
      ungroup() %>% as.data.frame()
    censusdata2$fpl_percent <- censusdata2$B17001_002E/censusdata2$B17001_001E*100
    censusdata2$fpl_percent_bin <- ifelse(censusdata2$fpl_percent >= 20, 1, 0)
    censusdata2$lowveh_bin <- ifelse(censusdata2$B08201_001E < 100, 1, 0)
    censusdata2$med_income <- censusdata2$B19013_001E
    
    censusdata4 <- st_as_sf(censusdata2, # transform this to a geo for use in maps
                            crs = 4269)
    
# After this we deal with pharmacies and census tracts
    # Group pharmgeo by by GEOID for census tract, count number of pharmacies per census tract
    # group by census tracts and count how many pharmacies there are
    pharm_to_tract <- pharmgeo_df4 %>%
      select(GEOID, ncpdp_id) %>%
      group_by(GEOID) %>%
      summarise(ph_per_tract = n()) %>%
      arrange(desc(ph_per_tract))
    pharm_to_tract <- as.data.frame(pharm_to_tract)
    # 35,785 tracts have pharmacies, out of 84,414 tracts in the US total

    # join pharmacies dataframe wo ruca codes and census dataframe

# Can now calculate buffers around each pharmacy using urban/rural ID
# Buffer creation, identify block groups in each radius

    
# last- create binary variables for low-income and low-access
  
