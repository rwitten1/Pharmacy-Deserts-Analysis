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
    # TODO: then use tidycensus to add up all the moe for each estimate as well: https://walker-data.com/census-r/wrangling-census-data-with-tidyverse-tools.html?q=error#calculating-group-wise-margins-of-error
    # TODO: then put NA for tracts where MOE > estimates- investigate this more. Replace with county proportions or is that worse?
    
    # estimates: (test out second table for same calcs but with MOE? then evaluate if pop_adult_e > pop_adult_m ?)
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
    
    
    # Get income of nearest metro area ( # TODO what do if rural? put NA because there is no nearby metro area. Get list of metro areas)
    income_county <- tidycensus::get_acs(geography = "county",                # gets read in with a GEOID field, so can merge with pharmacy points here
                                         variables = "B19013_001E",           # median income
                                         geometry = FALSE,                    # if false, doesnt read in geometry col with lat/long
                                         output = "wide",                     # may need output = tidy if want to use ggplot for static maps later
                                         year = 2021)                         # use 2021 ACS
    
    # Merge with RUCA data and define urban, suburban, rural tracts
    rucadf$GEOID <- as.character(rucadf$GEOID)
    mydata_sf <- full_join(censusdata2, rucadf, by = "GEOID") %>% 
      mutate(urbanicity = case_when(Primary.RUCA.Code.2010 %in% c(1,2,3) ~ 1,       # urban
                                    Primary.RUCA.Code.2010 %in% c(4,5) ~ 2,         # suburban
                                    Primary.RUCA.Code.2010 %in% c(6,7,8,9,10) ~ 3,  # rural
                                    Primary.RUCA.Code.2010 %in% "99" ~ NA),         # no population / not evaluated
             access_radius = case_when(urbanicity = 1 ~ 1,
                                       urbanicity = 2 ~ 5,
                                       urbanicity = 3 ~ 10,
                                       urbanicity = NA ~ NA,
                                       lowvehbin = 1 ~ 0.5)) # TODO check that this goes in order such that lowvehbin is the last thing defined
    
    # put this back as a geo for use in mapping
    censusdata4 <- st_as_sf(mydatasf, # transform this to a geo for use in maps
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
    

# Back to the pharmacy dataset: take the pharmacy dataset and add the urban/rural radius information to it so we can calculate the buffers
    
    # Define the buffers
    radius_km <- swfscMisc::convert.distance(c(0.5, 1, 5, 10), from = c("mi"), to = c("km"))
    radius_km*1000
    
    buffers <- st_transform(pharmgeo_df4, crs = 3857) #transform to a different projection that uses meters as unit of distance
    buffers_0.5mi <- buffers %>% filter(access_radius == 0.5) %>% st_buffer(dist = 804.672) #804.672 is 0.5 mile in meters, as calculated in line 500 above
    buffers_1mi <- buffers %>% filter(access_radius == 1) %>% st_buffer(dist = 1609.344) #1609.34 is 1 mile in meters
    buffers_5mi <- buffers %>% filter(access_radius == 5) %>% st_buffer(dist = 8046.720) #8046.720 is 5 miles in meters
    buffers_10mi <- buffers %>% filter(access_radius == 10) %>% st_buffer(dist = 16093.440) #10mi in meters
    buffers2 <- rbind(buffers_0.5mi, buffers_1mi, buffers_5mi, buffers_10mi)
    buffers2 <- st_transform(buffers2, crs = 4326) # transform back to wsg84 to use with leaflet
    
    groups_c_trans <- st_transform(groups_c, crs = st_crs(buffers2)) # just make sure the census block group polys are same crs as the buffer df
    centroidsgroups <- st_centroid(groups_c_trans) # create a df of the points that are the centroids of each block
    centroidsgroups$inbuffer_bin <- st_within(centroidsgroups, buffers2) %>% lengths > 0 # define: is the blkgrp centroid in any buffer? Likely will have to do this in loops again

    # Get the populations of each block group -- Likely will have to make this into a loop
    groupspop <- tidycensus::get_acs(geography = "block group",   # gets read in with a GEOID field, so can merge with pharmacy points here
                                     variables = census_vars,
                                     geometry = TRUE, # if false, doesnt read in geometry col with lat/long
                                     output = "wide") 
    
    groups_join <- full_join(groupspop, as.data.frame(centroidsgroups), by = "GEOID")
    groups_join2 <- groups_join %>% rowwise() %>% 
      mutate(pop_total = B01001_001E,
             pop_adult = sum(c(B01001_007E,B01001_008E,B01001_009E,B01001_010E,B01001_011E,B01001_012E,B01001_013E,B01001_014E,B01001_015E,B01001_016E,B01001_017E,B01001_018E,B01001_019E,B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,
                               B01001_031E,B01001_032E,B01001_033E,B01001_034E,B01001_035E,B01001_036E,B01001_037E,B01001_038E,B01001_039E,B01001_040E,B01001_041E,B01001_042E,B01001_043E,B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E)),
             pop_65up = sum(c(B01001_020E,B01001_021E,B01001_022E,B01001_023E,B01001_024E,B01001_025E,
                              B01001_044E,B01001_045E,B01001_046E,B01001_047E,B01001_048E,B01001_049E))) %>% 
      ungroup() %>% as.data.frame() %>% 
      dplyr::select(GEOID, NAME, COUNTYFP, TRACTCE, BLKGRPCE, NAMELSAD, inbuffer_bin, pop_total, pop_adult, pop_65up)
    
    # column for the population of each block group whose centroid is in a pharmacy buffer:
    groups_join2$in_pop <- ifelse(groups_join2$inbuffer_bin == TRUE, groups_join2$pop_adult, 0)
    
    groups_join2$GEOID10 <- substr(groups_join2$GEOID, 1, 11)
    groups_join3 <- groups_join2 %>% group_by(GEOID10) %>% summarise(in_pop_total = sum(in_pop)) %>% 
      mutate(GEOID = GEOID10) %>% select(GEOID, in_pop_total) 
    
    datafull <- full_join(mydata_sf, groups_join3, by = "GEOID") # TODO make sure here I'm joining with the census / full remaining data
    
    # create final variables for low-access
    datafull$in_pop_percent <- datafull$in_pop_total/datafull$pop_adult
    datafull$in_pop_percent_flag <- ifelse(datafull$in_pop_percent <0.667 | datafull$in_pop_total < 500, 1, 0)
    
    # Do final cleaning and dropping of columns
    # Create pharmacy desert definition: low-income and low-access
    datafull$pharmacydesert <- ifelse(datafull$lowincome_bin == 1 & datafull$in_pop_percent_flag == 1, 1, 0)

###################################################################
###################################################################
# Statistical analyses and tables
###################################################################

# Create summary tables per supplement in long proposal
    

# Do statistical analyses: add to MS word tables
    # Dont forget which correction I'll be doing
    
    
    
    
    
###################################################################
###################################################################
# Maps and visualizations
###################################################################   
# map check for just WA to check what these new proportions and urbanicities look like by census tract:
# should eventually output a map of what was defined as urban / suburban / rural / lowveh nationally in supplemental materials?
popup1 <- paste0(str_extract(mydata_sf$legal_name, "^([^,]*)")) # check that these names and characters are right
map_ur_WA <- leaflet(width = "100%") %>%                     # sets the width of the map
  addProviderTiles(provider = "CartoDB.Positron") %>%        # TODO remind myself what this does
  addPolygons(data = states_c %>% 
                filter(state = WA) %>% 
                st_transform(crs = "+proj=longlat +datum=WGS84"),
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 1.5) %>% 
  addPolygons(data = counties_c %>%
                filter(state = WA) %>%                       # filter to just WA on each of these to do quick test but then remove
                st_transform(crs = "+proj=longlat +datum=WGS84"), # put my data on WSG84 CRS for mapping in open street maps
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 1) %>% 
  addPolygons(data = tract_c %>% dplyr::filter(state = WA) %>% 
                st_transform(crs = "+proj=longlat +datum=WGS84"),
              fillOpacity = 0,
              stroke = TRUE,
              color = "black",
              weight = 0.5) %>% 
  addPolygons(data = mydata_sf %>%
                st_transform(crs = "+proj=longlat +datum=WGS84") %>%  # change mydata from NAD83 to WSG84 for mapping in OpenStreetMaps
                filter(accessradius == 0.5), #
              popup = popup4,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7, 
              color = "darkred") %>% 
  addPolygons(data = mydata_sf %>%
                st_transform(crs = "+proj=longlat +datum=WGS84") %>%  # change mydata from NAD83 to WSG84 for mapping in OpenStreetMaps
                filter(accessradius == 1), #
              popup = popup4,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7, 
              color = "blue2") %>% 
  addPolygons(data = mydata_sf %>%
                st_transform(crs = "+proj=longlat +datum=WGS84") %>%  # change mydata from NAD83 to WSG84 for mapping in OpenStreetMaps
                filter(accessradius == 5), #
              popup = popup4,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7, 
              color = "green4") %>% 
  addPolygons(data = mydata_sf %>%
                st_transform(crs = "+proj=longlat +datum=WGS84") %>%  # change mydata from NAD83 to WSG84 for mapping in OpenStreetMaps
                filter(accessradius == 10), #
              popup = popup4,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7, 
              color = "gold2") %>% 
  addPolygons(data = mydata_sf %>%
                st_transform(crs = "+proj=longlat +datum=WGS84") %>%  # change mydata from NAD83 to WSG84 for mapping in OpenStreetMaps
                filter(is.na(accessradius)), #
              popup = popup4,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7, 
              color = "gray66")

  
