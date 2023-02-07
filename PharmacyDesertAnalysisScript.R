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
setwd(rootDir)

## Load files needed:
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

## Merge NCPDP data:
pharmacy_df <- full_join(providerinfo_df, services_df, by = "NCPDP Provider ID")
table(pharmacy_df$`Primary Provider Type Code`, useNA = "always") # get numbers of pharmacy by type 
pharmacy_df <- pharmacy_df %>% filter(`Primary Provider Type Code` == 1) # then filter to just include community pharmacies


pharmbystate_df <- pharmacy_df %>% group_by(`Physical Address State`) %>% 
  dplyr::summarise(n_pharm = n()) %>% 
  filter(!`Physical Address State` %in% c("PR", "GU", "VI", "MP", "NA")) # removing territories except DC. (PR 1041; GU 24; VI 20; MP 9).

# 2222222222222222222222222222222222222222222222222222222222222222222222222
# 2222222222222222222222222222222222222222222222222222222222222222222222222
# Read in shape files

# 3333333333333333333333333333333333333333333333333333333333333333333333333
# 3333333333333333333333333333333333333333333333333333333333333333333333333
# Geocode pharmacy data:

# break into 2 chunks: 
# need to be under 40k addresses in order to stay under GoogleAPI geocoding budget limit for the month
pharmbystate_df1 <- pharmbystate_df[1:25,] # contains 31510 pharmacies
pharmbystate_df2 <- pharmbystate_df[26:52,] %>% filter(!is.na(`Physical Address State`)) #contains 30549 pharmacies
# going to start with geocoding the second half alphabetically because includes WA so can compare to previous analysis
states1 <- pharmbystate_df1$`Physical Address State`
states2 <- pharmbystate_df2$`Physical Address State`

pharmacy_df1 <- pharmacy_df %>% filter(`Physical Address State` %in% states1)
pharmacy_df2 <- pharmacy_df %>% filter(`Physical Address State` %in% states2)

# # concatenate address info into 1 column
pharmacy_df2$addresses <- paste(pharmacy_df2$`Physical Address 1`,",",
                                pharmacy_df2$`Physical Address City`,",",
                                pharmacy_df2$`Physical Address State`,
                                pharmacy_df2$`Physical Address ZIP`)
pharmacy_df1$addresses <- paste(pharmacy_df1$`Physical Address 1`,",",
                                pharmacy_df1$`Physical Address City`,",",
                                pharmacy_df1$`Physical Address State`,
                                pharmacy_df1$`Physical Address ZIP`)

# find lat and long of addresses using geocode and Google API (for 30k addresses will take a while to run)
  # df2 first chunk of addresses
    for (i in 1:nrow(pharmacy_df2)) {
      result <- geocode(pharmacy_df2$addresses[i], output = "latlon", source = "google")
      pharmacy_df2$lon[i] <- as.numeric(result[1])
      pharmacy_df2$lat[i] <- as.numeric(result[2])
    }
    
    View(pharmacy_df2)
    table(pharmacy_df2$`Physical Address State`[is.na(pharmacy_df2$lat)], useNA = "always") # there are 11 out of 30549 that have NA for lat/lon
    table(pharmacy_df2$`Legal Business Name`[is.na(pharmacy_df2$lon)], useNA = "always") # some of these seem like legit pharmacies- investigate
  
  # df1 second chunk of addresses
    for (i in 1:nrow(pharmacy_df1)) {
      result <- geocode(pharmacy_df1$addresses[i], output = "latlon", source = "google")
      pharmacy_df1$lon[i] <- as.numeric(result[1])
      pharmacy_df1$lat[i] <- as.numeric(result[2])
    }
    
    View(pharmacy_df1)
    table(pharmacy_df1$`Physical Address State`[is.na(pharmacy_df2$lat)], useNA = "always") # there are XX out of 38772 that have NA for lat/lon
    table(pharmacy_df1$`Legal Business Name`[is.na(pharmacy_df2$lon)], useNA = "always") # some of these seem like legit pharmacies- investigate


  # write.csv(pharmacy_df2, file = "pharmacy_df2_geo.csv")
  write.csv(pharmacy_df1, file = "pharmacy_df1_geo.csv")

  # read in csv that I saved earlier
  # pharmacy_df1 <- read.csv("pharmacy_df1_geo.csv")
  pharmacy_df2 <- read.csv("pharmacy_df2_geo.csv")
  # check col order first before doing this:
  pharmgeo_df <- rbind(pharmacy_df1, pharmacy_df2)

# #***************
# pharm_address <- pharm_address %>% filter(!is.na(lat), #check what some of these are?
#                                           str_detect(pharm_address$Credential, "HOSP", negate = TRUE)) # decide to exclude HOSP ones too

# check the colnames, drop unneeded ones, define factor levels

# drop unneeded columns (do this after join, just putting lang here in the meantime)
pharmacy_df2_2 <- pharmacy_df2 %>% select(-X, -Doctors.Name, -Store.Number, -Phone.Number, -Phone.Extension, 
                                          -Fax.Number, -Email.Address, -Cross.Streets, -PMSA, -Provider.Hours,
                                          -Congressional.Voting.District, -Store.Open.Date, - Store.Close.Data,
                                          -DEA.Registration.ID, - DEA.Expiration.Date, -Federal.Tax.ID.Number,
                                          -State.Income.Tax.ID.Number, -Deactivation.Code, -Reinstatement.Code,
                                          -Reinstatement.Date, -Transaction.Code, -Transaction.Date, -ID.y,)

# go one by one and update the class and factor levels- see HSERV 523 code
