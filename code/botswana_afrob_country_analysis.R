########################################################################

#   Author(s): Laura Keen
#   Date created: 06 May 2019
#   Date last modified: 06 May 2019
#   Files used:
#     - data_raw/Afrobarometer
#   Files created:
#     - data_intermediate/Afrobarometer

#  Country: Botswana

########################################################################

library(dplyr)
library(foreign)
library(tidyverse)
library(lubridate)

# File path
afrob <- "./data/data_raw/Afrobarometer"

# Do you want to save the results?
save <- TRUE

if (save) {out <- "./data/data_intermediate/Afrobarometer"}

########################################################
# Load Data and Set Parameters
########################################################

afrob.files <- list.files(afrob, pattern = "[2-7]",full.names = TRUE) # round 1 survey is excluded
roundkeys <- paste0("r", c(2:7))

ctry <- "Botswana" # which country are you analyzing?

########################################################
# Iterate over the survey rounds
########################################################

results <- as.data.frame(matrix(nrow = 0, ncol = 0)) # dataframe to save statistics summaries
regions_un <- as.data.frame(matrix(nrow = 0, ncol = 0)) # dataframe to save region names per survey round

for (i in roundkeys){
  
  round <- list.files(afrob, pattern = i,full.names = TRUE) %>% read.spss(to.data.frame = TRUE) 
  colnames(round) <- colnames(round) %>% tolower() # change colnames to lowercase
  
  country.exist <- unique(round[["country"]]) %>% {grepl(ctry, .)} %>% sum() #is your country included in this survey round?
  
  if(country.exist == 1) {
    
    r <- gsub("\\D", "", i) # extract country round
    
    n_duplicates <- round[["respno"]] %>% duplicated() %>% sum() # check duplicates
    
    cat("\n", ctry, "in Round", i, 
        "\n", "Number of Duplicates:", n_duplicates, "\n") # print duplicate check results
    
    
    round <- round %>% 
      filter(country == ctry) %>%  # only keep your country
      mutate(dateintr = as.Date(dateintr/86400, origin = "1582-10-14")) # convert SPSS date to calendar date
    
    # interview time variabes
    inter_start <- round$dateintr %>% unique() %>% min() # when did the interview start?
    inter_end <- round$dateintr %>% unique() %>% max() # when did it end?
    inter_length <- difftime(inter_end, inter_start) # number of days it took
    
    # region/geolocation variables
    num_regions <- round[["region"]] %>% unique() %>% length() # how many regions are surveyed?
    
    regions <- round[["region"]] %>% unique() %>% 
      as.data.frame() %>% `colnames<-`("region") %>%
      mutate(region = tolower(region),
             region =gsub("[^a-z]|duplicated|duplicate", "", region)) # extract regions names and remove unrelated strings
    
    regions[[paste0("round", r)]] <- 1
    
    
    if(length(regions_un) == 0) {regions_un <- bind_cols(regions)}
    else {regions_un <- regions_un %>% full_join(regions)} 
    
    location_level_1 <- grepl("location", colnames(round)) %>% sum() # is there a location_level_1 variable?
    
    if(location_level_1 == 1){
      num_locations <- round %>% pull(colnames(round)[grep("location", colnames(round))]) %>% unique() %>% length()
    } else {num_locations = NA}
    
    # save your results
    country_results <- data.frame(ctry, r, inter_start, inter_end, inter_length, 
                                  num_regions, location_level_1, num_locations)
    results <- bind_rows(results, country_results)
    
    # NA Check
    threshold <- 0.1 * (dim(round)[2]) # using 10% threshold for NA check
    
    na_check <- round %>% 
      apply(2, function(x) sum(grepl("Missing", x))) %>% as.data.frame() %>%
      rownames_to_column() %>% `colnames<-`(c("var","num_na")) %>% 
      filter(num_na >= threshold) %>% pull(var) # variables have missing values greater than 10% of the observations
    
    if(length(na_check) > 0) {cat("\n", "Variables with NAs greater than 0.1 threshold:", na_check)}
    
  } else {cat("\n", ctry, "NOT in round", r, "\n")}
}


if (save) {
  write.csv(results, paste0(out, "/countrySummary_", ctry))
  write.csv(regions_un, paste0(out, "/countryRegionCheck_", ctry))
}


# Create key to match all regions listed to administrative regions in shapefile using a numeric key
  
regions_un <- regions_un %>% 
  mutate(mapid = case_when(str_detect(region, "kweneng") ~ "9",
                          str_detect(region, "lobatse") ~ "10",
                          str_detect(region, "northwest") ~ "12",
                          str_detect(region,"southeast") ~ "14",
                          str_detect(region, "gaborone") ~ "4",
                          str_detect(region,"southern") ~ "15",
                          str_detect(region,"central") ~ "1", # Captures centralmahalapye, centraltutume, centralboteti, 
                                                              # centralserowepalapye, centralbotetiorapa, centralserowe, and centraltutumesowatown 
                          str_detect(region,"kgatleng") ~ "8",
                          str_detect(region,"francistown") ~ "3",
                          str_detect(region,"northeast") ~ "11",
                          str_detect(region,"orapa") ~ "1",   # Orapa located in Central district
                          str_detect(region,"ghanzi") ~ "5",
                          str_detect(region,"kgalagadi") ~ "7",
                          str_detect(region,"jwaneng") ~ "6",
                          str_detect(region,"ngamiland") ~ "12", # Ngamiland located in North-West district
                          str_detect(region,"chobe") ~ "2",
                          str_detect(region,"ngwaketse") ~ "15", # Ngwaketse located in Southern district
                          str_detect(region,"barolong") ~ "1",   # Borolong located in Central district
                          grepl("sel", region) ~ "13")    # Captures seleibephikwe, selibepikwe, and selibephikwe
  )



########################################################
# Merge with shapefile
########################################################
         
library(sf)
         
# first, make sure that your shapefile package has .shp, .shx, .dbf, .prj within the same folder

shapefile_bwa <- st_read("data/data_raw/shapefiles/gadm36_BWA_1.shp")

# clean mapid key for merging
shapefile_bwa <- shapefile_bwa %>%
  mutate(mapid = gsub("BWA.", "", GID_1)) %>%
  mutate(mapid = gsub("_1$", "", mapid))

shapefile_bwa <- shapefile_bwa[c(1, 12, 2:11)] #reorder columns

        
# merge
#shapefile.merge <- shapefile %>% left_join(survey.round.data, by = ...)
         
shapefile.merge_bwa <- shapefile_bwa %>% left_join(regions_un, by = "mapid")

plot(shapefile.merge_bwa["geometry"])

# Save merged dataframe with merged shapefile and regions_un (crosswalk for later merging)         
write.csv(shapefile.merge_bwa, paste0(out, "/shapefile.merge_Botswana.csv"))

# Save unmerged shapefile with key in Shapefile folder
st_write(shapefile_bwa, paste0(out, "/Shapefile/Botswana.shp"))



