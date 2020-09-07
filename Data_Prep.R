# Loading Libraries -------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(readr)

# Loading Data ------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-01')

key_crop_yields <- tuesdata$key_crop_yields
fertilizer <- tuesdata$cereal_crop_yield_vs_fertilizer_application
tractors <- tuesdata$cereal_yields_vs_tractor_inputs_in_agriculture
land_use <- tuesdata$land_use_vs_yield_change_in_cereal_production
arable_land <- tuesdata$arable_land_pin

continent_country_crosswalk <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")

# Pre-Join Cleaning -------------------------------------------------------
#Making Crop yields longer 
crops <- key_crop_yields %>% pivot_longer(`Wheat (tonnes per hectare)`:`Bananas (tonnes per hectare)`,
                                          names_to = "Crop",
                                          values_to = "Yield")

#Converting year to numeric in tractor
tractors <- tractors %>% mutate(Year= as.numeric(Year))
land_use <- land_use %>% mutate(Year= as.numeric(Year))

# Joining -----------------------------------------------------------------
full_data <- crops %>% 
  left_join(fertilizer) %>% 
  left_join(tractors) %>% 
  left_join(land_use) %>% 
  left_join(arable_land)


# Creating a Continent and Subregion Column ---------------------------------------------
#Cleaning the continent crosswalk tibble (connects continents to country names)
clean_cc_crosswalk <- continent_country_crosswalk %>% 
  rename("Code"=`alpha-3`,"Continent"=region, "Subregion"=`sub-region`) %>% 
  select(Code,Continent,Subregion)

#Filtering out rows in which the Entity column is not a country
full_data <- full_data %>% 
  filter(!is.na(Code))
  

# Adding continent and subregion columns, and putting them at the start for ease of use
full_data <- full_data %>% 
  left_join(clean_cc_crosswalk, by=c("Code")) %>% 
  relocate(Continent, .before=Entity) %>% 
  relocate(Subregion, .before=Entity) %>% 
  rename("Country"=Entity)


# Cleaning Columns --------------------------------------------------------
full_data <- full_data %>% 
  mutate(Crop=str_remove(Crop,"\\(tonnes per hectare\\)"),
         Crop=str_remove(Crop," "))


  

