library(stringr)
library(tidyverse)

# read dfs
# species <- read.csv("data-raw/species_traits/species.list.csv")
# traits <- read.csv("data-raw/species_traits/Straus_MovementProfiles_cleaned.csv")
# 
# species <- species %>% 
#   mutate(scientific_name.x = str_replace(x, " ", "_"))
# 
# species_traits <- left_join(species, traits, by = "scientific_name.x")
# 
# ## this leaves us very few. Trying with the uncleaned version of my movement database

traits_raw <- traits <- read.csv("data-raw/species_traits/Straus_MovementProfiles_raw.csv")

species <- read.csv("data-raw/species_traits/species.list.csv") #reread bc this df has the space
species <- species %>% rename(scientific_name.x = x)

species_traits2 <- left_join(species, traits_raw, by = "scientific_name.x")

# remove unnecessary columns
species_traits2 <- species_traits2 %>% 
  select(scientific_name.x, dispersal_km, source_dispersal, mean.hra.m2, source_hra, Mass_kg, Mass_source)

write.csv(species_traits2, "data-raw/species_traits/species_traits.csv")
