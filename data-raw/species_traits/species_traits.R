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


####### Taxize #######
library(taxize)
species_final <- read.csv("data-processed/final-species-list.csv")

## create a list of unique binomials
species.list <- species_final %>% 
  select(scientific_name)

## search for upstream taxonomy
species.tax <- classification(unlist(species.list), db = "itis")

## convert this into a useable data format (from a nested list to a tibble)
species.tax <- species.tax %>% 
  rbind() %>% ## bind_rows() doesn't work because the output is not a typical list...
  tibble() %>% 
  ## drop database 'id' column
  ## keep 'query' column for indexing while pivoting below
  select(-id) %>% 
  ## we only want the 'traditional' taxonomic levels (i.e., KPCOFGP)
  filter(rank %in% c("class", "order", 
                     "family", "genus", "species")) %>%
  pivot_wider(., id_cols = "query", names_from = "rank", values_from = "name") %>% 
  ## drop the 'query' column (same as 'species')
  select(-query)

species.tax


species_final_tax <- species_final %>% 
  select(scientific_name) %>% 
  rename(species = scientific_name) %>% 
  left_join(., species.tax, by = "species")

write_csv(species_final_tax, file = "data-processed/species_final_taxized.csv")


#### compare with Jenkins db #####

jenkins <- read.csv("data-raw/species_traits/Jenkins_2007_data.csv")

jenkins_reps <- jenkins %>% filter(group == "Reptile") %>% 
  rename(species = Scientific.Name, dispersal_meters = Max..Indiv..Dispersal.Distance..m.)

species_tax_space_rem <- species_final_tax %>% 
  mutate(species = str_replace(species, " ", ""))

jenkins_tax <- left_join(species_tax_space_rem, jenkins_reps, by = "species") %>% 
  filter(!is.na(dispersal_meters))
