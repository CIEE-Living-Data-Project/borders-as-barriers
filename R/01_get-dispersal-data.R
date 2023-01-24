## get dispersal observations from existing database 
library(tidyverse)
library(parallel)
library(pbapply)
library(traitdataform)
library(data.table)
source("R/harmonize.R")

## read in list of 143 species 
sp <- read.csv("data-processed/initial-species-list.csv")

## taxize our species list 
sp_harm <- harmonize(sp$x)

notfound <- filter(sp_harm, is.na(db_code)) ## all species found

## rename columns 
sp <- sp %>%
  rename("initial_name" = x) 

sp <- left_join(sp, sp_harm, by = c("initial_name" = "species")) %>%
  unique()

## write harmonized species list 
write.csv(sp, "data-processed/initial-species-list_taxonomy.csv", row.names = FALSE)

## read in Nikki's super secret dispersal database
dd <- read.csv("data-raw/species_traits/dispersal-distance-collated_ALL.csv")

## get rid of some columns 
dd <- select(dd, -c(reported_name, reported_name_fixed))

## filter to species in our initial species list
dd_oursp <- dd %>%
  filter(scientificName %in% sp$scientificName)

## how many species with dispersal observations?
length(unique(dd_oursp$scientificName)) ## 27

## attach empty lines for species with no dispersal data 
no_dd <- sp$scientificName[which(!sp$scientificName %in% dd_oursp$scientificName)]

dd_tofill = left_join(sp, dd_oursp)

## write:
write.csv(dd_tofill, "data-raw/species_traits/dispersal-distance-data_unsearched.csv", row.names = F)


