## get dispersal observations from existing database 
## developed by: Nikki Moore
library(tidyverse)
library(parallel)
library(pbapply)
library(traitdataform)
library(data.table)
source("R/harmonize.R")

## read in list of final species 
sp <- read.csv("data-processed/final-species-list.csv")

## taxize our species list 
sp_harm <- harmonize(sp$scientific_name)

notfound <- filter(sp_harm, is.na(db_code)) ## all species found

## rename columns 
sp <- sp %>%
  rename("initial_name" = scientific_name) 

sp <- left_join(sp, sp_harm, by = c("initial_name" = "species")) %>%
  unique()

## write harmonized species list 
write.csv(sp, "data-processed/final-species-list_taxonomy.csv", row.names = FALSE)

## read in Nikki's super secret dispersal database
dd <- read.csv("data-raw/species_traits/dispersal-distance-collated_ALL.csv")

## get rid of some columns 
dd <- select(dd, -c(reported_name, reported_name_fixed))

## filter to species in our initial species list
dd_oursp <- dd %>%
  filter(scientificName %in% sp$scientificName)

## how many species with dispersal observations?
length(unique(dd_oursp$scientificName)) ## 18

## add in sam's data
sam <- read.csv("data-raw/species_traits/Straus_MovementProfiles_raw.csv")
head(sam)
colnames(sam)
sam$binomial <- str_replace_all(sam$scientific_name.x, "\\_", " ")

## which species are in the final species list?
sam_fsp <- filter(sam, binomial %in% sp$scientificName)
length(unique(sam_fsp$binomial)) ## 24 

## which are species in my database?
length(which(unique(sam_fsp$binomial) %in% unique(dd_oursp$scientificName)))
## 14 - so 3 are not 
sam_unique <- filter(sam_fsp, !binomial %in% dd_oursp$scientificName)

## filter out species with no distance data
sam_unique <- filter(sam_unique, !is.na(dispersal_km))

length(unique(sam_unique$dispersal_km)) ## 8 

## make sure none are duplicated with data we already have 
unique(sam_unique$source_dispersal)

## reformat sam's data to add
colnames(dd_oursp)
colnames(sam)

sam_sub <- sam_unique %>%
  rename("scientificName" = binomial, 
         "DispersalDistance" = dispersal_km,
         "Source" = source_dispersal) %>%
  mutate(Unit = "km",
         Database = "Straus et al. (unpub)",
         Sex = NA,
         Code = "DispersalDistance",
         Field = "dispersal_km",
         ObservationTypeGeneral = NA,
         ObservationTypeSpecific = NA) %>%
  select(scientificName, DispersalDistance, Source, Sex, Unit, Code, Field, ObservationTypeSpecific,
         ObservationTypeGeneral, Database)
  
sam_sub <- left_join(sam_sub, select(sp, -initial_name, -genus, -species))

## bind with my data 
disp_data <- rbind(sam_sub, dd_oursp)

length(unique(disp_data$scientificName)) ## 26


## attach empty lines for species with no dispersal data 
no_dd <- sp$scientificName[which(!sp$scientificName %in% disp_data$scientificName)]

dd_tofill = left_join(sp, disp_data)



## write:
write.csv(dd_tofill, "data-raw/species_traits/dispersal-distance-data_unsearched.csv", row.names = F)








## visualize
dd = dd_tofill
dd$DispersalDistance <- as.numeric(as.character(dd$DispersalDistance))

dd$DispersalDistanceKm = ifelse(dd$Unit == "m", dd$DispersalDistance/1000, dd$DispersalDistance)

dd %>%
  filter(!is.na(DispersalDistanceKm)) %>%
  ggplot(aes(x = DispersalDistanceKm, fill = class)) + geom_histogram() + scale_x_log10()

dd %>%
  filter(!is.na(DispersalDistanceKm)) %>%
  ggplot(aes(y = ObservationTypeGeneral, fill = class)) + geom_bar() 

## within movement studies, what kind of data?
dd %>%
  filter(!is.na(DispersalDistanceKm)) %>%
  filter(ObservationTypeGeneral == "movement study") %>%
  ggplot(aes(y = ObservationTypeSpecific, fill = class)) + geom_bar() 


