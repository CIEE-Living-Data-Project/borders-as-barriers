## get dispersal observations from existing database 
## developed by: Nikki Moore
library(tidyverse)
library(parallel)
library(pbapply)
library(traitdataform)
library(data.table)
source("R/harmonize.R")

##-----------------------------------##
##  collate dispersal distance data  ##
##-----------------------------------##
## read in list of final species 
sp <- read.csv("data-processed/final-species-list_updated.csv")

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
dd$DispersalDistance <- as.numeric(as.character(dd$DispersalDistance))
dd$DispersalDistanceKm = ifelse(dd$Unit == "m", dd$DispersalDistance/1000, dd$DispersalDistance)

## get rid of some columns 
dd <- select(dd, -c(reported_name, reported_name_fixed))

## filter to species in our initial species list
dd_oursp <- dd %>%
  filter(scientificName %in% sp$scientificName)

## how many species with dispersal observations?
length(unique(dd_oursp$scientificName)) ## 44

## add in sam's data
sam <- read.csv("data-raw/species_traits/Straus_MovementProfiles_raw.csv")
head(sam)
colnames(sam)
sam$binomial <- str_replace_all(sam$scientific_name.x, "\\_", " ")

## which species are in the final species list?
sam_fsp <- filter(sam, binomial %in% sp$scientificName)
length(unique(sam_fsp$binomial)) ## 43 

## which are species in my database?
length(which(unique(sam_fsp$binomial) %in% unique(dd_oursp$scientificName)))
## 30 - so 13 are not 
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
         DispersalDistanceKm = DispersalDistance,
         Field = "dispersal_km",
         ObservationTypeGeneral = NA,
         ObservationTypeSpecific = NA) %>%
  select(scientificName, DispersalDistance, Source, Sex, Unit, Code, Field, ObservationTypeSpecific,
         ObservationTypeGeneral, Database, DispersalDistanceKm)
  
sam_sub <- left_join(sam_sub, select(sp, -initial_name, -genus, -species))

## bind with my data 
disp_data <- rbind(sam_sub, dd_oursp)

length(unique(disp_data$scientificName)) ## 52


## attach empty lines for species with no dispersal data 
no_dd <- sp$scientificName[which(!sp$scientificName %in% disp_data$scientificName)]

dd_tofill = left_join(sp, disp_data)

## write:
write.csv(dd_tofill, "data-raw/species_traits/dispersal-distance-data_unsearched.csv", row.names = F)


##--------------------------------------------------------##
##  estimate dispersal potential for species in our data  ##
##--------------------------------------------------------##
## read in new species list 
taxa <- sp

## Assigning species a dispersal distance:
## - using the dataset of empirical dispersal estimates we collated, we assigned each species a dispersal distance based on averages calculated by class or order
## - if more than 10 observations of dispersal events were available at the order level, we calculated a mean at the order level. otherwise, we calculated a mean at the class level

## summarize types of measurements by order and class:
order_dd <- dd %>%
  filter(order %in% sp$order) %>%
  group_by(order, ObservationTypeSpecific) %>%
  tally() %>%
  spread(key = ObservationTypeSpecific, value = n) %>%
  filter(!is.na(order))

class_dd <- dd %>%
  filter(class %in% sp$class) %>%
  group_by(class, ObservationTypeSpecific) %>%
  tally() %>%
  spread(key = ObservationTypeSpecific, value = n) %>%
  filter(!is.na(class))

## we included movement studies, mark-recapture studies, radiotagging studies, or natal/breeding dispersal studies 

## create column that identifies studies as either mark-recapture, breeding, or movement 
dd_reclass <- dd %>%
  mutate(dispersal_estimate_type = ifelse(ObservationTypeSpecific 
                                          %in% c("mark-release-recapture",
                                                 "trapping/radio tracking",
                                                 "mark-release-recapture and/or radio-tracking studies",
                                                 "mark-recapture",
                                                 "capture-mark-recapture"),
                                                 "mark-recapture study",
                                                 ifelse(ObservationTypeSpecific 
                                                        %in% c("breeding dispersal",
                                                               "natal dispersal",
                                                               "natal dispersal distance"),
                                                        "breeding/natal dispersal",
                                                        ifelse(ObservationTypeSpecific 
                                                               %in% c("radio-tracking",
                                                                      "individual movement distance",
                                                                      "homing study"),
                                                               "movement study",
                                                               "other"))))

ggplot(dd_reclass, aes(x = log(DispersalDistanceKm), fill = dispersal_estimate_type)) + geom_histogram()

## filter out "other" types of observations 
dd_reclass <- filter(dd_reclass, dispersal_estimate_type != "other")
## fix reptile class:
dd_reclass <- dd_reclass %>%
  mutate(order = ifelse(class %in% c("Squamata", "Testudines"),
                        class, 
                        order)) %>%
  mutate(class = ifelse(class %in% c("Squamata", "Testudines"),
                        "Reptilia", 
                        class)) %>%
  filter(!family %in% c("Cheloniidae", "Dermochelyidae")) ## get rid of sea turtles

taxa <- taxa %>%
  mutate(order = ifelse(class %in% c("Squamata", "Testudines"),
                        class, 
                        order)) %>%
  mutate(class = ifelse(class %in% c("Squamata", "Testudines"),
                        "Reptilia", 
                        class)) 

## calculate dataframe of class and order level means 
## filter out ones with sample size of less than 10
class_means <- dd_reclass %>%
  group_by(class) %>%
  filter(!is.na(class)) %>%
  filter(class %in% taxa$class) %>%
  summarise(ClassMeanDD = mean(DispersalDistanceKm, na.rm = TRUE),
            ClassMeanN = length(which(!is.na(DispersalDistanceKm))),
            ClassSD = sd(DispersalDistanceKm, na.rm = TRUE)) %>%
  filter(ClassMeanN >= 10)
  
order_means <- dd_reclass %>%
  group_by(order) %>%
  filter(!is.na(order)) %>%
  filter(order %in% taxa$order) %>%
  summarise(OrderMeanDD = mean(DispersalDistanceKm, na.rm = TRUE),
            OrderMeanN = length(which(!is.na(DispersalDistanceKm))),
            OrderSD = sd(DispersalDistanceKm, na.rm = TRUE)) %>%
  filter(OrderMeanN >= 10) 

## join 
dd_ourspp <- left_join(taxa, order_means) %>%
  left_join(., class_means) 

## write out:
write.csv(dd_ourspp, "data-processed/taxa-list-with-dispersal.csv", row.names = F)


## for bats, we split species into long and short dispersers based on information about migration and movement (most were tagging studies) found through a targeted search








## data visualization
dd_reclass %>%
  group_by(class) %>%
  filter(class == "Aves") %>%
  summarise(mean = mean(DispersalDistanceKm)) 
dd_reclass %>%
  group_by(class) %>%
  filter(class == "Aves") %>%
  filter(dispersal_estimate_type != "movement study") %>%
  summarise(mean = mean(DispersalDistanceKm)) 
dd_reclass %>%
  group_by(order) %>%
  filter(order %in% taxa$order) %>%
  filter(class == "Aves") %>%
  summarise(mean = mean(DispersalDistanceKm),
            n = length(DispersalDistanceKm)) 
dd_reclass %>%
  group_by(order) %>%
  filter(order %in% taxa$order) %>%
  filter(class == "Aves") %>%
  filter(dispersal_estimate_type != "movement study") %>%
  summarise(mean = mean(DispersalDistanceKm),
            n = length(DispersalDistanceKm)) 

dd_reclass %>%
  group_by(class) %>%
  filter(class == "Mammalia") %>%
  summarise(mean = mean(DispersalDistanceKm)) 
dd_reclass %>%
  group_by(class) %>%
  filter(class == "Mammalia") %>%
  filter(dispersal_estimate_type != "movement study") %>%
  summarise(mean = mean(DispersalDistanceKm)) 
dd_reclass %>%
  group_by(order) %>%
  filter(order %in% taxa$order) %>%
  filter(order != "Chiroptera") %>%
  filter(class == "Mammalia") %>%
  summarise(mean = mean(DispersalDistanceKm),
            n = length(DispersalDistanceKm)) 
dd_reclass %>%
  group_by(order) %>%
  filter(order %in% taxa$order) %>%
  filter(order != "Chiroptera") %>%
  filter(class == "Mammalia") %>%
  filter(dispersal_estimate_type != "movement study") %>%
  summarise(mean = mean(DispersalDistanceKm),
            n = length(DispersalDistanceKm)) 

dd_reclass %>%
  group_by(class) %>%
  filter(class == "Amphibia") %>%
  summarise(mean = mean(DispersalDistanceKm)) 
dd_reclass %>%
  group_by(class) %>%
  filter(class == "Amphibia") %>%
  filter(dispersal_estimate_type != "movement study") %>%
  summarise(mean = mean(DispersalDistanceKm)) 
dd_reclass %>%
  group_by(order) %>%
  filter(order %in% taxa$order) %>%
  filter(class == "Amphibia") %>%
  summarise(mean = mean(DispersalDistanceKm),
            n = length(DispersalDistanceKm)) 
dd_reclass %>%
  group_by(order) %>%
  filter(order %in% taxa$order) %>%
  filter(class == "Amphibia") %>%
  filter(dispersal_estimate_type != "movement study") %>%
  summarise(mean = mean(DispersalDistanceKm),
            n = length(DispersalDistanceKm))

## for reptiles, only have movement studies 
dd_reclass %>%
  group_by(class) %>%
  filter(class %in% c("Squamata", "Testudines")) %>%
  summarise(mean = mean(DispersalDistanceKm)) 
dd_reclass %>%
  group_by(class) %>%
  filter(class %in% taxa$order) %>% 
  filter(class %in% c("Squamata", "Testudines")) %>%
  filter(!family %in% c("Cheloniidae", "Dermochelyidae")) %>% ## get rid of sea turtles
  summarise(mean = mean(DispersalDistanceKm),
            n = length(DispersalDistanceKm)) 





