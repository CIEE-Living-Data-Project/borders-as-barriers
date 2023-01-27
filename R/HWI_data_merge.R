# Dispersal distance paper
library(tidyverse)

# HWI downloaded from: https://zenodo.org/record/3832215#.Y9BSsOzMK3I

## Look at overlap between HWI and our data

# species list
species <- read.csv("data-raw/species_traits/specieslist_afterremoval.csv")

# HWI dataset
HWI <- read.csv("data-raw/species_traits/HWI_index/HWI_dataset.csv")

both <- merge(species, HWI, by.x="scientific_name", by.y="Species.name")

# export as csv  
write.csv(both, "data-raw/species_traits/aves_HWI.csv")


### Look at how many bird species we have dispersal for
dispersal <- read.csv("data-raw/species_traits/dispersal-distance-data_unsearched.csv")

d <- dispersal %>% filter(class=="Aves")

aves.dispersal <- merge(d, species, by.x="scientificName", by.y="scientific_name")
# there are 8 out of 26 that have dispersal information

# now merge with HWI
HWI.dispersal <- merge(aves.dispersal, HWI, by.x="scientificName", by.y="Species.name", all.x=T)
# we have 6 species wtihout HWI or dispersal distance


ggplot(HWI.dispersal, mapping=aes(y=DispersalDistance, x=HWI))+
  geom_point()+
  geom_smooth()













