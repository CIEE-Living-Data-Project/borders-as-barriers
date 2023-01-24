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







