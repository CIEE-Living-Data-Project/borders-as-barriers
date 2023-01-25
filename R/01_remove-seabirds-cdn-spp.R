## filtering projections to get rid of seabirds and canadian species
library(tidyverse)

## read in projection validation data 
projval <- read.csv("data-raw/projection_validation.csv")

## clean
unique(projval$canadian_sp_yn)
unique(projval$marine_seabird_yn)
projval <- projval %>%
  mutate(canadian_sp_yn = ifelse(canadian_sp_yn == "Y", "y",
                                 ifelse(canadian_sp_yn == "N", "n", 
                                        ifelse(canadian_sp_yn == "", NA,
                                               canadian_sp_yn)))) %>%
  mutate(marine_seabird_yn = ifelse(marine_seabird_yn == "Y", "y",
                                 ifelse(marine_seabird_yn == "N", "n", 
                                        ifelse(marine_seabird_yn == "", NA, 
                                               marine_seabird_yn))))



## get list of species that are not Canadian/not seabirds 
projval$remove = ifelse(projval$canadian_sp_yn == "y" | projval$marine_seabird_yn == "y",
                        "Remove",
                        NA)

projval$remove = ifelse(str_detect(projval$notes, "emove"),
                        "Remove",
                        projval$remove)

length(which(projval$remove == "Remove")) ## 38 spp
length(which(is.na(projval$remove))) ## 105 spp

## write final species list:
sp_list <- projval %>%
  filter(is.na(remove)) %>%
  select(scientific_name, genus, species) 

write.csv(sp_list, "data-processed/final-species-list.csv", row.names = F)

