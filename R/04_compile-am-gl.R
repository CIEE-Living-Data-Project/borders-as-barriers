## script to pull together age at maturity / generation time data 
## developed by: Nikki Moore
library(tidyverse)
library(readxl)

## read in taxized species list 
sp <- read.csv("data-processed/final-species-list_taxonomy.csv")

cols_to_keep <- c("scientificName", "MeasureType", "Measurement",
                  "Units", "Field", "Code", "MeasurementSource")

#-----------------
# Amphibio
#------------------
pulldata("amphibio")
head(amphibio)
amphibio$Age_at_maturity_min_y
amphibio$Age_at_maturity_max_y

## search for our species
amph_oursp <- amphibio[which(amphibio$Species %in% sp$scientificName),]

amph_oursp <- amph_oursp %>%
  select(Species, Age_at_maturity_min_y, Age_at_maturity_max_y) %>%
  distinct()

amph_sub = amph_oursp %>%
  gather(key = "Field", value = "Measurement", 
         c(Age_at_maturity_min_y, Age_at_maturity_max_y)) %>%
  mutate(Code = ifelse(Field == "Age_at_maturity_max_y", "MaxAgeAtMaturity",
                       "MinAgeAtMaturity")) %>%
  rename("scientificName" = Species) %>%
  mutate(MeasurementSource = "Amphibio", 
         Units = "years", 
         MeasureType = "AgeAtMaturity")%>%
  filter(!is.na(Measurement))

## how many? 
length(unique(amph_sub$scientificName))
## 18 species 

amph_sub <- select(amph_sub, cols_to_keep)




#-----------------
# Meiri
#------------------
meiri <- read.csv("data-raw/species_traits/Meiri/Appendix S1 - Lizard data version 1.0.csv")
colnames(meiri)
unique(meiri$youngest.age.at.first.breeding..months.)
unique(meiri$oldest.age.at.first.breeding..months.)

meiri <- filter(meiri, Binomial != "")

## search for our species
mei_oursp <- meiri[which(meiri$Binomial %in% sp$scientificName),]

mei_oursp <- mei_oursp %>%
  select(Binomial, youngest.age.at.first.breeding..months., oldest.age.at.first.breeding..months.) %>%
  distinct()

mei_sub = mei_oursp %>%
  gather(key = "Field", value = "Measurement", 
         c(youngest.age.at.first.breeding..months., oldest.age.at.first.breeding..months.)) %>%
  mutate(Code = ifelse(Field == "youngest.age.at.first.breeding..months.",
                       "oldest.age.at.first.breeding..months.",
                       "MinAgeAtMaturity")) %>%
  rename("scientificName" = Binomial) %>%
  mutate(MeasurementSource = "", 
         Units = "months",
         MeasureType = "AgeAtMaturity")%>%
  filter(!is.na(Measurement))


## how many? 
length(unique(mei_sub$scientificName))
## 11 species 

mei_sub <- select(mei_sub, cols_to_keep)

#-----------------
# Pacifici et al. 2014
#------------------
pacifici <- read.csv("data-raw/species_traits/Pacifici/doi_10.5061_dryad.gd0m3__v1/Generation Lenght for Mammals.csv")
head(pacifici)
pacifici$GenerationLength_d

## search for our species
pac_oursp <- pacifici[which(pacifici$Scientific_name %in% sp$scientificName),]

pac_oursp <- pac_oursp %>%
  select(Scientific_name, GenerationLength_d) %>%
  distinct()

pac_sub = pac_oursp %>%
  mutate(Code = "GenerationLength",
         MeasureType = "GenerationLength",
         Field = "GenerationLength_d") %>%
  rename("scientificName" = Scientific_name,
         "Measurement" = GenerationLength_d) %>%
  mutate(MeasurementSource = "Pacifici et al. 2014", 
         Units = "days")%>%
  filter(!is.na(Measurement))

## how many? 
length(unique(pac_sub$scientificName))
## 26 species 

pac_sub <- select(pac_sub, cols_to_keep)


#----------
# Pantheria
#----------
panth1 <- read.delim('data-raw/species_traits/Pantheria/ECOL_90_184/PanTHERIA_1-0_WR05_Aug2008.txt')
head(panth1)
panth1$X23.1_SexualMaturityAge_d

## search for our species
panth1_ourspp <- panth1[which(panth1$MSW05_Binomial %in% sp$scientificName),]

panth1_ourspp <- panth1_ourspp %>%
  select(MSW05_Binomial, X23.1_SexualMaturityAge_d) %>%
  distinct()

panth1_sub = panth1_ourspp %>%
  rename("Measurement" = X23.1_SexualMaturityAge_d,
         "scientificName" = MSW05_Binomial) %>%
  mutate(MeasurementSource = "Pantheria - 2005", 
         Units = "days",
         Field = "SexualMaturityAge_d", 
         Code = "AgeAtMaturity",
         MeasureType = "AgeAtMaturity")%>%
  filter(!is.na(Measurement)) %>%
  filter(Measurement != "-999")

## how many? 
length(unique(panth1_sub$scientificName))
## 11 species 

panth1_sub <- select(panth1_sub, cols_to_keep)

panth2 <- read.delim('data-raw/species_traits/Pantheria/ECOL_90_184/PanTHERIA_1-0_WR93_Aug2008.txt')
head(panth2)
panth2$X13.1_AdultHeadBodyLen_mm

## search for our species
panth2_ourspp <- panth2[which(panth2$MSW05_Binomial %in% sp$scientificName),]
## none


#-------
# AnAge
#-------
anage <- read.delim("data-raw/species_traits/AnAge/anage_data.txt")
head(anage)
anage$Female.maturity..days.
anage$Male.maturity..days.
anage$genus_species <- paste(anage$Genus, anage$Species, sep = " ")

## search for our species
anage_oursp <- anage[which(anage$genus_species %in% sp$scientificName),]

anage_oursp <- anage_oursp %>%
  select(genus_species, Male.maturity..days., Female.maturity..days.) %>%
  distinct()

anage_sub = anage_oursp %>%
  gather(key = "Field", value = "Measurement", 
         c(Male.maturity..days., Female.maturity..days.)) %>%
  mutate(MeasureType = "AgeAtMaturity",
         Code = ifelse(Field == "Male.maturity..days.", 
                       "MaleAgeAtMaturity",
                       "FemaleAgeAtMaturity")) %>%
  rename("scientificName" = genus_species) %>%
  mutate(MeasurementSource = "AnAge", 
         Units = "days")%>%
  filter(!is.na(Measurement))

## how many? 
length(unique(anage_sub$scientificName))
## 39 species 

anage_sub <- select(anage_sub, cols_to_keep)


#-----------------
# Amniota
#------------------
amniota <- read.csv("data-raw/species_traits/amniota/ECOL_96_269/Data_Files/Amniote_Database_Aug_2015.csv")
head(amniota)
amniota$female_maturity_d
amniota$male_maturity_d
amniota$genus_species <- paste(amniota$genus, amniota$species, sep = " ")

## search for our species
amniota_oursp <- amniota[which(amniota$genus_species %in% sp$scientificName),]

amniota <- amniota %>%
  select(genus_species, female_maturity_d, male_maturity_d) %>%
  distinct()

amni_sub = amniota_oursp %>%
  gather(key = "Field", value = "Measurement", 
         c(female_maturity_d, male_maturity_d)) %>%
  mutate(MeasureType = "AgeAtMaturity",
         Code = ifelse(Field == "Male_maturity_d", 
                       "MaleAgeAtMaturity",
                       "FemaleAgeAtMaturity")) %>%
  rename("scientificName" = genus_species) %>%
  mutate(MeasurementSource = "Amniota", 
         Units = "days")%>%
  filter(!is.na(Measurement)) %>%
  filter(Measurement != "-999")

## how many? 
length(unique(amni_sub$scientificName))
## 46 species 

amni_sub <- select(amni_sub, cols_to_keep)


#### combine them all
all_amgl <- rbind(panth1_sub, amph_sub) %>%
  rbind(., mei_sub) %>%
  rbind(., pac_sub) %>%
  rbind(., anage_sub) %>%
  rbind(., amni_sub)

length(unique(all_amgl$scientificName)) ## 81 species

## add empty rows for species with missing body size
no_amgl <- sp$scientificName[which(!sp$scientificName %in% all_amgl$scientificName)]

amgl_tofill = left_join(sp, all_amgl)

## write out: 
write.csv(amgl_tofill, "data-processed/age-at-maturity-gen-length-compilation.csv", row.names = FALSE)

