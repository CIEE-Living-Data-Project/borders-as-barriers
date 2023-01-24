## script to pull together body size data for our species 
library(tidyverse)
library(readxl)

## read in taxized species list 
sp <- read.csv("data-processed/initial-species-list_taxonomy.csv")

cols_to_keep <- c("scientificName", "BodySize", "Units", "Field", "Code", "BodySizeSource")

#-----------------
# Amphibio
#------------------
pulldata("amphibio")
head(amphibio)
amphibio$Body_size_mm

## search for our species
amph_oursp <- amphibio[which(amphibio$Species %in% sp$scientificName),]

amph_oursp <- amph_oursp %>%
  select(Species, Body_size_mm) %>%
  distinct()

amph_sub = amph_oursp %>%
  rename("BodySize" = Body_size_mm,
         "scientificName" = Species) %>%
  mutate(BodySizeSource = "Amphibio", 
         Units = "mm",
         Field = "Body_size_mm", 
         Code = "BodyLength")%>%
  filter(!is.na(BodySize))

## how many? 
length(unique(amph_sub$scientificName))
## 30 species 

amph_sub <- select(amph_sub, cols_to_keep)

#---------------------------
# GARD - feldman 2015
#---------------------------
gard <- read.csv('data-raw/species_traits/GARD/feldman_et_al._2015_lepidosaur_body_sizes_appendix_s1.csv')
gard$max.length..mm.

## search for our species
gard_oursp <- gard[which(gard$binomial %in% sp$scientificName),]

gard_oursp <- gard_oursp %>%
  select(binomial, max.length..mm.) %>%
  distinct()

gard_sub = gard_oursp %>%
  rename("BodySize" = max.length..mm.,
         "scientificName" = binomial) %>%
  mutate(BodySizeSource = "GARD - Feldman 2015", 
         Units = "mm",
         Field = "max.length..mm.", 
         Code = "MaxBodySize") %>%
  filter(!is.na(BodySize))

## how many? 
length(unique(gard_sub$scientificName))
## 25 species 

gard_sub <- select(gard_sub, cols_to_keep)

#----------------------
# Storchova & Horak 2018
#----------------------
stor <- read.csv('data-raw/species_traits/Storchova/Life-history characteristics of European birds.txt',sep="\t")
head(stor)
stor$LengthU_MEAN

## search for our species
stor_oursp <- stor[which(stor$Species %in% sp$scientificName),]

stor_oursp <- stor_oursp %>%
  select(Species, LengthU_MEAN) %>%
  distinct()

stor_sub = stor_oursp %>%
  rename("BodySize" = LengthU_MEAN,
         "scientificName" = Species) %>%
  mutate(BodySizeSource = "Storchova & Horak 2018", 
         Units = "cm",
         Field = "LengthU_MEAN", 
         Code = "MeanBodyLength")%>%
  filter(!is.na(BodySize))

## how many? 
length(unique(stor_sub$scientificName))
## 2 species 

stor_sub <- select(stor_sub, cols_to_keep)


#-------
# Trochet
#-------
troch <- read.csv('data-raw/species_traits/Trochet/Trochet_et_al_2014_bodysize.csv')
head(troch)
colnames(troch)

## search for our species
troch_oursp <- troch[which(troch$Species %in% sp$scientificName),]
## none


#-------
# AVONET
#-------
avonet <- read.csv('data-raw/species_traits/AVONET/AVONET_Raw.csv')
head(avonet)
avonet$Tarsus.Length

avonet <- filter(avonet, !is.na(Species1))

## search for our species
avo_oursp <- avonet[which(avonet$Species1 %in% sp$scientificName),]

avo_oursp <- avo_oursp %>%
  select(Species1, Tarsus.Length) %>%
  distinct()

avo_sub = avo_oursp %>%
  rename("BodySize" = Tarsus.Length,
         "scientificName" = Species1) %>%
  mutate(BodySizeSource = "AVONET", 
         Units = "cm",
         Field = "Tarsus.Length", 
         Code = "MeanBodyLength")%>%
  filter(!is.na(BodySize))

## how many? 
length(unique(avo_sub$scientificName))
## 38 species 

avo_sub <- select(avo_sub, cols_to_keep)



#----------------------
#Pincebourde et al 2021
#----------------------
pin <- read.csv('data-raw/species_traits/Pincebourde_et_al_2021/Pincebourde_et_al_2021_Dryad_dataset (1).csv')
head(pin)
pin$length_mm

## search for our species
pin_ourspp <- pin[which(pin$Species_latin_name_or_taxa %in% sp$scientificName),]
## none

#--------
# BioTIME
#--------
bio <- read.csv('data-raw/species_traits/BIOTIME/bt_names_all_traits.csv')
head(bio)
bio$TR_BodyLength_mm

## search for our species
bio_ourspp <- bio[which(bio$TidyBTName %in% sp$scientificName),]

bio_ourspp <- bio_ourspp %>%
  select(TidyBTName, TR_BodyLength_mm) %>%
  distinct()

bio_sub = bio_ourspp %>%
  rename("BodySize" = TR_BodyLength_mm,
         "scientificName" = TidyBTName) %>%
  mutate(BodySizeSource = "BIOTIME", 
         Units = "mm",
         Field = "TR_BodyLength_mm", 
         Code = "MeanBodyLength") %>%
  filter(!is.na(BodySize))

## how many? 
length(unique(bio_sub$scientificName))
## 1 species 

bio_sub <- select(bio_sub, cols_to_keep)

#----------
# Pantheria
#----------
panth1 <- read.delim('data-raw/species_traits/Pantheria/ECOL_90_184/PanTHERIA_1-0_WR05_Aug2008.txt')
head(panth1)
panth1$X13.1_AdultHeadBodyLen_mm

## search for our species
panth1_ourspp <- panth1[which(panth1$MSW05_Binomial %in% sp$scientificName),]

panth1_ourspp <- panth1_ourspp %>%
  select(MSW05_Binomial, X13.1_AdultHeadBodyLen_mm) %>%
  distinct()

panth1_sub = panth1_ourspp %>%
  rename("BodySize" = X13.1_AdultHeadBodyLen_mm,
         "scientificName" = MSW05_Binomial) %>%
  mutate(BodySizeSource = "Pantheria - 2005", 
         Units = "mm",
         Field = "AdultHeadBodyLen_mm", 
         Code = "MeanBodyLength")%>%
  filter(!is.na(BodySize))

## how many? 
length(unique(panth1_sub$scientificName))
## 32 species 

panth2 <- read.delim('data-raw/species_traits/Pantheria/ECOL_90_184/PanTHERIA_1-0_WR93_Aug2008.txt')
head(panth2)
panth2$X13.1_AdultHeadBodyLen_mm

## search for our species
panth2_ourspp <- panth2[which(panth2$MSW05_Binomial %in% sp$scientificName),]
## none


#### combine them all
all_bs <- rbind(bio_sub, amph_sub) %>%
  rbind(., gard_sub) %>%
  rbind(., stor_sub) %>%
  rbind(., avo_sub) %>%
  rbind(., panth1_sub)

length(unique(all_bs$scientificName)) ## 123 species

## write out: 
write.csv(all_bs, "data-processed/body-size-compilation.csv", row.names = FALSE)



       