## script to pull together age at maturity / generation time data 
## developed by: Nikki Moore
library(tidyverse)
library(readxl)

## read in taxized species list 
sp <- read.csv("data-processed/final-species-list_taxonomy.csv")

cols_to_keep <- c("scientificName", "BodyMass",
                  "Units", "Field", "Code", "BodyMassSource")

#-----------------
# Amphibio
#------------------
pulldata("amphibio")
head(amphibio)
amphibio$Body_mass_g

## search for our species
amph_oursp <- amphibio[which(amphibio$Species %in% sp$scientificName),]

amph_oursp <- amph_oursp %>%
  select(Species, Body_mass_g) %>%
  distinct()

amph_sub = amph_oursp %>%
  rename("BodyMass" = Body_mass_g,
         "scientificName" = Species) %>%
  mutate(BodyMassSource = "Amphibio", 
         Units = "g",
         Field = "Body_mass_g", 
         Code = "BodyMass")%>%
  filter(!is.na(BodyMass))

## how many? 
length(unique(amph_sub$scientificName))
## 8 species 

amph_sub <- select(amph_sub, cols_to_keep)


#-----------------
# Amniota
#------------------
amniota <- read.csv("data-raw/species_traits/amniota/ECOL_96_269/Data_Files/Amniote_Database_Aug_2015.csv")
head(amniota)
amniota$adult_body_mass_g
amniota$genus_species <- paste(amniota$genus, amniota$species, sep = " ")

## search for our species
amniota_oursp <- amniota[which(amniota$genus_species %in% sp$scientificName),]

amniota <- amniota %>%
  select(genus_species, adult_body_mass_g) %>%
  distinct()

amni_sub = amniota_oursp %>%
  mutate(Code = "AdultBodyMass") %>%
  rename("scientificName" = genus_species,
         "BodyMass" = adult_body_mass_g) %>%
  mutate(BodyMassSource = "Amniota", 
         Units = "g",
         Field = "adult_body_mass_g") %>%
  filter(!is.na(BodyMass)) %>%
  filter(BodyMass != "-999")

## how many? 
length(unique(amni_sub$scientificName))
## 70 species 

amni_sub <- select(amni_sub, cols_to_keep)


#-----------------
# Liselevand
#------------------
lislevand <- read_excel("data-raw/species_traits/Lislevand_et_al_2007/Lislevand2007.xlsx") %>%
  rename("genus_species" = Species_name) %>%
  mutate(kingdom = "Animalia",
         genus = str_split_fixed(genus_species, " ", 2)[,1]) 
colnames(lislevand)
lislevand$M_mass
lislevand$F_mass
lislevand$unsexed_mass

## search for our species
lis_oursp <- lislevand[which(lislevand$genus_species %in% sp$scientificName),]

lislevand <- lislevand %>%
  select(genus_species, M_mass, F_mass, unsexed_mass) %>%
  distinct()

lis_sub = lis_oursp %>%
  gather(key = "Field", value = "BodyMass", 
         c(M_mass, F_mass, unsexed_mass)) %>%
  rename("scientificName" = genus_species) %>%
  mutate(BodyMassSource = "Liselevand et al. 2007", 
         Units = "g",
         Code = ifelse(Field == "M_mass", "MaleBodyMass",
                       ifelse(Field == "F_mass", "FemaleBodyMass",
                              "UnsexedBodyMass"))) %>%
  filter(!is.na(BodyMass)) %>%
  filter(BodyMass != "-999")

## how many? 
length(unique(lis_sub$scientificName))
## 15 species 

lis_sub <- select(lis_sub, cols_to_keep)


#-------
# AnAge
#-------
anage <- read.delim("data-raw/species_traits/AnAge/anage_data.txt")
head(anage)
anage$Body.mass..g.
anage$genus_species <- paste(anage$Genus, anage$Species, sep = " ")

## search for our species
anage_oursp <- anage[which(anage$genus_species %in% sp$scientificName),]

anage_oursp <- anage_oursp %>%
  select(genus_species, Body.mass..g.) %>%
  distinct()

anage_sub = anage_oursp %>%
  mutate(Field = "BodyMass",
         Code = "BodyMass") %>%
  rename("scientificName" = genus_species,
         "BodyMass" = Body.mass..g.) %>%
  mutate(BodyMassSource = "AnAge", 
         Units = "g")%>%
  filter(!is.na(BodyMass))

## how many? 
length(unique(anage_sub$scientificName))
## 12 species 

anage_sub <- select(anage_sub, cols_to_keep)


#-------------
# Elton traits
#-------------
etraits <- read_delim("data-raw/species_traits/EltonTraits/BirdFuncDat.txt") %>%
  rename("genus_species" = Scientific, "order" = IOCOrder, "family" = BLFamilyLatin) %>%
  mutate(kingdom = "Animalia", class = "Aves",
         genus = str_split_fixed(genus_species, " ", 2)[,1]) %>%
  filter(!is.na(genus_species))
head(etraits)
etraits$`BodyMass-Value`
colnames(etraits)[which(colnames(etraits) == "BodyMass-Value")] = "BodyMass_Value"

## search for our species
etraits_oursp <- etraits[which(etraits$genus_species %in% sp$scientificName),]

etraits_oursp <- etraits_oursp %>%
  select(genus_species, BodyMass_Value) %>%
  distinct()

etraits_sub = etraits_oursp %>%
  mutate(Field = "BodyMass_Value",
         Code = "BodyMass") %>%
  rename("scientificName" = genus_species,
         "BodyMass" = BodyMass_Value) %>%
  mutate(BodyMassSource = "EltonTraits", 
         Units = "g")%>%
  filter(!is.na(BodyMass))

## how many? 
length(unique(etraits_sub$scientificName))
## 16 species 

etraits_sub <- select(etraits_sub, cols_to_keep)


#---------------------------
# GARD - feldman 2015
#---------------------------
gard <- read.csv('data-raw/species_traits/GARD/feldman_et_al._2015_lepidosaur_body_sizes_appendix_s1.csv')
gard$mass..g.

## search for our species
gard_oursp <- gard[which(gard$binomial %in% sp$scientificName),]

gard_oursp <- gard_oursp %>%
  select(binomial, mass..g.) %>%
  distinct()

gard_sub = gard_oursp %>%
  rename("BodyMass" = mass..g.,
         "scientificName" = binomial) %>%
  mutate(BodyMassSource = "GARD - Feldman 2015", 
         Units = "g",
         Field = "mass..g.", 
         Code = "BodyMass") %>%
  filter(!is.na(BodyMass))

## how many? 
length(unique(gard_sub$scientificName))
## 23 species 

gard_sub <- select(gard_sub, cols_to_keep)

#---------------------------
# GARD - appendix 1
#---------------------------
gardapp1 <- read_excel("data-raw/species_traits/GARD/appendix_1_-_data.xlsx") %>%
  rename("genus_species" = species, "class" = Class) %>%
  mutate(genus = str_split_fixed(genus_species, " ", 2)[,1],
         kingdom = "Animalia")
colnames(gardapp1) = str_replace_all(colnames(gardapp1), " ", "_")
colnames(gardapp1) = str_replace_all(colnames(gardapp1), "\\(", "")
colnames(gardapp1) = str_replace_all(colnames(gardapp1), "\\)", "")
gardapp1$adult_mass_g

## search for our species
gard1_oursp <- gardapp1[which(gardapp1$genus_species %in% sp$scientificName),]

gard1_oursp <- gard1_oursp %>%
  select(genus_species, adult_mass_g) %>%
  distinct()

gard1_sub = gard1_oursp %>%
  rename("BodyMass" = adult_mass_g,
         "scientificName" = genus_species) %>%
  mutate(BodyMassSource = "GARD - appendix 1", 
         Units = "g",
         Field = "adult mass (g)", 
         Code = "BodyMass") %>%
  filter(!is.na(BodyMass))

## how many? 
length(unique(gard1_sub$scientificName))
## 43 species 

gard1_sub <- select(gard1_sub, cols_to_keep)


####################################
##  GARD [novosolov_et_al._2017]  ##
####################################
gardnovo <- read_excel("data-raw/species_traits/GARD/novosolov_et_al._2017_appendix_1_population_densities___range_sizes.xlsx") %>%
  rename("genus_species" = Binomial) %>%
  mutate(genus = str_split_fixed(genus_species, " ", 2)[,1],
         kingdom = "Animalia")
colnames(gardnovo) = str_replace_all(colnames(gardnovo), " ", "_")
colnames(gardnovo) = str_replace_all(colnames(gardnovo), "\\(", "")
colnames(gardnovo) = str_replace_all(colnames(gardnovo), "\\)", "")
gardnovo$Body_mass_g

## search for our species
gardn_oursp <- gardnovo[which(gardnovo$genus_species %in% sp$scientificName),]

gardn_oursp <- gardn_oursp %>%
  select(genus_species, Body_mass_g) %>%
  distinct()

gardn_sub = gardn_oursp %>%
  rename("BodyMass" = Body_mass_g,
         "scientificName" = genus_species) %>%
  mutate(BodyMassSource = "GARD - novosolov_et_al._2017", 
         Units = "g",
         Field = "Body mass (g)", 
         Code = "BodyMass") %>%
  filter(!is.na(BodyMass))

## how many? 
length(unique(gardn_sub$scientificName))
## 7 species 

gardn_sub <- select(gardn_sub, cols_to_keep)



#--------------
# GARD lizards
#--------------
gardliz <- read_excel("data-raw/species_traits/GARD/lizard_body_temperatures_natural_history_and_life-history_traits.xlsx") 
colnames(gardliz) = str_replace_all(colnames(gardliz), " ", "_")
gardliz$maximum_body_mass

## search for our species
gardliz_oursp <- gardliz[which(gardliz$species %in% sp$scientificName),]

gardliz_oursp <- gardliz_oursp %>%
  select(species, maximum_body_mass) %>%
  distinct()

gardliz_sub = gardliz_oursp %>%
  rename("BodyMass" = maximum_body_mass,
         "scientificName" = species) %>%
  mutate(BodyMassSource = "GARD - novosolov_et_al._2017", 
         Units = "g",
         Field = "maximum body mass", 
         Code = "MaxBodyMass") %>%
  filter(!is.na(BodyMass))

## how many? 
length(unique(gardliz_sub$scientificName))
## 9 species 

gardliz_sub <- select(gardliz_sub, cols_to_keep)



#----------
# Pantheria
#----------
panth1 <- read.delim('data-raw/species_traits/Pantheria/ECOL_90_184/PanTHERIA_1-0_WR05_Aug2008.txt')
head(panth1)
panth1$X5.1_AdultBodyMass_g

## search for our species
panth1_ourspp <- panth1[which(panth1$MSW05_Binomial %in% sp$scientificName),]

panth1_ourspp <- panth1_ourspp %>%
  select(MSW05_Binomial, X5.1_AdultBodyMass_g) %>%
  distinct()

panth1_sub = panth1_ourspp %>%
  rename(BodyMass = X5.1_AdultBodyMass_g,
         "scientificName" = MSW05_Binomial) %>%
  mutate(BodyMassSource = "Pantheria - 2005", 
         Units = "g",
         Field = "AdultBodyMass_g", 
         Code = "AdultBodyMass")%>%
  filter(!is.na(BodyMass)) %>%
  filter(BodyMass != "-999")

## how many? 
length(unique(panth1_sub$scientificName))
## 25 species 

panth1_sub <- select(panth1_sub, cols_to_keep)


#----------
# Heinen
#----------
heinen <- read_delim("data-raw/species_traits/Heinen/doi_10.5061_dryad.s522m__v1/Data_Traits_IslandFrugivores.txt") %>%
  rename("family" = Family, "class" = Class, "order" = Order, "genus_species" = Species) %>%
  mutate(kingdom = "Animalia",
         genus = str_split_fixed(genus_species, " ", 2)[,1]) 
colnames(heinen)
heinen$Body.mass

## search for our species
hein_ourspp <- heinen[which(heinen$genus_species %in% sp$scientificName),]

hein_ourspp <- hein_ourspp %>%
  select(genus_species, Body.mass) %>%
  distinct()

hein_sub = hein_ourspp %>%
  rename(BodyMass = Body.mass,
         "scientificName" = genus_species) %>%
  mutate(BodyMassSource = "Heinen et al. 2017", 
         Units = "g",
         Field = "Body.mass", 
         Code = "BodyMass")%>%
  filter(!is.na(BodyMass)) %>%
  filter(BodyMass != "-999")

## there are none







#### combine them all
all_bm <- rbind(lis_sub, amph_sub) %>%
  rbind(., amni_sub) %>%
  rbind(., anage_sub) %>%
  rbind(., etraits_sub) %>%
  rbind(., gard_sub) %>%
  rbind(., gard1_sub)  %>%
  rbind(., panth1_sub)%>%
  rbind(., gardn_sub) %>%
  rbind(., gardliz_sub)

length(unique(all_bm$scientificName)) ## 81 species

## add empty rows for species with missing body size
no_bm <- sp$scientificName[which(!sp$scientificName %in% all_bm$scientificName)]

bm_tofill = left_join(sp, all_bm)

## write out: 
write.csv(bm_tofill, "data-processed/age-at-maturity-gen-length-compilation.csv", row.names = FALSE)
