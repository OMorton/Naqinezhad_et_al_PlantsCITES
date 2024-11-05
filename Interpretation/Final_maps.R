
## Set working directory and package location.
## Change this in yours

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sp)
library(ggpubr)
library(sf)
library(viridis)

## To read in the data with the exporter country like
## CITES_IUCN_Timber_WITH_EXPORTERS.csv or CITES_IUCN_Products_WITH_EXPORTER.csv make sure you specify the na_strings 
## argument so the ISO code for Namibia is read in correctly (as NA the letter not NA the placeholder for missing values)

## First Read in the CITES species country level data and attach the country names and other relevant info.

## read in the cites data

CITES_IUCN_Timber_Countries <- data.table::fread("Outputs/Data_preparation/Timber/CITES_IUCN_Timber_Countries.csv",
                                                  na.strings = "") %>% select(-V1) %>%
  mutate(Exporter = ifelse(is.na(Exporter), "NA", Exporter),
         Importer = ifelse(is.na(Importer), "NA", Importer))

CITES_IUCN_Live_Countries <- data.table::fread ("Outputs/Data_preparation/Live/CITES_IUCN_Live_Countries.csv",
                                                na.strings = "") %>%
  mutate(Exporter = ifelse(is.na(Exporter), "NA", Exporter),
         Importer = ifelse(is.na(Importer), "NA", Importer))

CITES_IUCN_Products_Countries <- data.table::fread ("Outputs/Data_preparation/Products/CITES_IUCN_Products_Countries.csv",
                                                    na.strings = "")  %>%
  mutate(Exporter = ifelse(is.na(Exporter), "NA", Exporter),
         Importer = ifelse(is.na(Importer), "NA", Importer))


## https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv
Alpha_codes <- data.table::fread("Data/Preparation/Alpha_codes.csv") %>%
  mutate(alpha_2 = ifelse(is.na(alpha_2), "NA", alpha_2))

#### Exporter reported ####

Alpha_sum <- Alpha_codes %>% select(1,2, 6,7) %>% rename(Exporter = 2)


CITES_IUCN_Timber_Countries_E<- left_join (CITES_IUCN_Timber_Countries, Alpha_sum, by = "Exporter") %>%
  filter(Exporter != "XX")

CITES_IUCN_Live_Countries_E<- left_join(CITES_IUCN_Live_Countries, Alpha_sum, by = "Exporter") %>%
  filter(Exporter != "XX")

CITES_IUCN_Products_Countries_E<- left_join(CITES_IUCN_Products_Countries, Alpha_sum, by = "Exporter") %>%
  filter(Exporter != "XX")

timber_proportions <- CITES_IUCN_Timber_Exporters_E %>% group_by(name, Taxon) %>% summarise(Volume = sum (Vol)) %>% 
  mutate(proportion = Volume/sum(Volume))

products_proportions <- CITES_IUCN_Products_Exporters_E %>% group_by(name, Taxon) %>% summarise(Mass = sum (mass)) %>% 
  mutate(proportion = Mass/sum(Mass))

live_proportions <- CITES_IUCN_Live_Exporters_E %>% group_by(name, Taxon) %>% summarise(Number = sum (n)) %>% 
  mutate(proportion = Number/sum(Number))

#### Plots ####
Country_dat_Vol_Timber <- CITES_IUCN_Timber_Countries %>% group_by(Exporter, name, region, subregion) %>%
  filter(Year>1999) %>%
  summarise(Volume = sum(Volume)) %>%
  rename(Country = name)%>%
  mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                             grepl("union", Country) ~ "Reunion",
                             grepl("Cura", Country) ~ "Curacao",
                             TRUE ~ Country))

Exp_Vol_Timber <- Country_dat_Vol_Timber %>% 
  # deal with Namibia
  mutate(Exporter = ifelse(is.na(Exporter), "NA", Exporter)) %>%
  rename(iso_a2 = Exporter) %>%
  select(-subregion)

Exp_Vol_Timber %>% summarise (min_Vol = min(Volume), max_Vol = max(Volume))


#### Preparation for Live ####

Country_dat_N_Live <- CITES_IUCN_Live_Countries %>% group_by(Exporter, name, region, subregion) %>%
  filter(Year>1999) %>%
  summarise(Number = sum(n)) %>%
  rename(Country = name)%>%
  mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                             grepl("union", Country) ~ "Reunion",
                             grepl("Cura", Country) ~ "Curacao",
                             TRUE ~ Country))

Exp_N_Live <- Country_dat_N_Live %>% 
  # deal with Namibia
  mutate(Exporter = ifelse(is.na(Exporter), "NA", Exporter)) %>%
  rename(iso_a2 = Exporter) %>%
  select(-subregion)

Exp_N_Live %>% summarise (min_N = min (Number), max_N = max(Number))

#### Preparation for Products ####

Country_dat_mass_Products <- CITES_IUCN_Products_Countries %>% group_by(Exporter, name, region, subregion) %>%
  filter(Year>1999) %>%
  summarise(Mass = sum(mass)) %>%
  rename(Country = name)%>%
  mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                             grepl("union", Country) ~ "Reunion",
                             grepl("Cura", Country) ~ "Curacao",
                             TRUE ~ Country))

Exp_mass_Products <- Country_dat_mass_Products %>% 
  # deal with Namibia
  mutate(Exporter = ifelse(is.na(Exporter), "NA", Exporter)) %>%
  rename(iso_a2 = Exporter) %>%
  select(-subregion)

Exp_mass_Products %>% summarise (min_Mass = min(Mass), max_Mass = max(Mass))


world <- ne_countries(scale = "medium", returnclass = "sf") 

## No NA
test <-  world %>% select(iso_a2, admin)
Check <- left_join(Exp_Vol_Timber, test)
Check %>% group_by(iso_a2) %>% filter(n()>1)

Check <- left_join(Exp_mass_Products, test)
Check %>% group_by(iso_a2) %>% filter(n()>1)

Check <- left_join(Exp_N_Live, test)
Check %>% group_by(iso_a2) %>% filter(n()>1)

ExVol_Timber <- left_join(world, Exp_Vol_Timber)

Exmass_Products <- left_join(world, Exp_mass_Products)

ExN_Live <- left_join(world, Exp_N_Live)


#### Timber ####
length(unique(Exp_Vol_Timber$Country)) ## 72

Timber_V <- ggplot() + geom_sf(data = ExVol_Timber, aes(fill = Volume), colour = "white", size = .5) +
  scale_fill_viridis(name = "Volume", trans = "log10", na.value="grey90",
                     option = "viridis", 
                     breaks = c(100, 10000,1000000), labels = c(100, 10000,1000000), limits = c(1, 180000000)) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "none")

#### Live ####
length(unique(ExN_Live$Country)) ## 37
check <- ExN_Live[,c(1:10, 50:67)]
Live_N <- ggplot() + geom_sf(data = ExN_Live, aes(fill = Number), colour = "white", size = .5) +
  scale_fill_viridis(name = "Number", trans = "log10", na.value="grey90",
                     option = "viridis", 
                     breaks = c(100, 10000,1000000), labels = c(100, 10000,1000000), limits = c(1, 180000000)
                     ) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "none")

#### Products ####
length(unique(Exmass_Products$Country)) ## 46

Products_mass <- ggplot() + geom_sf(data = Exmass_Products, aes(fill = Mass), colour = "white", size = .5) +
  scale_fill_viridis(name = "Mass", trans = "log10", na.value="grey90",
                     option = "viridis", 
                     breaks = c(100, 10000,1000000), labels = c(100, 10000,1000000), limits = c(1, 180000000)) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "none")


library(ggpubr)

Vol_exporters <- ggarrange(Timber_V, Live_N, Products_mass, ncol = 1, nrow = 3,  labels = c("A", "B", "C"))



######IMPORTER MAP###########

Alpha_sum <- Alpha_codes %>% select(1,2, 6,7) %>% rename(Importer = 2)

CITES_IUCN_Timber_Countries_I<- left_join(CITES_IUCN_Timber_Countries, Alpha_sum, by = "Importer") %>%
  filter(Importer != "XX")

CITES_IUCN_Products_Countries_I<- left_join(CITES_IUCN_Products_Countries, Alpha_sum, by = "Importer") %>%
  filter(Importer != "XX")

CITES_IUCN_Live_Countries_I<- left_join(CITES_IUCN_Live_Countries, Alpha_sum, by = "Importer") %>%
  filter(Importer != "XX")

##Delete those rows with no specified exporters (XX in the exporters column and without "name" in the name column)

timber_proportions <- CITES_IUCN_Timber_Countries_I %>% group_by(name) %>% summarise(Volume = sum (Volume)) %>% 
  mutate(proportion = Volume/sum(Volume))

products_proportions <- CITES_IUCN_Products_Countries_I %>% group_by(name) %>% summarise(Mass = sum (mass)) %>% 
  mutate(proportion = Mass/sum(Mass))

live_proportions <- CITES_IUCN_Live_Countries_I %>% group_by(name) %>% summarise(Number = sum (n)) %>% 
  mutate(proportion = Number/sum(Number))


Country_dat_Vol_Timber_2 <- CITES_IUCN_Timber_Countries %>% group_by(Importer, name, region, subregion) %>%
  filter(Year>1999) %>%
  summarise(Volume = sum(Volume)) %>%
  rename(Country = name)%>%
  mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                             grepl("union", Country) ~ "Reunion",
                             grepl("Cura", Country) ~ "Curacao",
                             TRUE ~ Country))

Exp_Vol_Timber_2 <- Country_dat_Vol_Timber_2 %>% 
  # deal with Namibia
  mutate(Importer = ifelse(is.na(Importer), "NA", Importer)) %>%
  rename(iso_a2 = Importer) %>%
  select(-subregion)

Exp_Vol_Timber_2 %>% summarise (min_Vol = min(Volume), max_Vol = max(Volume))


#### Preparation for Live ####

Country_dat_N_Live_2 <- CITES_IUCN_Live_Countries %>% group_by(Importer, name, region, subregion) %>%
  filter(Year>1999) %>%
  summarise(Number = sum(n)) %>%
  rename(Country = name)%>%
  mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                             grepl("union", Country) ~ "Reunion",
                             grepl("Cura", Country) ~ "Curacao",
                             TRUE ~ Country))

Exp_N_Live_2 <- Country_dat_N_Live_2 %>% 
  # deal with Namibia
  mutate(Importer = ifelse(is.na(Importer), "NA", Importer)) %>%
  rename(iso_a2 = Importer) %>%
  select(-subregion)

Exp_N_Live_2 %>% summarise (min_N = min (Number), max_N = max(Number))

#### Preparation for Products ####

Country_dat_mass_Products_2 <- CITES_IUCN_Products_Countries %>% group_by(Importer, name, region, subregion) %>%
  filter(Year>1999) %>%
  summarise(Mass = sum(mass)) %>%
  rename(Country = name)%>%
  mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                             grepl("union", Country) ~ "Reunion",
                             grepl("Cura", Country) ~ "Curacao",
                             TRUE ~ Country))

Exp_mass_Products_2 <- Country_dat_mass_Products_2 %>% 
  # deal with Namibia
  mutate(Importer = ifelse(is.na(Importer), "NA", Importer)) %>%
  rename(iso_a2 = Importer) %>%
  select(-subregion)

Exp_mass_Products_2 %>% summarise (min_Mass = min(Mass), max_Mass = max(Mass))


world <- ne_countries(scale = "medium", returnclass = "sf") 

## No NA
test <-  world %>% select(iso_a2, admin)
Check <- left_join(Exp_Vol_Timber_2, test)
Check %>% group_by(iso_a2) %>% filter(n()>1)

Check <- left_join(Exp_mass_Products_2, test)
Check %>% group_by(iso_a2) %>% filter(n()>1)

Check <- left_join(Exp_N_Live_2, test)
Check %>% group_by(iso_a2) %>% filter(n()>1)

ExVol_Timber_2 <- left_join(world, Exp_Vol_Timber_2)

Exmass_Products_2 <- left_join(world, Exp_mass_Products_2)

ExN_Live_2 <- left_join(world, Exp_N_Live_2)


#### Timber ####
Timber_V_2 <- ggplot() + geom_sf(data = ExVol_Timber_2, aes(fill = Volume), colour = "white", size = .5) +
  scale_fill_viridis(name = "Volume", trans = "log10", na.value="grey90",
                     option = "viridis", 
                     breaks = c(100, 10000,1000000), labels = c(100, 10000,1000000), limits = c(1, 180000000)) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "right")

#### Live ####
check_2 <- ExN_Live_2 [,c(1:10, 50:67)]
Live_N_2 <- ggplot() + geom_sf(data = ExN_Live_2, aes(fill = Number), colour = "white", size = .5) +
  scale_fill_viridis(name = "Number", trans = "log10", na.value="grey90",
                     option = "viridis", 
                     breaks = c(100, 10000,1000000), labels = c(100, 10000,1000000), limits = c(1, 180000000)
  ) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "right")

#### Products ####

Products_mass_2 <- ggplot() + geom_sf(data = Exmass_Products_2, aes(fill = Mass), colour = "white", size = .5) +
  scale_fill_viridis(name = "Mass", trans = "log10", na.value="grey90",
                     option = "viridis", 
                     breaks = c(100, 10000,1000000), labels = c(100, 10000,1000000), limits = c(1, 180000000)) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "right")


library(ggpubr)

Vol_Importers <- ggarrange(Timber_V_2, Live_N_2, Products_mass_2, ncol = 1, nrow = 3,  labels = c("D", "E", "F"))

Vol_exporters <- ggarrange(Timber_V, Live_N, Products_mass, ncol = 1, nrow = 3,  labels = c("A", "B", "C"))


Figure1 <- ggarrange(Vol_exporters, Vol_Importers, ncol = 2, nrow = 1) 



