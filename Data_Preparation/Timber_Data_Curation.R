#################################
##---CITES raw data curation for Timber species---##
#################################

## Author - O Morton 
## Date - 27/01/22 (adapted on 14/07/22 from code given to Alice)
## Notes - Processing methods derived from Harfoot et al (2018) and coded approach from Morton et al (2022)

## Purpose
## To read in the raw cites data product and process it to include WOES and focus only on commercial and wild sourced trades 
## between 2000 and 2020
## Runtime ~ 5 minutes

#### Packages and presets ####
## for all plotting and data handling (you might need to install it the first time)
library(tidyverse)

#### Bulk data download and tidying ####

## Read in the 46 seperate .csv's from CITES latest Bulk release 2021 v1.
## https://trade.cites.org/ database is availble here needs downloading and saving.
## Then you supply the code below with the path to the entire file of csv's not to an individual one, the function then reads in all 46 csvs and 
## appends them.
## Takes a few minutes to run ~ results in a database with 23680557 records.
CITES_MASTER <- list.files(path="D:/CITES paper/Analysis/CITES_Data", full.names = TRUE, pattern="*.csv") %>% 
  lapply(read_csv,  na = "", col_types = cols(Unit = col_character(), Import.permit.RandomID = col_character(),
                                    Export.permit.RandomID = col_character(), Origin.permit.RandomID = col_character(),
                                    Purpose = col_character())) %>% 
  bind_rows



## Remove all re-exports as per published CITES guidelines and Pers Comm with UNEP-WMC and CITES
## remove all reports where the origin is stated and is not the same as the exporter.
## This avoids double counting i.e. where a trade passes through multiple countries. 
CITES_TRUE <- CITES_MASTER %>% filter(Origin == Exporter  | is.na(Origin)) # this leaves 13.4 out 23.6 million shipments

## Focus on only the target taxa, you may want to broaden this to other taxa, you can add other families to the bracket
## to get all target plant groups at the minute I've just added all timber families
CITES_Plants <- CITES_TRUE %>% filter(is.na(Class))

#### Subset into wild source and commercial purpose ####

## Wild source as defined in Morton et al (2022), a full list of codes are available at
## https://trade.cites.org/cites_trade_guidelines/en-CITES_Trade_Database_Guide.pdf
## If we want to use captive as well just need to add some more conditions to this to class trades as wild, captive or unknown.
CITES_Plants$WildSource <- if_else(CITES_Plants$Source %in% c("W", "X", "R"), "Yes", "No")

CITES_Plants_Source <- CITES_Plants %>% filter(WildSource == "Yes")

## Commercial purpose as defined by ourselves. We have justified the inclusion of Personal (Code P) in addition to commercial (Code T)
## to capture aspects of the pet trade, as done in previous studies see Bush et al. 2014, but for plants this probably isnt relevant to we 
## only need to include T.
CITES_Plants$PurposeC <- if_else(CITES_Plants$Purpose %in% c("T"), "Commercial", "Not Commercial")

## Filter to include only wild and commercial trades (this will shrink the dataset substantially)
## 420,561 records
CITES_Wild_Com <- CITES_Plants %>% filter(WildSource == "Yes", PurposeC == "Commercial")

unique(CITES_Wild_Com$Unit)
unique(CITES_Wild_Com$Term)
CITES_Wild_Com <- CITES_Plants %>% filter(WildSource == "Yes", PurposeC == "Commercial", Year %in% c(2000:2020))

## 358,641 records
write.csv(CITES_Wild_Com, "Outputs/Data_preparation/Timber/CITES_Wild_Com_new.csv", na = "")


## Tidy and remove species with uncertain naming and species not listed in the Appendices
CITES_Wild_Com <- CITES_Plants %>% 
  filter(WildSource == "Yes", PurposeC == "Commercial", Year %in% c(2000:2020), 
         !grepl("Plantae", Taxon), !grepl("spp", Taxon),!grepl("hybrid", Taxon))

CITES_Wild_Com <- CITES_Plants %>% 
  filter(WildSource == "Yes", PurposeC == "Commercial", Year %in% c(2000:2020), 
         !grepl("Plantae", Taxon), !grepl("spp", Taxon),!grepl("hybrid", Taxon), !Appendix == "N" )

## Importer reported has more records (full re-analysis with both in time)
CITES_Wild_Com %>% group_by(Reporter.type) %>% tally()

CITES_Wild_Com_Imp <- CITES_Wild_Com %>% filter(Reporter.type == "I", Year %in% c(1999:2020), 
                                                !grepl("spp", Taxon), !grepl("Plantae", Taxon), 
                                                !grepl("hybrid", Taxon), !Appendix == "N",
                                      WildSource == "Yes", PurposeC == "Commercial") 


CITES_Wild_Com_Imp <- CITES_Wild_Com_Imp %>% filter(!Term %in% c("cultures", "plywood", "raw corals", "gall" , "transformed woods")) 


## Focus on terms and units that could be converted to timber volumes

unique(CITES_Wild_Com_Imp$Term)
unique(CITES_Wild_Com_Imp$Unit)


CITES_Timber_Terms <- CITES_Wild_Com_Imp  %>% filter(Term %in% c('sawn wood', 'logs', 'chips','timber', 'timber pieces',
                                      'veneer', 'wood product'), Unit %in% c('m3', 'cm3', 'kg', 'g', 'mg'))
unique(CITES_Timber_Terms$Family) ## 15 families
unique(CITES_Timber_Terms$Taxon) ## 60 taxa


## We have 60 species that are traded in woody terms and have units that are volumes or are masses 
## that can be converted to volumes
Wood_Species <- CITES_Timber_Terms %>% distinct(Family, Taxon)

## Add collumn to identify if it needs density or not
CITES_Density <- CITES_Timber_Terms %>% filter(Unit %in% c('kg', 'g', 'mg')) %>%
  distinct(Family, Taxon) %>% mutate(Comment = "Needs density")

Wood_Species1 <- left_join(Wood_Species, CITES_Density) %>% 
  mutate(Comment = ifelse(is.na(Comment), "Already recorded in volume units", Comment))

## Read in the Kew list of timber species and the BGCI species list
Kew_List <- data.table::fread("Data/Preparation/Kew_CITES_Species_List.csv") %>% mutate_all(na_if,"")
BGCI_Tree <- read.csv ("Data/Preparation/BGCI_Tree_List.csv") %>% 
  mutate(BGCI_TREE = "Yes") %>% select(TaxonName, BGCI_TREE) %>% rename(Taxon = 1)

## First broadly match the timber genuses in the kew list
Wood_Kew_Check <- Wood_Species1 %>% mutate(Kew_Genus = case_when(grepl("Taxus", Taxon) ~ "Yes",
                                                grepl("Swietenia", Taxon) ~ "Yes",
                                                grepl("Guibourtia", Taxon) ~ "Yes",
                                                grepl("Guaiacum", Taxon) ~ "Yes",
                                                grepl("Gonystylus", Taxon) ~ "Yes",
                                                grepl("Diospyros", Taxon) ~ "Yes",
                                                grepl("Cedrela", Taxon) ~ "Yes",
                                                grepl("Gyrinops", Taxon) ~ "Yes",
                                                grepl("Aquilaria", Taxon) ~ "Yes")) %>%
  ## Specifically match the timber species in the kew list
  left_join(filter(Kew_List, !is.na(Species_list)) %>% select(Species_list) %>% mutate(Kew_Species = "Yes"),
            by = c("Taxon" = "Species_list")) %>%
  ## Specifcally match the BGCI list of tree species
  left_join(BGCI_Tree) %>%
  ## Class any species as a timber providing tree if it appears on either list
  mutate(Timber_species = case_when(BGCI_TREE == "Yes" | Kew_Genus == "Yes" | Kew_Species == "Yes" ~ "Yes"))
  
  
## Extract the list of woody/timber species (There are 58)
## Species that are lost are two dalbergia (e.g. liana), tree ferns, some cacti and an aloe.
## NOTE - some cacti species are retained as wood providing, and the bitter aloe is retained (all present on the BGCI list)
CITES_Timber_Checked <- Wood_Kew_Check %>% filter(Timber_species == "Yes")

## 30 species need density
Density_needed_sp <- CITES_Timber_Checked %>% filter(Comment == "Needs density")

## Read in the wood density TRY data
TRY <- read.delim2("Data/Preparation/TRY_Wood_Density.txt") %>% filter(!is.na(TraitID)) %>%
  select(SpeciesName, AccSpeciesName, OrigValueStr, OrigUnitStr)

## Get the available wood density from TRY and tidy
Wood_dens_join <- Density_needed_sp %>% 
  mutate(Taxon = ifelse(Taxon == "Osyris lanceolata", "Osyris arborea", Taxon)) %>%
  left_join(TRY, by = c("Taxon" = "SpeciesName")) %>% select(-AccSpeciesName) %>% 
  mutate(Source = case_when(!is.na(OrigValueStr) ~ "TRY")) %>% 
  rename(Density = OrigValueStr, Original_Unit = OrigUnitStr)
  
## Use BIOMASS to find genus and family level estimate of unknown species density.
library(BIOMASS)

## Get the species list 10 NA sp
NA_dens <-Wood_dens_join %>% filter(is.na(Density))

## Make seperate columns for the taxanomic hierachy
Sp_for_BIOMASS <- NA_dens %>% separate(Taxon, c("Genus", "Species"), " ") %>% 
  unite("Taxon", 1:2, na.rm = TRUE, remove = FALSE, sep = " ")

## Get the wood density
Missing_Wood_Dens <- getWoodDensity(family = Sp_for_BIOMASS$Family,
                                    genus = Sp_for_BIOMASS$Genus, species = Sp_for_BIOMASS$Species) %>%
  rename(Density = meanWD, SD_Density = sdWD, Resolution = levelWD, Family = family) %>% 
  unite("Taxon", 2:3, na.rm = TRUE, remove = TRUE, sep = " ") %>%
  select(Family, Taxon, Density, SD_Density, Resolution) %>% mutate(Original_Unit = "g/cm3")

## NOTE the aloe ferox value is estimated at the dataset level rather than genus or family.
## Attach the TRY and BIOMASS densities together and standardise names
Full_Dens_Table_Raw <- left_join(Wood_dens_join, Missing_Wood_Dens, by = c("Family", "Taxon")) %>% 
  mutate(Density.x = as.numeric(as.character(Density.x)),
         Density = coalesce(Density.x, Density.y),
         Original_Unit = coalesce(Original_Unit.x, Original_Unit.y)) %>%
  select(-Density.x, -Density.y, -Original_Unit.y, - Original_Unit.x) %>%
  mutate(Source = ifelse(is.na(Source), "BIOMASS", Source),
         Resolution = ifelse(is.na(Resolution), "Species", Resolution))

## Lots of density units and non-standard writing
unique(Full_Dens_Table_Raw$Original_Unit)

## Tidy up unit names and standardise everything to the g/cm3 unit.
Std_Full_Dens_Table_Raw <- Full_Dens_Table_Raw %>% mutate(Clean_units = case_when(Original_Unit %in% c("g/cm^3", "g/cm3", "g cm-3") ~ "g/cm3",
                                                                   Original_Unit == "mg / mm3" ~ "mg/mm3",
                                                                   Original_Unit == "kg/m3" ~ "kg/m3",
                                                                   Original_Unit == "t m3" ~ "t/m3")) %>%
  mutate(Dens_std = case_when(Clean_units %in% c("g/cm3", "t/m3", "mg/mm3") ~ Density,
                              Clean_units == "kg/m3" ~ Density/1000),
         Unit_std = "g/cm3")

## Pull out the TRY list and take the mean and sd of the multiple obs
TRY_Sp <- Std_Full_Dens_Table_Raw %>% filter(Source == "TRY") %>% 
  group_by(Taxon, Family, Comment, Kew_Genus, Kew_Species, BGCI_TREE, Timber_species, Source, Resolution, Unit_std) %>%
  summarise(Dens_stdm = mean(Dens_std), SD_Density = sd(Dens_std))

BIOMASS_Sp <- Std_Full_Dens_Table_Raw %>% filter(Source == "BIOMASS") %>% select(-Density, -Original_Unit, -Clean_units) %>% rename(Dens_stdm = Dens_std)

## Join both datasets together
All_Dens <- rbind(TRY_Sp, BIOMASS_Sp) %>% mutate(SD_Density = ifelse(is.na(SD_Density), 0, SD_Density)) %>%
  mutate(Taxon = ifelse(Taxon == "Osyris arborea", "Osyris lanceolata", Taxon))
  

## NOTE Dalbergia lactea is only given fmaily level estimates from Biomass so we go back to TRY and calculate our own genus level estimate
Dalbergia_Genus_Est <- TRY %>% filter(grepl("Dalbergia", SpeciesName)) %>% 
  summarise(Dens_stdm = mean(as.numeric(as.character(OrigValueStr))),
            SD_Density = sd(as.numeric(as.character(OrigValueStr))))

All_Dens <- All_Dens %>% mutate(Source = ifelse(Taxon == "Dalbergia lactea", "TRY", Source),
                    Resolution = ifelse(Taxon == "Dalbergia lactea", "Genus", Resolution),
                    Dens_stdm = ifelse(Taxon == "Dalbergia lactea", Dalbergia_Genus_Est$Dens_stdm, Dens_stdm),
                    SD_Density = ifelse(Taxon == "Dalbergia lactea", Dalbergia_Genus_Est$SD_Density, SD_Density))

write.csv(All_Dens, "Data/Preparation/Wood_Density_Master.csv")
All_Dens <-  data.table::fread("Data/Preparation/Wood_Density_Master.csv")


#### CITES Unit and Term Codes  ####

## 287,865 records of timber species in trade
CITES_TRUE_Timber <- CITES_Timber_Terms %>% filter(Taxon %in% CITES_Timber_Checked$Taxon)
unique(CITES_TRUE_Timber$Term)
unique(CITES_TRUE_Timber$Unit)


## For species already in Volume units we want everything to be in m3
CITES_Vol_to_Vol <- CITES_TRUE_Timber %>% filter(Unit %in% c("m3", "cm3")) %>%
  mutate(Volume = case_when(Unit == "m3" ~ Quantity,
                            Unit == "cm3" ~ Quantity/1000000), ## to m3
         Vol_Unit = "m3")

CITES_Mass_to_Vol1 <- CITES_TRUE_Timber %>% filter(Unit %in% c("kg", "g")) %>%
  mutate(Quantity_g = case_when(Unit == "kg" ~ Quantity*1000, ## to g
                            Unit == "g" ~ Quantity))

## Add the density data to mass trade records
CITES_Mass_to_Vol2 <- left_join(CITES_Mass_to_Vol1, select(ungroup(All_Dens), Taxon, Dens_stdm)) %>%
  mutate(Volume_cm3 = Quantity_g/Dens_stdm) %>% ## v = m/d, cm3 = g/gcm3
  mutate(Volume = Volume_cm3/1000000, ## to m3
         Vol_Unit = "m3") 

## Now 168922 volume records
CITES_vol <- rbind(select(CITES_Vol_to_Vol, Id, Year, Appendix, Taxon, Order, Family, Genus, Term, Quantity, Unit, Importer, Exporter, Purpose, Source,
       Reporter.type, WildSource, PurposeC, Volume, Vol_Unit),
      select(CITES_Mass_to_Vol2, Id, Year, Appendix, Taxon, Order, Family, Genus, Term, Quantity, Unit, Importer, Exporter, Purpose, Source,
       Reporter.type, WildSource, PurposeC, Volume, Vol_Unit))


#### Reporter Type ####

CITES_Volplot <- CITES_vol %>% filter(Year %in% c(2000:2022)) %>% group_by(Year) %>% tally()
CITES_Volplot %>% group_by(Year) %>% tally() %>% 
  ggplot(aes(Year, n)) + geom_bar(stat = 'identity', width = 1, alpha = 0.5) +
  geom_bar(data = CITES_Volplot, stat = 'identity', width = 1, fill = "dodgerblue", alpha = 0.5) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  #annotate(x = 1980, y=110000, "text", label = "b.", size = 8) +
  xlab("Year") + ylab("Records") +
  theme_classic(base_size = 14) + 
  theme(axis.text = element_text(angle = 45, hjust = 1, vjust = -1))

## Importer reported has more records
CITES_vol %>% group_by(Reporter.type) %>% tally()

unique(CITES_vol$Taxon)



#### Original listing ####

## extract species in trade taxonomic information focus on Importer reported trade
## 53 species
(CITES_Species <- CITES_vol %>% filter(Reporter.type == "I", Year %in% c(1999:2020), !grepl("spp", Taxon), !Appendix == "N",
                                        WildSource == "Yes", PurposeC == "Commercial") %>%
   group_by(Taxon) %>% slice(1) %>% select(Order, Family, Genus, Taxon) %>% ungroup()) 

CITES_vol_Imp <- CITES_vol %>% filter(Reporter.type == "I", Year %in% c(1999:2020), !grepl("spp", Taxon), !Appendix == "N",
                                    WildSource == "Yes", PurposeC == "Commercial") 

unique(CITES_vol_Imp$Taxon)


## This may or may not be relevant to you - I needed this to make individual species time series e.g. if a species was traded yearly 2000 - 2010
## but not after 2010 I needed to add these years and fill the volumes with 0 (not traded - cites doesnt do this its only a record of trade that 
## happened). But the issue is what if the species was only cites listed 2000 - 2012, as cites only records trade in listed species so after 2012 
## trade wouldnt be reported to cites so those years shouldnt have zeros there should be no values.
## Thus this code uses the cites listings database to extract the time periods the species were listed for and add the actual trade and 0's to
## that.

## Read in the cites historic listings data
Historic_CITES <- data.table::fread("Data/Preparation/History_of_CITES_Listings_2021.csv") %>% 
  mutate(Year = format(as.Date(EffectiveAt, format="%d/%m/%Y"),"%Y"))

## Get the unique listings from the listing data/
## This tidies and gets the first year a species is CITES listed (the start of its possible time series)
First_listing <- Historic_CITES %>% group_by(Order, Family, Genus, FullName, Year, Appendix) %>% tally() %>%
  mutate(FullName = ifelse(FullName == Order, NA, FullName),
         FullName = ifelse(FullName == Family, NA, FullName),
         FullName = ifelse(FullName == Genus, NA, FullName)) %>%
  group_by(Order, Family, Genus, FullName) %>% slice_min(Year) %>% ungroup() %>% rename(Taxon = FullName) %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

s <- select(First_listing, Order) %>% distinct() %>% mutate(score = 1)

t <- left_join(select(CITES_Species, Order), s, by = "Order")

## First match all species level CITES listings
FL_SP <- First_listing %>% filter(!is.na(Taxon)) %>% select(Taxon, Year)

Sp_join <- left_join(CITES_Species, FL_SP, by = "Taxon")

sp_done <- Sp_join %>% filter(!is.na(Year)) ## 34 perfect matches

## Second match at genus level
genus_to_match <- Sp_join %>% filter(is.na(Year)) %>% select(-Year)
## get all the genus level appendix listings
FL_Genus <- First_listing %>% filter(is.na(Taxon), !is.na(Genus)) %>% select(Genus, Year) ## 217 listings
Genus_join <- left_join(genus_to_match, FL_Genus, by = "Genus")
Genus_done <- Genus_join %>% filter(!is.na(Year)) ## 17 perfect matches


## third match at family level
## get all the family level listings 52 listings
FL_Family <- First_listing %>% filter(is.na(Taxon), is.na(Genus), !is.na(Family)) %>% select(Family, Year)
Fam_to_match <- Genus_join %>% filter(is.na(Year)) %>% select(-Year)
Fam_join <- left_join(Fam_to_match, FL_Family, by = "Family")
Fam_done <- Fam_join %>% filter(!is.na(Year)) ## 2 perfect matches

## Fourth match at order level
FL_Order <- First_listing %>% filter(is.na(Taxon), is.na(Genus), is.na(Family), !is.na(Order)) %>% select(Order, Year)
Order_to_match <- Fam_join %>% filter(is.na(Year)) %>% select(-Year) ## 0 to still match
Order_join <- left_join(Order_to_match, FL_Order, by = "Order")

Order_done <- Order_join %>% filter(!is.na(Year)) ## 0 perfect matches

All_sp_fl <- rbind(sp_done,Genus_done,Fam_done, Order_done)

All_sp_fl %>% filter(Year >=1999)

#### Attach species FL to CITES db ####
FL_CITES_Species <- All_sp_fl %>% rename(FL_year = Year) %>% select(Taxon, FL_year) ## 53 sp

CITES_vol_Imp <- left_join(CITES_vol_Imp, FL_CITES_Species, by = "Taxon")
CITES_vol_Imp %>% filter(is.na(FL_year))


#### Remove CITES Deletion ####

## extract the true cites deletions
## We considered removing these, but that would bias the data against "success stories". Therefore we
## include them for the period in 2000 - 2018 that they were listed. Therefore a species could have a time series 2000 - 2007.

CITES_Deletions <- Historic_CITES %>% filter(ChangeType == "DELETION" & IsCurrent == "TRUE") %>%
  select(Year, FullName, ChangeType) %>% 
  rename(Taxon = FullName, Year_DEL = Year) %>%
  group_by(Taxon) %>% arrange(Taxon, Year_DEL) %>% slice_max(Year_DEL)

Del_sp <- CITES_Deletions$Taxon

CITES_vol_Imp %>% filter(Taxon %in% Del_sp) %>% summarise(n = (unique(Taxon))) ## 0 speices

## Add the deletions to the trade database as a separate year of deletion collumn.
## Later we will use this as an end point for all these series.
## All other species that had not been deleted will have full time series to 2018, therefore we can add the
## final year (2018) to the species without a deletion year.
CITES_vol_Imp <- left_join(CITES_vol_Imp, CITES_Deletions, by = "Taxon") %>%
  mutate(Year_DEL = ifelse(is.na(Year_DEL), 2020, Year_DEL))

write.csv(CITES_vol_Imp, "Outputs/Data_preparation/Timber/Imp_reported_vol.csv", na = "")

## Get all trades to volumes per species per year per Exporter and per importer
CITES_vol_Imp_Sum <- CITES_vol_Imp %>% 
  ## Exporter ONLY, In our study period, No aggregate of species
  filter(Reporter.type == "I", Year %in% c(1999:2020),WildSource == "Yes", PurposeC == "Commercial", !Appendix == "N") %>% 
  ## Group and tally per species/year
  group_by(Year, Taxon, Family, Order, FL_year, Year_DEL, Vol_Unit, Importer, Exporter) %>% 
  tally(Volume) %>%
  mutate(FL_year = as.numeric(FL_year), Year_DEL = as.numeric(Year_DEL))

## Create custom length time series for each species potential presence in trade
## the series begins when the species was listed, this prevent us marking species as absent from trade in years when 
## that species wasnt listed on the cites appendices
## We use the FL_year to start the series (when the species was first listed) and the Year_DEL column to end it.
## Thus creating a series corrected for each species first listing and/or its deletion from the appendices.
## Except for the complex cases which potentially move in and out and in (and out) again of the appendices.
Species_timeframe <- CITES_vol_Imp_Sum %>% group_by(Taxon, Family, Order, Vol_Unit, Importer, Exporter) %>% 
  summarise(Year = seq(from = min(FL_year), to = max(Year_DEL), length.out = max(Year_DEL) - min(FL_year) + 1))


## We automate the process but at this point we checked all species that our code produced as being listed then
## removed. We manually read the the historic cites listings for each species.
Manual_check <- Species_timeframe %>% group_by(Taxon) %>% filter(Year == max(Year)) %>% filter(Year != 2020)

unique(CITES_vol_Imp_Sum$Taxon)
## Expand the data set to fill missing years between 2000 - 2020
CITES_Imp_Timber_Vol <- left_join(Species_timeframe, CITES_vol_Imp_Sum) %>% select(-FL_year, -Year_DEL) %>%
  rename(Vol = n) %>% mutate(Volume = ifelse(is.na(Vol), 0, Vol)) %>%
  filter(Year > 1999)

ggplot(CITES_Imp_Timber_Vol, aes(Year, Volume)) + geom_point() + facet_wrap(~Taxon, scales = "free")

sum(CITES_Imp_Timber_Vol$Vol)

write.csv(CITES_Imp_Timber_Vol, "Outputs/Data_preparation/Timber/Processed_CITES_2022.csv", na = "")


##### Get IUCN data for timber species ######
library(rredlist)

## Read in the final processed sheet from the preparation script.
CITES_Imp_Timber_Vol <- data.table::fread("Outputs/Data_preparation/Timber/Processed_CITES_2022.csv", 
                                          na.strings = "")

## 53 back to 1999 for the lagged volumes
Sp_List <- CITES_Imp_Timber_Vol %>% group_by(Taxon) %>% tally() %>% select(Taxon) %>% as.data.frame()
CITES_Imp_Timber_Vol %>% filter(Year > 1999) %>% summarise(n_distinct(Taxon))

NA_update <- Sp_List %>% 
  mutate(IUCNName = case_when(Taxon == "Aloe ferox" ~ "Aloe ferox", ## Cape aloe not assessed
                              Taxon == "Bulnesia sarmientoi" ~"Gonopterodendron sarmientoi", ## Synonym
                              Taxon == "Dalbergia bariensis" ~"Dalbergia oliveri", ## Synonym 
                              Taxon == "Dipteryx panamensis" ~"Dipteryx oleifera", ## Not assessed
                              Taxon == "Gyrinops caudata" ~"Gyrinops caudata", ## Not assessed
                              Taxon == "Gyrinops ledermanii" ~"Gyrinops ledermanii", ## Not assessed
                              Taxon == "Gyrinops versteegii" ~"Dipteryx  oleifera", ## newly accepted
                              TRUE ~ Taxon))

## get key
## token can be generated here
## https://apiv3.iucnredlist.org/api/v3/token
apikey <- "a3fc116c122aefc621329055aeae8f67483e575c518acc14fcb77709bd94f6a2"

## Dummy data frame for the loops
df <- data.frame(IUCNName = character(),
                 Year = character(),
                 IUCN_code = character(),
                 IUCN_cat = character())

## Get species historical statuses. The loop calls the IUCN API to get all statuses for species.
## It is based on naming matches so if CITES uses a different name it will just return NA and have to be checked.
## Needs an API key and because of the delay needed between calls takes a few minutes to run.
## Do not remove or shorten the delay see rredlist guide for details.
## https://cran.r-project.org/web/packages/rredlist/rredlist.pdf

for(i in 1:nrow(NA_update)){ # would have used for(sp in speciesList) but need i for progress bar?
  ## incorporate 2s delay between each query
  Sys.sleep(2)
  ## Progress update
  cat('Species=',i, '\n')
  ## get historical data from website
  sp <- NA_update$IUCNName[i]
  iucnHistory <- rl_history(name=sp, key=apikey)
  # IF species cannot be found
  if (length(iucnHistory$result) == 0){ 
    spDf <- data.frame(IUCNName = sp,
                       Year = NA,
                       IUCN_code = NA,
                       IUCN_cat = NA)
    df <- rbind(df, spDf)
    # cat('Check ', sp, '\n')
  } else { 
    spdf <- data.frame(IUCNName = sp,
                       Year = iucnHistory$result$year,
                       IUCN_code = iucnHistory$result$code,
                       IUCN_cat = iucnHistory$result$category)
    df <- rbind(df, spdf)
  }
}

## 3 species after we checked names that have not been assessed.
df %>% filter(is.na(Year))

df_all <- left_join (df, NA_update)

write.csv(df_all, "Outputs/Data_preparation/Timber/Timber_IUCN_Assess_With_NA.csv", na = "")
df_all <- read.csv("Outputs/Data_preparation/Timber/Timber_IUCN_Assess_With_NA.csv", na = "")

#### Cleaning pre 2000 IUCN codes ####

## A variety of codes are used especially older version codes
unique(df_all$IUCN_code)
length(unique(df_all$Taxon)) ## 53


## Therefore remove these
Historic_IUCN <- df_all %>%
  ## remove I (interdeterminate) codes as all I species were assessed the same year again.
  ## Set Not assessed species as not assessed in 2000 otherwise they are also NA for Year and get removed later when they shouldnt
  mutate(Clean_code = case_when(IUCN_code == "LR/lc" ~ "LC",
                                IUCN_code == "LR/cd" ~ "NT",
                                IUCN_code == "LR/nt" ~ "NT",
                                IUCN_code == "V" ~ "VU",
                                IUCN_code == "nt" ~ "NT",
                                IUCN_code == "E" ~ "EN",
                                is.na(IUCN_code) ~ "NE", 
                                TRUE ~ IUCN_code)) %>%
  ungroup() %>%
  filter(Clean_code != "I") %>%
  select(IUCNName, Taxon, Year, Clean_code) %>% distinct()

length(unique(Historic_IUCN$Taxon)) ## 53
## Check species that are assessed multiple times in one year
Historic_IUCN %>% group_by(Taxon, Year) %>% filter(n() > 1)

## Many species assessed more than once in 1998, manually check date and clean
Historic_IUCN <- Historic_IUCN %>% filter(!(Taxon == "Cedrela odorata" & Year == 1998 & Clean_code == "NT"),
                                          !(Taxon == "Dalbergia latifolia" & Year == 1998 & Clean_code == "NT"),
                                          !(Taxon == "Guaiacum officinale" & Year == 1998 & Clean_code == "NT"),
                                          !(Taxon == "Guaiacum sanctum" & Year == 1998 & Clean_code == "NT"),
                                          !(Taxon == "Pericopsis elata" & Year == 1998 & Clean_code == "NT"),
                                          !(Taxon == "Swietenia humilis" & Year == 1998 & Clean_code == "NT"))

## No EX and EW species
Historic_IUCN %>% filter(Clean_code %in% c("EX", "EW"))


## Check removal and conversion left only the post 2001 framework
unique(Historic_IUCN$Clean_code)

## Backbone of values 2000 - 2020
backbone <- expand.grid(Year = as.integer(1975:2022), Taxon = unique(Historic_IUCN$Taxon))

## left join this and create your unrolled status
## Some species in trade before being IUCN assessed these are the NA values
Historic_IUCN$Year <- as.integer(Historic_IUCN$Year)
backbone$Year <- as.integer(backbone$Year)

## Here we add the backbone of species and dates to the IUCN data
df_new <- left_join(backbone, Historic_IUCN) %>%
  arrange(Taxon, Year) %>% group_by(Taxon) %>% 
  fill(Clean_code , .direction = "down") %>% 
  fill(IUCNName, .direction = "down") %>% 
  fill(IUCNName, .direction = "up") %>% 
  fill(Taxon, .direction = "down") %>% 
  fill(Taxon, .direction = "up") %>% 
  filter(Year %in% c(1999:2020)) %>% ungroup() %>%
  mutate(Year = as.numeric(Year)) %>% 
  select(Year, Taxon, Clean_code, IUCNName, Taxon) %>% 
  mutate(Clean_code = replace_na(Clean_code, "NE"),
         IUCNName = ifelse(is.na(IUCNName), Taxon, IUCNName))


length(unique(df_new$Taxon))

df_new <-df_new %>% mutate (Threat = case_when (Clean_code %in% c("CR", "EN", "VU") ~ "threatened",
                                               Clean_code%in% c("LC", "NT","NE")~ "not threatened"))

## Lots of species have assessments after our timeframe (e.g. 2021 and 2022) but not
#x in our timeframe (2000 - 2020)
CITES_IUCN_Timber <- left_join(CITES_Imp_Timber_Vol, df_new)


write.csv(CITES_IUCN_Timber, "Outputs/Data_preparation/Timber/CITES_IUCN_Timber_Countries.csv", na = "")

CITES_IUCN_Timber_Sum <- CITES_IUCN_Timber %>% group_by(Taxon, Family, Order, Vol_Unit,
                                                    Year,  Clean_code, IUCNName, Threat) %>%
  tally(Volume) %>% rename("Volume" = "n")

write.csv(CITES_IUCN_Timber_Sum, "Outputs/Data_preparation/Timber/CITES_IUCN_Timber.csv", na = "")

