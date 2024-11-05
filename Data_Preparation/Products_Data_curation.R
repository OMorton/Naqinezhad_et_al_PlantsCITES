#################################
##---CITES raw data curation for products plants---##
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
CITES_MASTER <- list.files(path="G:/My Drive/TUoS/Data_Sources/CITES/CITES_all_records_2022.1", full.names = TRUE, pattern="*.csv") %>% 
  lapply(read_csv, na = "", col_types = cols(Unit = col_character(), Import.permit.RandomID = col_character(),
                                    Export.permit.RandomID = col_character(), Origin.permit.RandomID = col_character(),
                                    Purpose = col_character())) %>% 
  bind_rows

###SOME SIMPLE INFORMATION

##CITES_Orchidaceae <- CITES %>% filter (Family == "Orchidaceae",  , Year > 1999)

##CITES_Orchidaceae <- CITES_Orchidaceae %>% filter(Term %in% c('live', "stems", "roots"), Unit %in% c(Unit=="", "Number of specimens")) %>% group_by (Term, Unit) %>% tally(Quantity) 

##write.csv(CITES_Orchidaceae, "CITES_Orchidaceae.csv")


##CITES_Cactaceae <- CITES %>% filter(Family == "Cactaceae", Year > 1999)

##write.csv(CITES_Cactaceae, "CITES_Cactaceae.csv")

##CITES_Galanthus <- CITES %>% filter(Genus == "Galanthus", Year > 1999)

##write.csv(CITES_Galanthus, "CITES_Galanthus.csv")


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

## 358,641 records
CITES_Wild_Com <- CITES_Plants %>% filter(WildSource == "Yes", PurposeC == "Commercial", Year %in% c(2000:2020))
write.csv(CITES_Wild_Com, "Outputs/Data_preparation/Timber/CITES_Wild_Com_new.csv", na = "")


CITES_Wild_Com <- CITES_Plants %>% filter(WildSource == "Yes", PurposeC == "Commercial", 
                                          Year %in% c(2000:2020), !grepl("Plantae", Taxon), 
                                          !grepl("spp", Taxon),!grepl("hybrid", Taxon), !Appendix == "N" )


## Importer reported has more records
CITES_Wild_Com %>% group_by(Reporter.type) %>% tally()

CITES_Wild_Com_Imp <- CITES_Wild_Com %>% filter(Reporter.type == "I", Year %in% c(1999:2020), !grepl("spp", Taxon), !grepl("Plantae", Taxon), !grepl("hybrid", Taxon), !Appendix == "N",
                                                WildSource == "Yes", PurposeC == "Commercial") 

## Focus on terms and units that could be converted to timber volumes

unique(CITES_Wild_Com_Imp$Term)
unique(CITES_Wild_Com_Imp$Unit)

##This part of the code is only for extracting different units available in the dataset. We first take out live and timber we already assessed in the previous stages
####################################################################################

## Get our product terms

CITES_products <- CITES_Wild_Com_Imp %>% filter(Term %in% c("powder", "oil", "carvings", "dried plants", "leaves", 
                                                        "derivatives", "extract", "stems", "roots", 
                                                        "medicine", "seeds", "flowers", "fibres", "bark", "wax", 
                                                        "fruit", "cosmetics",  "kernel") , 
                                            Unit %in% c("kg", "g", "mg"))


CITES_products <-CITES_products %>% mutate (Quantity_kg = case_when(Unit == "g" ~ Quantity/1000, Unit == "mg" ~ Quantity/1000000,Unit == "kg" ~ Quantity ))



CITES_products <-CITES_products %>% filter (Year > 1999,  !grepl('spp.', Taxon))




##Test <- CITES_products %>% group_by(Taxon) %>% tally(n_distinct(Type))

unique(CITES_products$Family) 
unique(CITES_products$Taxon)
unique (CITES_products$Unit)
unique (CITES_products$Term)
## 74 species in 24 families




#### Reporter Type ####

CITES_massplot <- CITES_products %>% filter(Year %in% c(2000:2022)) %>% group_by(Year) %>% tally()
CITES_massplot %>% group_by(Year) %>% tally() %>% 
  ggplot(aes(Year, n)) + geom_bar(stat = 'identity', width = 1, alpha = 0.5) +
  geom_bar(data = CITES_massplot, stat = 'identity', width = 1, fill = "dodgerblue", alpha = 0.5) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  #annotate(x = 1980, y=110000, "text", label = "b.", size = 8) +
  xlab("Year") + ylab("Records") +
  theme_classic(base_size = 14) + 
  theme(axis.text = element_text(angle = 45, hjust = 1, vjust = -1))

## Importer reported has more records
CITES_products %>% group_by(Reporter.type) %>% tally()

unique(CITES_products$Taxon)


##8234 records for importded trades and 74 species


#### Original listing ####

## extract species in trade taxonomic information focus on Importer reported trade
## 74 species
(CITES_Species <- CITES_products %>% filter(Reporter.type == "I", Year %in% c(1999:2020), !grepl("spp", Taxon), !Appendix == "N",
                                       WildSource == "Yes", PurposeC == "Commercial") %>%
   group_by(Taxon) %>% slice(1) %>% select(Order, Family, Genus, Taxon) %>% ungroup()) 

CITES_products_Imp <- CITES_products %>% filter(Reporter.type == "I", Year %in% c(1999:2020), !grepl("spp", Taxon), !Appendix == "N",
                                      WildSource == "Yes", PurposeC == "Commercial") 

unique(CITES_products_Imp$Taxon)
unique (CITES_products_Imp$Term)
unique (CITES_products_Imp$Family)


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
First_listing <- Historic_CITES %>% group_by(Order, Family, Genus, FullName, Year) %>% tally()%>%
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

sp_done <- Sp_join %>% filter(!is.na(Year)) ## 25 perfect matches out of 72 species of imported product species

## Second match at genus level
genus_to_match <- Sp_join %>% filter(is.na(Year)) %>% select(-Year)
## get all the genus level appendix listings
FL_Genus <- First_listing %>% filter(is.na(Taxon), !is.na(Genus)) %>% select(Genus, Year) ## 217 listings
Genus_join <- left_join(genus_to_match, FL_Genus, by = "Genus")
Genus_done <- Genus_join %>% filter(!is.na(Year)) ## 22 perfect matches out of 72 species


## third match at family level
## get all the family level listings 52 listings
FL_Family <- First_listing %>% filter(is.na(Taxon), is.na(Genus), !is.na(Family)) %>% select(Family, Year)
Fam_to_match <- Genus_join %>% filter(is.na(Year)) %>% select(-Year)
Fam_join <- left_join(Fam_to_match, FL_Family, by = "Family")
Fam_done <- Fam_join %>% filter(!is.na(Year)) ## 27 perfect matches

## Fourth match at order level
FL_Order <- First_listing %>% filter(is.na(Taxon), is.na(Genus), is.na(Family), !is.na(Order)) %>% select(Order, Year)
Order_to_match <- Fam_join %>% filter(is.na(Year)) %>% select(-Year) ## 0 to still match
Order_join <- left_join(Order_to_match, FL_Order, by = "Order")

Order_done <- Order_join %>% filter(!is.na(Year)) ## 0 perfect matches

All_sp_fl <- rbind(sp_done,Genus_done,Fam_done, Order_done)

All_sp_fl %>% filter(Year >=1999)

#### Attach species FL to CITES db ####
FL_CITES_Species <- All_sp_fl %>% rename(FL_year = Year) %>% select(Taxon, FL_year) ## 74 sp

CITES_products_Imp <- left_join(CITES_products_Imp, FL_CITES_Species, by = "Taxon")
CITES_products_Imp %>% filter(is.na(FL_year))


#### Remove CITES Deletion ####
## extract the true cites deletions
## We considered removing these, but that would bias the data against "success stories". Therefore we
## include them for the period in 2000 - 2020 that they were listed. Therefore a species could have a time series 2000 - 2007.

CITES_Deletions <- Historic_CITES %>% filter(ChangeType == "DELETION" & IsCurrent == "TRUE") %>%
  select(Year, FullName, ChangeType) %>% 
  rename(Taxon = FullName, Year_DEL = Year) %>%
  group_by(Taxon) %>% arrange(Taxon, Year_DEL) %>% slice_max(Year_DEL)

Del_sp <- CITES_Deletions$Taxon

CITES_products_Imp %>% filter(Taxon %in% Del_sp) %>% summarise(n = (unique(Taxon))) ## 1 speices: Cibotium barometz

## Add the deletions to the trade database as a separate year of deletion collumn.
## Later we will use this as an end point for all these series.
## All other species that had not been deleted will have full time series to 2018, therefore we can add the
## final year (2018) to the species without a deletion year.
CITES_products_Imp <- left_join(CITES_products_Imp, CITES_Deletions, by = "Taxon") %>%
  mutate(Year_DEL = ifelse(is.na(Year_DEL), 2020, Year_DEL))


write.csv(CITES_products_Imp, "Outputs/Data_preparation/Products/CITES_products_Imp.csv")

CITES_products_Imp  <- data.table::fread ("Outputs/Data_preparation/Products/CITES_products_Imp.csv")


## Get all trades to volumes per species per year per Exporter
CITES_products_Sum <- CITES_products_Imp %>% 
  ## Exporter ONLY, In our study period, No aggregate of species
  filter(Reporter.type == "I", Year %in% c(1999:2020)) %>% 
  ## Group and tally per species/year
  group_by(Year, Taxon, Family, Order, FL_year, Year_DEL, Importer, Exporter) %>% 
  tally(Quantity_kg) %>%
  mutate(FL_year = as.numeric(FL_year), Year_DEL = as.numeric(Year_DEL))


## Create custom length time series for each species potential presence in trade
## the series begins when the species was listed, this prevent us marking species as absent from trade in years when 
## that species wasnt listed on the cites appendices
## We use the FL_year to start the series (when the species was first listed) and the Year_DEL column to end it.
## Thus creating a series corrected for each species first listing and/or its deletion from the appendices.
## Except for the complex cases which potentially move in and out and in (and out) again of the appendices.
Species_timeframe <- CITES_products_Sum %>% group_by(Taxon, Family, Order, Importer, Exporter) %>% 
  summarise(Year = seq(from = min(FL_year), to = max(Year_DEL), length.out = max(Year_DEL) - min(FL_year) + 1))


## We automate the process but at this point we checked all species that our code produced as being listed then
## removed. We manually read the the historic cites listings for each species.
Manual_check <- Species_timeframe %>% group_by(Taxon) %>% filter(Year == max(Year)) %>% filter(Year != 2020)

unique(CITES_products_Sum$Taxon)

Species_timeframe <- rbind (filter(Species_timeframe, !Taxon %in% c("Cibotium barometz")),
                            expand.grid(Taxon = "Cibotium barometz", Family = "Dicksoniaceae", 
                                        Exporter = c("CN", "ID", "VN"),
                                        Order= "Dicksoniales" ,Year = c(2000:2020)))


## Expand the data set to fill missing years between 2000 - 2020
CITES_Imp_products <- left_join(Species_timeframe, CITES_products_Sum,
                                by = c('Taxon','Year',  'Family', 'Order', "Importer", 'Exporter')) %>% 
  select(-FL_year, -Year_DEL) %>%
  rename(mass = n) %>% mutate(mass = ifelse(is.na(mass), 0, mass)) %>%
  filter(Year > 1999)

ggplot(CITES_Imp_products, aes(Year, mass)) + geom_point() + facet_wrap(~Taxon, scales = "free")

sum(CITES_Imp_products $mass)

write.csv(CITES_Imp_products, "Outputs/Data_preparation/Products/CITES_Imp_products_CLEAN.csv", na = "")




#### To get IUCN status of each species in our dataset ####


## for all plotting and data handling (you might need to install it the first time)
library(rredlist)

## Read in the final processed sheet from the preparation script.
CITES_Imp_products <- data.table::fread("Outputs/Data_preparation/Products/CITES_Imp_products_CLEAN.csv",
                                    na.strings = "")


CITES_Imp_products <- CITES_Imp_products %>% filter(!grepl("hybrid", Taxon))
Sp_List <- CITES_Imp_products %>% group_by(Taxon) %>% tally() %>% select(Taxon) %>% as.data.frame()
CITES_Imp_products %>% filter(Year > 1999) %>% summarise(n_distinct(Taxon))


NA_update <- Sp_List %>% 
  mutate(IUCNName = case_when(
    Taxon == "Picrorhiza kurrooa" ~"Picrorhiza kurroa", ## Correcttion
    Taxon == "Sarracenia purpurea" ~"Sarracenia purpurea subsp. purpurea", ## Correcttion
    Taxon == "Opuntia cochenillifera" ~"Nopalea cochenillifera", ## Synonym
    Taxon == "Barlia robertiana" ~"Himantoglossum robertianum", ## Synonym
    
    TRUE ~ Taxon))

##################


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
    spDf <- data.frame (IUCNName = sp,
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

## 31 species after we checked names that have not been assessed.
df %>% filter(is.na(Year))

df_all <- left_join (df, NA_update)

write.csv(df_all, "Outputs/Data_preparation/Products/products_IUCN_Assess_With_NA.csv", na = "")
df_all <- read.csv ("Outputs/Data_preparation/Products/products_IUCN_Assess_With_NA.csv", na = "")

#### Cleaning pre 2000 IUCN codes ####

## A variety of codes are used especially older version codes
unique(df_all$IUCN_code)
length(unique(df_all$Taxon)) ## 74


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
                                IUCN_code == "R" ~ "NT", #Zamia manicata was R in 98,but NT consistently after.
                                is.na(IUCN_code) ~ "NE",
                                IUCN_code == "DD" ~ "NE",
                                TRUE ~ IUCN_code)) %>%
  mutate(Year = ifelse(IUCNName == "Adonis vernalis", 2011,Year),
         Year = ifelse(IUCNName == "Aniba rosaeodora", 2020,Year), 
         Year = ifelse(IUCNName == "Nardostachys grandiflora", 2020,Year),
         Year = ifelse(IUCNName == "Sarracenia purpurea subsp. purpurea", 2000,Year),
         Year = ifelse(IUCNName == "Himantoglossum robertianum", 2015,Year),
         Clean_code =ifelse(IUCNName == "Adonis vernalis", "LC", Clean_code),
         Clean_code = ifelse(IUCNName == "Aniba rosaeodora", "EN", Clean_code),
         Clean_code = ifelse(IUCNName == "Nardostachys grandiflora", "CR", Clean_code),
         Clean_code = ifelse(IUCNName == "Sarracenia purpurea subsp. purpurea", "LC", Clean_code),
         Clean_code = ifelse(IUCNName == "Himantoglossum robertianum", "LC", Clean_code)) %>%
  ungroup() %>%
  filter(Clean_code != "I") %>%  select(IUCNName, Taxon, Year, Clean_code) %>% distinct()


length(unique(Historic_IUCN$Taxon)) ## 74
## Check species that are assessed multiple times in one year
Historic_IUCN %>% group_by(Taxon, Year) %>% filter(n() > 1)

## Many species assessed more than once in 1998, manually check date abd clean
Historic_IUCN <- Historic_IUCN %>% filter(!(Taxon == "Marojejya darianii" & Year == 1998 & Clean_code == "CR"),
                                          !(Taxon == "Guaiacum sanctum" & Year == 1998 & Clean_code == "NT"))

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

## Lots of species have assessments after our timeframe (e.g. 2021 and 2022) but not
#x in our timeframe (2000 - 2020)

CITES_IUCN_Products_Countries <- left_join (CITES_Imp_products, df_new)
write.csv(CITES_IUCN_Products_Countries, "Outputs/Data_preparation/Products/CITES_IUCN_Products_Countries.csv", na = "")


CITES_IUCN_Products <- CITES_IUCN_Products_Countries %>% group_by(Taxon, Family, Order, Year, Clean_code, IUCNName) %>% tally(mass) %>%
  rename("mass" = "n")

write.csv(CITES_IUCN_Products, "Outputs/Data_preparation/Products/CITES_IUCN_Products.csv", na = "")
unique(CITES_IUCN_Products$Taxon)
