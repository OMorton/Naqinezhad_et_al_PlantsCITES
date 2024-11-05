library(brms)
library(bayesplot)
library(tidybayes)
library(ggpubr)
library(ggridges)
library(tidyverse)
library(bayestestR)
options(scipen = 999)

## Read in Data
CITES_IUCN_live_E<- data.table::fread ("Outputs/Data_preparation_E/Live/CITES_IUCN_Live_E.csv", na.strings = "")%>%
  select(-V1)


## Standardise year and make a factor variable
CITES_IUCN_live_E <- CITES_IUCN_live_E %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                              FYear = as.factor(Year))

## 57.5% of records are zeroes
nrow(filter(CITES_IUCN_live_E, n == 0))/nrow(CITES_IUCN_live_E)

## lots of zeroes and relatively low values with a small number of very high values
hist(CITES_IUCN_live_E$n)
max(CITES_IUCN_live_E$n)

####This part of the code is related to adjusting dataset according to 12 orders. We left join these 12 orders with the available dataset

live_orders <- data.table::fread ("Data/Preparation/live_orders.csv", na.strings = "")


CITES_IUCN_live_2_E <- merge (CITES_IUCN_live_E, live_orders, all.x = TRUE, by = "Family")

write.csv (CITES_IUCN_live_2_E, "Outputs/Data_preparation_E/Live/CITES_IUCN_Live_2_E.csv", na = "")

CITES_IUCN_live_2_E <- data.table::fread ("Outputs/Data_preparation_E/Live/CITES_IUCN_Live_2_E.csv", na.strings = "")



species3 <- distinct(CITES_IUCN_live_2_E , Taxon)

species3 <- distinct(CITES_IUCN_live_2_E , IUCNName)

##We just removed those families(or better to say orders) with less than 1000 records. THis could reduce the number of species from 775 to 765. So after this code the number of species gets to 765
CITES_IUCN_live_2_E$analysis <- if_else (CITES_IUCN_live_2_E$Family %in% c("Taxaceae", "Compositae", "Cucurbitaceae", "Vitaceae", "Podocarpaceae", "Pedaliaceae"), "No", "Yes")


CITES_IUCN_live_2_E <- CITES_IUCN_live_2_E %>% filter(analysis == "Yes")

write.csv (CITES_IUCN_live_2_E, "Outputs/Data_preparation_E/Live/CITES_IUCN_Live_2_E.csv", na = "")




unique(CITES_IUCN_live_2_E$order_new)

unique(CITES_IUCN_live_2_E$Taxon)

unique(CITES_IUCN_live_2_E$IUCNName)


ggplot(CITES_IUCN_live_2_E, aes(Year, n, colour = Clean_code)) +
  geom_point() + geom_smooth() +
  facet_wrap(~Clean_code, scales = "free") + theme_bw()


###HG_7 based on new orders (12 orders) for the data###

HG_7_E <- brm(bf(n ~ Clean_code + Clean_code:SYear + SYear +
                 (SYear|order_new/Taxon) + (1|FYear),
               hu ~ Clean_code + Clean_code:SYear + SYear +
                 ( SYear|order_new/Taxon)+ (1|FYear)),
            family = hurdle_gamma(),
            sample_prior = TRUE,
            prior = c(
              prior(normal(0,2), "b"),
              prior(normal(0,2), "Intercept"),
              prior(normal(0,2), "Intercept", dpar = "hu"),
              prior(normal(0,2), "b", dpar = "hu"),
              prior(normal(0,2), "sd", dpar = "hu"),
              prior(normal(0,2), "sd")),
            #control = list(adapt_delta = 0.9),
            data = CITES_IUCN_live_2_E,
            file="Outputs/Models_E/Live/HG_7_E.rds",
            chains = 4, iter = 1000, thin = 1, cores = 4, warmup = 500)

summary (HG_7_E)
prior_summary(HG_7_E)


