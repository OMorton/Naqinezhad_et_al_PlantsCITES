library(brms)
library(bayesplot)
library(tidybayes)
library(ggpubr)
library(ggridges)
library(tidyverse)
library(bayestestR)
options(scipen = 999)

## Read in Data
CITES_IUCN_live<- data.table::fread ("Outputs/Data_preparation/Live/CITES_IUCN_Live.csv", na.strings = "")%>%
  select(-V1)


## Standardise year and make a factor variable
CITES_IUCN_live <- CITES_IUCN_live %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                              FYear = as.factor(Year))

## 57.5% of records are zeroes
nrow(filter(CITES_IUCN_live, n == 0))/nrow(CITES_IUCN_live)

## lots of zeroes and relatively low values with a small number of very high values
hist(CITES_IUCN_live$n)
max(CITES_IUCN_live$n)



###HG_7 based on new orders (12 orders) for the data###

HG_7 <- brm(bf(n ~ Clean_code + Clean_code:SYear + SYear +
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
            data = CITES_IUCN_live_2,
            file="Outputs/Models/Live/HG_7.rds",
            chains = 4, iter = 1000, thin = 1, cores = 4, warmup = 500)

summary (HG_7)
prior_summary(HG_7)
