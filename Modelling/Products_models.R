
library(brms)
library(bayesplot)
library(tidybayes)
library(ggpubr)
library(ggridges)
library(tidyverse)
library(bayestestR)
options(scipen = 999)

## Read in Data
CITES_IUCN_Products <- data.table::fread ("Outputs/Data_preparation/Products/CITES_IUCN_Products.csv",
                                         na.strings = "")%>% select(-V1)

## Standardise year and make a factor variable
CITES_IUCN_Products <- CITES_IUCN_Products %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                      FYear = as.factor(Year))

## 57.5% of records are zeroes
nrow(filter(CITES_IUCN_Products, mass == 0))/nrow(CITES_IUCN_Products)

## lots of zeroes and relatively low values with a small number of very high values
hist(CITES_IUCN_Products$mass)
max(CITES_IUCN_Products$mass)



HG_6 <- brm(bf(mass ~ Clean_code + Clean_code:SYear + SYear +
                        (SYear|Taxon) + (1|FYear),
                      hu ~ Clean_code + Clean_code:SYear + SYear +
                        ( SYear|Taxon)+ (1|FYear)),
                   family = hurdle_gamma(),
                   sample_prior = TRUE,
                   prior = c(
                     prior(normal(0,1), "b"),
                     prior(normal(0,1), "Intercept"),
                     prior(normal(0,1), "Intercept", dpar = "hu"),
                     prior(normal(0,1), "b", dpar = "hu"),
                     prior(normal(0,1), "sd", dpar = "hu"),
                     prior(normal(0,1), "sd")),
                   #control = list(adapt_delta = 0.9),
                   data = CITES_IUCN_Products,
                   file="Outputs/Models/Products/HG_6.rds",
                   chains = 4, iter = 1000, thin = 1, cores = 4, warmup = 500)

## Remember that out specifcation of (1 + SYear|Order/Taxon)+ (1|FYear) actually expands to 
## (1 + SYear|Order:Taxon) + (1 + SYear|Order) + (1|FYear). 
