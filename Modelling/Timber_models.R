
###Building the model for timber###

###Originally we used very simple models (i.e. model HG_1 and HG_3, given down to this code) and then we added (1|FYear) and Taxon (HG_5). 
##So model HG_1 & HG_3 had some divergent transitions that need fixing and we used (1|FYear). 

## I've fixed the divergent transitions with this model (HG_5), basically I put some wider priors on the species and year varying intercepts (sd) to catch the massive variation
## and upped the adapt delta even more so the model takes smaller steps through parameter space. I would recomend using this model from now on 
## for timber.
library(brms)
library(bayesplot)
library(tidybayes)
library(ggpubr)
library(ggridges)
library(bayestestR)
library(tidyverse)

options(scipen = 999)

## Read in Data
CITES_IUCN_Timber<- data.table::fread ("Outputs/Data_preparation/Timber/CITES_IUCN_Timber.csv", na.strings = "")%>% select(-V1)

## Standardise year and make a factor variable
CITES_IUCN_Timber <- CITES_IUCN_Timber %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                  FYear = as.factor(Year))

## 57.5% of records are zeroes
nrow(filter(CITES_IUCN_Timber_Sum, Vol == 0))/nrow(CITES_IUCN_Timber_Sum)

## lots of zeroes and relatively low values with a small number of very high values
hist(CITES_IUCN_Timber$Vol)
max(CITES_IUCN_Timber$Vol)


HG_5 <- brm(bf(Vol ~ 1 + Clean_code + Clean_code:SYear + SYear +
                 (1 + SYear|Taxon) + (1|FYear),
               hu ~ 1 + Clean_code + Clean_code:SYear + SYear +
                 (1 + SYear|Taxon)+ (1|FYear)),
            family = hurdle_gamma(),
            sample_prior = TRUE,
            prior = c(
              prior(normal(0,2), "b"),
              prior(normal(0,3), "Intercept"),
              prior(normal(0,3), "Intercept", dpar = "hu"),
              prior(normal(0,2), "b", dpar = "hu"),
              prior(normal(0,6), "sd", dpar = "hu"),
              prior(normal(0,6), "sd")),
            control = list(adapt_delta = 0.9),
            data = CITES_IUCN_Timber,
            file="Outputs/Models/Timber/HG_5.rds",
            chains = 4, iter = 1000, thin = 1, cores = 4, warmup = 500)

summary (HG_5)
prior_summary(HG_5)
