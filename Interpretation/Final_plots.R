###This code is a summary of all codes for producing Figure 4 of the paper (Joint hurdle-distribution estimates of traded amount through time 
###for the average traded species based on importer-reported dataset)


library(brms) # Model fitting
library(tidyverse) # Data manipulation + plotting
library(tidybayes) # posterior manipulation
library(bayestestR) # posterior testing
library(ggpubr) # multipanel figure arranging
library(ggridges)
library(grid)
library(gridtext)

## Read the models of timber, live and products

HG_5 <- readRDS("Outputs/Models/Timber/HG_5.rds")
CITES_IUCN_Timber <- HG_5$data
CITES_IUCN_Timber<- data.table::fread ("Outputs/Data_preparation/Timber/CITES_IUCN_Timber.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_Timber <- CITES_IUCN_Timber %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                  FYear = as.factor(Year))

HG_7 <- readRDS("Outputs/Models/Live/HG_7.rds")

## Get the raw data - you can just use the actual raw data you have

CITES_IUCN_live<- HG_7$data

CITES_IUCN_live<- data.table::fread ("Outputs/Data_preparation/Live/CITES_IUCN_live.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_live <- CITES_IUCN_live %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                              FYear = as.factor(Year))

HG_6 <- readRDS("Outputs/Models/Products/HG_6.rds")
CITES_IUCN_Products<- data.table::fread ("Outputs/Data_preparation/Products/CITES_IUCN_Products.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_Products <- CITES_IUCN_Products %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                      FYear = as.factor(Year))


###Plot for full fit for timber, live and products

New_Timber <- CITES_IUCN_Timber %>% select(SYear, Year, Clean_code) %>% distinct()

Full_fit_T <- add_epred_draws(HG_5, newdata = New_Timber, re_formula = NA, summary = FALSE) %>%
  ## then group and summarise the posterior
  group_by(Year, Clean_code) %>%
  ## Extra step to flip the probs of 0 to the probs of 1
  median_hdci(.epred, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("CR", "EN", "VU", "NT", "LC", "NE" )))

Line_plot_T <- ggplot(Full_fit_T, aes(Year, .epred, colour = Clean_code)) +
  geom_line(size = 1) +  
  scale_color_manual(values = c("red", "orange3", "goldenrod2", "skyblue", "dodgerblue", "grey" )) +
  ylab("Volumes") + 
  theme_minimal() +
  theme(legend.position = 'none' , axis.title.x = element_blank()) + labs(tag = "A")


New_Live <- CITES_IUCN_live %>% select(SYear, Year, Clean_code, FYear) %>% distinct()

Full_fit_L <- add_epred_draws(HG_7, newdata = New_Live, re_formula = NA, summary = FALSE) %>%
  ## then group and summarise the posterior
  group_by(Year, Clean_code) %>%
  ## Extra step to flip the probs of 0 to the probs of 1
  median_hdci(.epred, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("CR", "EN", "VU", "NT", "LC", "NE" )))
Line_plot_L <- ggplot(Full_fit_L, aes(Year, .epred, colour = Clean_code)) +
  geom_line(size = 1) +  
  scale_color_manual(values = c("red", "orange3", "goldenrod2", "skyblue", "dodgerblue", "grey" )) +
  ylab("Number") + 
  theme_minimal() +
  theme(legend.position = 'none', axis.title.x = element_blank())+ labs(tag = "C")

New_Products <- CITES_IUCN_Products %>% select(Year,SYear, FYear,  Clean_code) %>% distinct()

Full_fit_P <- add_epred_draws(HG_6, newdata = New_Products, re_formula = NA, summary = FALSE) %>%
  ## then group and summarise the posterior
  group_by(Year, Clean_code) %>%
  ## Extra step to flip the probs of 0 to the probs of 1
  median_hdci(.epred, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("CR", "EN", "VU", "NT", "LC", "NE" )))
Line_plot_P <- ggplot(Full_fit_P, aes(Year, .epred, colour = Clean_code)) +
  geom_line(size = 1) +  
  scale_color_manual(values = c("red", "orange3", "goldenrod2", "skyblue", "dodgerblue", "grey" )) +
  ylab("Mass") + 
  theme_minimal() +
  theme(legend.position = 'none' , axis.title.x = element_blank())+ labs(tag = "E")


# Lay out plots

p = list(Line_plot_T,Line_plot_L, Line_plot_P) 


bottom <- textGrob("Year", gp = gpar(fontsize = 20, col="red"))


bottom = richtext_grob(text= "Year")

# Lay out plots
uni_lineplot <- grid.arrange(grobs=p, ncol = 3, nrow = 1, 
                             bottom = bottom)


#### Fixed effects for Timber ####
#### Mu ####
Mu_Fixefs_T <- fixef(HG_5, summary = FALSE) %>% as.data.frame() %>%
  select(!starts_with("hu_"))

Mu_BetaYrs_T <- Mu_Fixefs_T %>%
  # reference year trend for CR species
  summarise(Beta_CR = SYear,
            # reference + EN offset
            Beta_EN = SYear + `Clean_codeEN:SYear`,
            # reference + VU offset
            Beta_VU = SYear + `Clean_codeVU:SYear`,
            # reference + NT offset
            Beta_NT = SYear + `Clean_codeNT:SYear`,
            # reference + LC offset
            Beta_LC = SYear + `Clean_codeLC:SYear`,
            # reference + NE offset
            Beta_NE = SYear + `Clean_codeNE:SYear`) %>%
  # pivot the dataset from seperate columsn per status to long tidy format
  pivot_longer(cols = everything(), names_to = "IUCN", values_to = "Betas") %>%
  mutate(IUCN = factor(IUCN,
                       levels = c("Beta_NE", "Beta_LC", "Beta_NT",
                                  "Beta_VU", "Beta_EN", "Beta_CR")),
         Par = "Volume if traded")

## Sum
Mu_BetaYrs_Summary_T <- Mu_BetaYrs_T %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9) %>%
  left_join(Mu_BetaYrs_T %>% group_by(IUCN) %>%
              group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
  mutate(pd = pd*100, Par = "Volume if traded")


#### Hu ####
Hu_Fixefs_T <- fixef(HG_5, summary = FALSE) %>% as.data.frame() %>%
  select(starts_with("hu_"))

Hu_BetaYrs_T <- Hu_Fixefs_T %>% 
  # reference year trend for CR species
  summarise(Beta_CR = hu_SYear,
            # reference + EN offset
            Beta_EN = hu_SYear + `hu_Clean_codeEN:SYear`,
            # reference + VU offset
            Beta_VU = hu_SYear + `hu_Clean_codeVU:SYear`,
            # reference + NT offset
            Beta_NT = hu_SYear + `hu_Clean_codeNT:SYear`,
            # reference + LC offset
            Beta_LC = hu_SYear + `hu_Clean_codeLC:SYear`,
            # reference + NE offset
            Beta_NE = hu_SYear + `hu_Clean_codeNE:SYear`) %>%
  # pivot the dataset from seperate columsn per status to long tidy format
  pivot_longer(cols = everything(), names_to = "IUCN", values_to = "Betas") %>%
  mutate(IUCN = factor(IUCN, 
                       levels = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR")),
         ## This is the key bit about flipping the probabilty from probs of 0 to the probs of 1
         Betas = Betas*-1, Par = "Presence in trade")

Hu_BetaYrs_Summary_T <- Hu_BetaYrs_T %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9)%>%
  left_join(Hu_BetaYrs_T %>% group_by(IUCN) %>%
              group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
  mutate(pd = pd*100, Par = "Presence in trade")

#### PLotting ####
fixef_sum_T <- rbind(Mu_BetaYrs_Summary_T, Hu_BetaYrs_Summary_T)
fixef_sum_pos_T <- rbind(Mu_BetaYrs_Summary_T, Hu_BetaYrs_Summary_T) %>% filter(Betas > 0)
fixef_sum_neg_T <- rbind(Mu_BetaYrs_Summary_T, Hu_BetaYrs_Summary_T) %>% filter(Betas < 0)

fixef_raw_T <- rbind(Mu_BetaYrs_T, Hu_BetaYrs_T)

## So this has hu and mu next to each other and the text numbers show the percentage of the posterior with the same sign
## as the median its like a indication of how certain we are that there is a directional effect.
Fixef_plot_T <- ggplot(fixef_sum_T, aes(Betas, IUCN, fill = IUCN)) +
  facet_wrap(~Par, scales = "free_x") +
  geom_density_ridges(data = fixef_raw_T, alpha = .75, scale = .9, rel_min_height = 0.01) +
  geom_point(data = fixef_sum_T, aes(Betas, IUCN), size = 2) +
  geom_errorbarh(data = fixef_sum_T, aes(xmin = .lower, xmax = .upper),
                 height = 0, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = .75) +
  geom_text(data = fixef_sum_neg_T, aes(x =  .lower - 1.25, y = IUCN, label = pd), 
            nudge_y = .25,  size = 3) +
  geom_text(data = fixef_sum_pos_T, aes(x =  .upper + 1.25, y = IUCN, label = pd), 
            nudge_y = .25,  size = 3) +
  xlab("Possible parameter values") +
  scale_fill_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_y_discrete(limits = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR"),
                   labels = c("NE", "LC", "NT", "VU", "EN", "CR"))+
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none", strip.text = element_text(size = 9),
        axis.title.y = element_blank(), axis.title.x = element_blank())+ labs(tag = "B")


#### Fixed effects for live ####
#### Mu ####
Mu_Fixefs_L <- fixef(HG_7, summary = FALSE) %>% as.data.frame() %>%
  select(!starts_with("hu_"))

Mu_BetaYrs_L <- Mu_Fixefs_L %>%
  # reference year trend for CR species
  summarise(Beta_CR = SYear,
            # reference + EN offset
            Beta_EN = SYear + `Clean_codeEN:SYear`,
            # reference + VU offset
            Beta_VU = SYear + `Clean_codeVU:SYear`,
            # reference + NT offset
            Beta_NT = SYear + `Clean_codeNT:SYear`,
            # reference + LC offset
            Beta_LC = SYear + `Clean_codeLC:SYear`,
            # reference + NE offset
            Beta_NE = SYear + `Clean_codeNE:SYear`) %>%
  # pivot the dataset from seperate columsn per status to long tidy format
  pivot_longer(cols = everything(), names_to = "IUCN", values_to = "Betas") %>%
  mutate(IUCN = factor(IUCN,
                       levels = c("Beta_NE", "Beta_LC", "Beta_NT",
                                  "Beta_VU", "Beta_EN", "Beta_CR")),
         Par = "Quantity if traded")

## Sum
Mu_BetaYrs_Summary_L <- Mu_BetaYrs_L %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9) %>%
  left_join(Mu_BetaYrs_L %>% group_by(IUCN) %>%
              group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
  mutate(pd = pd*100, Par = "Quantity if traded")


#### Hu ####
Hu_Fixefs_L <- fixef(HG_7, summary = FALSE) %>% as.data.frame() %>%
  select(starts_with("hu_"))

Hu_BetaYrs_L <- Hu_Fixefs_L %>% 
  # reference year trend for CR species
  summarise(Beta_CR = hu_SYear,
            # reference + EN offset
            Beta_EN = hu_SYear + `hu_Clean_codeEN:SYear`,
            # reference + VU offset
            Beta_VU = hu_SYear + `hu_Clean_codeVU:SYear`,
            # reference + NT offset
            Beta_NT = hu_SYear + `hu_Clean_codeNT:SYear`,
            # reference + LC offset
            Beta_LC = hu_SYear + `hu_Clean_codeLC:SYear`,
            # reference + NE offset
            Beta_NE = hu_SYear + `hu_Clean_codeNE:SYear`) %>%
  # pivot the dataset from seperate columsn per status to long tidy format
  pivot_longer(cols = everything(), names_to = "IUCN", values_to = "Betas") %>%
  mutate(IUCN = factor(IUCN, 
                       levels = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR")),
         ## This is the key bit about flipping the probabilty from probs of 0 to the probs of 1
         Betas = Betas*-1, Par = "Presence in trade")

Hu_BetaYrs_Summary_L <- Hu_BetaYrs_L %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9)%>%
  left_join(Hu_BetaYrs_L %>% group_by(IUCN) %>%
              group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
  mutate(pd = pd*100, Par = "Presence in trade")

#### PLotting ####
fixef_sum_L <- rbind(Mu_BetaYrs_Summary_L, Hu_BetaYrs_Summary_L)
fixef_sum_pos_L <- rbind(Mu_BetaYrs_Summary_L, Hu_BetaYrs_Summary_L) %>% filter(Betas > 0)
fixef_sum_neg_L <- rbind(Mu_BetaYrs_Summary_L, Hu_BetaYrs_Summary_L) %>% filter(Betas < 0)

fixef_raw_L <- rbind(Mu_BetaYrs_L, Hu_BetaYrs_L)

## So this has hu and mu next to each other and the text numbers show the percentage of the posterior with the same sign
## as the median its like a indication of how certain we are that there is a directional effect.
Fixef_plot_L <- ggplot(fixef_sum_L, aes(Betas, IUCN, fill = IUCN)) +
  facet_wrap(~Par, scales = "free_x") +
  geom_density_ridges(data = fixef_raw_L, alpha = .75, scale = .9, rel_min_height = 0.01) +
  geom_point(data = fixef_sum_L, aes(Betas, IUCN), size = 2) +
  geom_errorbarh(data = fixef_sum_L, aes(xmin = .lower, xmax = .upper),
                 height = 0, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = .75) +
  geom_text(data = fixef_sum_neg_L, aes(x =  .lower - 1.25, y = IUCN, label = pd), 
            nudge_y = .25,  size = 3) +
  geom_text(data = fixef_sum_pos_L, aes(x =  .upper + 1.25, y = IUCN, label = pd), 
            nudge_y = .25,  size = 3) +
  xlab("Possible parameter values") +
  scale_fill_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_y_discrete(limits = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR"),
                   labels = c("NE", "LC", "NT", "VU", "EN", "CR"))+
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(size = 9),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank())+ labs(tag = "D")


#### Fixed effects for products ####
#### Mu ####
Mu_Fixefs_P <- fixef(HG_6, summary = FALSE) %>% as.data.frame() %>%
  select(!starts_with("hu_"))

Mu_BetaYrs_P <- Mu_Fixefs_P %>%
  # reference year trend for CR species
  summarise(Beta_CR = SYear,
            # reference + EN offset
            Beta_EN = SYear + `Clean_codeEN:SYear`,
            # reference + VU offset
            Beta_VU = SYear + `Clean_codeVU:SYear`,
            # reference + NT offset
            Beta_NT = SYear + `Clean_codeNT:SYear`,
            # reference + LC offset
            Beta_LC = SYear + `Clean_codeLC:SYear`,
            # reference + NE offset
            Beta_NE = SYear + `Clean_codeNE:SYear`) %>%
  # pivot the dataset from seperate columsn per status to long tidy format
  pivot_longer(cols = everything(), names_to = "IUCN", values_to = "Betas") %>%
  mutate(IUCN = factor(IUCN,
                       levels = c("Beta_NE", "Beta_LC", "Beta_NT",
                                  "Beta_VU", "Beta_EN", "Beta_CR")),
         Par = factor("Mass if traded"))

## Sum
Mu_BetaYrs_Summary_P <- Mu_BetaYrs_P %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9) %>%
  left_join(Mu_BetaYrs_P %>% group_by(IUCN) %>%
              group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
  mutate(pd = pd*100, Par = factor("Mass if traded"))


#### Hu ####
Hu_Fixefs_P <- fixef(HG_6, summary = FALSE) %>% as.data.frame() %>%
  select(starts_with("hu_"))

Hu_BetaYrs_P <- Hu_Fixefs_P %>% 
  # reference year trend for CR species
  summarise(Beta_CR = hu_SYear,
            # reference + EN offset
            Beta_EN = hu_SYear + `hu_Clean_codeEN:SYear`,
            # reference + VU offset
            Beta_VU = hu_SYear + `hu_Clean_codeVU:SYear`,
            # reference + NT offset
            Beta_NT = hu_SYear + `hu_Clean_codeNT:SYear`,
            # reference + LC offset
            Beta_LC = hu_SYear + `hu_Clean_codeLC:SYear`,
            # reference + NE offset
            Beta_NE = hu_SYear + `hu_Clean_codeNE:SYear`) %>%
  # pivot the dataset from seperate columsn per status to long tidy format
  pivot_longer(cols = everything(), names_to = "IUCN", values_to = "Betas") %>%
  mutate(IUCN = factor(IUCN, 
                       levels = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR")),
         ## This is the key bit about flipping the probabilty from probs of 0 to the probs of 1
         Betas = Betas*-1, Par = factor("Presence in trade"))

Hu_BetaYrs_Summary_P <- Hu_BetaYrs_P %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9)%>%
  left_join(Hu_BetaYrs_P %>% group_by(IUCN) %>%
              group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
  mutate(pd = pd*100, Par = factor("Presence in trade"))

#### PLotting ####
fixef_sum_P <- rbind( Hu_BetaYrs_Summary_P, Mu_BetaYrs_Summary_P )
levels(fixef_sum_P$Par)

fixef_sum_pos_P <- rbind(Hu_BetaYrs_Summary_P, Mu_BetaYrs_Summary_P ) %>% filter(Betas > 0)
levels(fixef_sum_pos_P$Par)
fixef_sum_neg_P <- rbind(Hu_BetaYrs_Summary_P , Mu_BetaYrs_Summary_P  ) %>% filter(Betas < 0)
levels(fixef_sum_neg_P$Par)

fixef_raw_P <- rbind(Hu_BetaYrs_P, Mu_BetaYrs_P )  

levels(fixef_raw_P$Par)

## So this has hu and mu next to each other and the text numbers show the percentage of the posterior with the same sign
## as the median its like a indication of how certain we are that there is a directional effect.

Fixef_plot_P <- ggplot(fixef_sum_P, aes(Betas, IUCN, fill = IUCN)) +
  facet_wrap(~Par, scales = "free_x") +
  geom_density_ridges(data = fixef_raw_P, alpha = .75, scale = .9, rel_min_height = 0.01) +
  geom_point(data = fixef_sum_P, aes(Betas, IUCN), size = 2) +
  geom_errorbarh(data = fixef_sum_P, aes(xmin = .lower, xmax = .upper),
                 height = 0, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = .75) +
  geom_text(data = fixef_sum_neg_P, aes(x =  .lower - 1.25, y = IUCN, label = pd), 
            nudge_y = .25, size = 3) +
  geom_text(data = fixef_sum_pos_P, aes(x =  .upper + 1.25, y = IUCN, label = pd), 
            nudge_y = .25,  size = 3) + 
  xlab("Possible parameter values") +
  scale_fill_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_y_discrete(limits = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR"),
                   labels = c("NE", "LC", "NT", "VU", "EN", "CR"))+
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(size = 9),
        axis.title.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank()) + labs(tag = "F")


p = list(Fixef_plot_T, Fixef_plot_L, Fixef_plot_P) 


bottom <- textGrob("Possible parameter values", gp = gpar(fontsize = 20, col="red"))


bottom = richtext_grob(text= "Possible parameter values")

# Lay out plots
uni_fixplots <- grid.arrange(grobs=p, ncol = 3, nrow = 1, 
                             bottom = bottom)


## Plot for Figure 4 of the MS

Figure_4 <- ggarrange(
            ggarrange(uni_lineplot,uni_fixplots, nrow = 2, ncol = 1))
         
  
ggsave(path = "Outputs/Figures", Figure_4, filename = "Figure_4.png",  bg = "white",
       device = "png", width = 25, height = 25, units = "cm")


####
####
####
#####Plots of exporter-reported dataset######


## Read the model

HG_5_E <- readRDS("Outputs/Models_E/Timber/HG_5_E.rds")


## Get the raw data - you can just use the actual raw data you have

CITES_IUCN_Timber_E <- HG_5_E$data


CITES_IUCN_Timber_E<- data.table::fread ("Outputs/Data_preparation_E/Timber/CITES_IUCN_Timber_E.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_Timber_E <- CITES_IUCN_Timber_E %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                      FYear = as.factor(Year))

CITES_IUCN_Timber_E_Sum <- rbind(CITES_IUCN_Timber_E %>% group_by(Year, Clean_code) %>% summarise(Vol = sum(Vol)),
                                 CITES_IUCN_Timber_E %>% group_by(Year) %>% summarise(Vol = sum(Vol)) %>% mutate(Clean_code = "Total"))

HG_7_E <- readRDS("Outputs/Models_E/Live/HG_7_E.rds")

## Get the raw data - you can just use the actual raw data you have

CITES_IUCN_live_2_E <- HG_7_E$data


CITES_IUCN_live_2_E<- data.table::fread ("Outputs/Data_preparation_E/Live/CITES_IUCN_live_2_E.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_live_2_E <- CITES_IUCN_live_2_E %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                      FYear = as.factor(Year))

CITES_IUCN_live_2_E_Sum <- rbind(CITES_IUCN_live_2_E %>% group_by(Year, Clean_code) %>% summarise(N = sum(n)),
                                 CITES_IUCN_live_2_E %>% group_by(Year) %>% summarise(N = sum(n)) %>% mutate(Clean_code = "Total"))

HG_6_E <- readRDS("Outputs/Models_E/Products/HG_6_E.rds")


CITES_IUCN_products_E<- data.table::fread ("Outputs/Data_preparation_E/Products/CITES_IUCN_Products_E.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_products_E <- CITES_IUCN_products_E %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                          FYear = as.factor(Year))


###Plot for full fit for timber, live and products

New_Timber_E <- CITES_IUCN_Timber_E %>% select(SYear, Year, Clean_code) %>% distinct()

Full_fit_TE <- add_epred_draws(HG_5_E, newdata = New_Timber_E, re_formula = NA, summary = FALSE) %>%
  ## then group and summarise the posterior
  group_by(Year, Clean_code) %>%
  ## Extra step to flip the probs of 0 to the probs of 1
  median_hdci(.epred, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("CR", "EN", "VU", "NT", "LC", "NE" )))

Line_plot_TE <- ggplot(Full_fit_TE, aes(Year, .epred, colour = Clean_code)) +
  geom_line(size = 1) +  
  scale_color_manual(values = c("red", "orange3", "goldenrod2", "skyblue", "dodgerblue", "grey" )) +
  ylab("Volumes") + 
  theme_minimal() +
  theme(legend.position = 'none' , axis.title.x = element_blank()) + labs(tag = "A")


New_Live_E <- CITES_IUCN_live_2 %>% select(SYear, Year, Clean_code, FYear) %>% distinct()

Full_fit_LE <- add_epred_draws(HG_7_E, newdata = New_Live_E, re_formula = NA, summary = FALSE) %>%
  ## then group and summarise the posterior
  group_by(Year, Clean_code) %>%
  ## Extra step to flip the probs of 0 to the probs of 1
  median_hdci(.epred, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("CR", "EN", "VU", "NT", "LC", "NE" )))
Line_plot_LE <- ggplot(Full_fit_LE, aes(Year, .epred, colour = Clean_code)) +
  geom_line(size = 1) +  
  scale_color_manual(values = c("red", "orange3", "goldenrod2", "skyblue", "dodgerblue", "grey" )) +
  ylab("Number") + 
  theme_minimal() +
  theme(legend.position = 'none', axis.title.x = element_blank())+ labs(tag = "C")

New_Products_E <- CITES_IUCN_products_E %>% select(Year,SYear, FYear,  Clean_code) %>% distinct()

Full_fit_PE <- add_epred_draws(HG_6_E, newdata = New_Products_E, re_formula = NA, summary = FALSE) %>%
  ## then group and summarise the posterior
  group_by(Year, Clean_code) %>%
  ## Extra step to flip the probs of 0 to the probs of 1
  median_hdci(.epred, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("CR", "EN", "VU", "NT", "LC", "NE" )))
Line_plot_PE <- ggplot(Full_fit_PE, aes(Year, .epred, colour = Clean_code)) +
  geom_line(size = 1) +  
  scale_color_manual(values = c("red", "orange3", "goldenrod2", "skyblue", "dodgerblue", "grey" )) +
  ylab("Mass") + 
  theme_minimal() +
  theme(legend.position = 'none' , axis.title.x = element_blank())+ labs(tag = "E")


# Lay out plots

p = list(Line_plot_TE,Line_plot_LE, Line_plot_PE) 



bottom <- textGrob("Year", gp = gpar(fontsize = 20, col="red"))


bottom = richtext_grob(text= "Year")

# Lay out plots
uni_lineplot_E <- grid.arrange(grobs=p, ncol = 3, nrow = 1, 
                               bottom = bottom)

#### Fixed effects for Timber ####
#### Mu ####
Mu_Fixefs_TE <- fixef(HG_5_E, summary = FALSE) %>% as.data.frame() %>%
  select(!starts_with("hu_"))

Mu_BetaYrs_TE <- Mu_Fixefs_TE %>%
  # reference year trend for CR species
  summarise(Beta_CR = SYear,
            # reference + EN offset
            Beta_EN = SYear + `Clean_codeEN:SYear`,
            # reference + VU offset
            Beta_VU = SYear + `Clean_codeVU:SYear`,
            # reference + NT offset
            Beta_NT = SYear + `Clean_codeNT:SYear`,
            # reference + LC offset
            Beta_LC = SYear + `Clean_codeLC:SYear`,
            # reference + NE offset
            Beta_NE = SYear + `Clean_codeNE:SYear`) %>%
  # pivot the dataset from seperate columsn per status to long tidy format
  pivot_longer(cols = everything(), names_to = "IUCN", values_to = "Betas") %>%
  mutate(IUCN = factor(IUCN,
                       levels = c("Beta_NE", "Beta_LC", "Beta_NT",
                                  "Beta_VU", "Beta_EN", "Beta_CR")),
         Par = "Volume if traded")

## Sum
Mu_BetaYrs_Summary_TE <- Mu_BetaYrs_TE %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9) %>%
  left_join(Mu_BetaYrs_TE %>% group_by(IUCN) %>%
              group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
  mutate(pd = pd*100, Par = "Volume if traded")


#### Hu ####
Hu_Fixefs_TE <- fixef(HG_5_E, summary = FALSE) %>% as.data.frame() %>%
  select(starts_with("hu_"))

Hu_BetaYrs_TE <- Hu_Fixefs_TE %>% 
  # reference year trend for CR species
  summarise(Beta_CR = hu_SYear,
            # reference + EN offset
            Beta_EN = hu_SYear + `hu_Clean_codeEN:SYear`,
            # reference + VU offset
            Beta_VU = hu_SYear + `hu_Clean_codeVU:SYear`,
            # reference + NT offset
            Beta_NT = hu_SYear + `hu_Clean_codeNT:SYear`,
            # reference + LC offset
            Beta_LC = hu_SYear + `hu_Clean_codeLC:SYear`,
            # reference + NE offset
            Beta_NE = hu_SYear + `hu_Clean_codeNE:SYear`) %>%
  # pivot the dataset from seperate columsn per status to long tidy format
  pivot_longer(cols = everything(), names_to = "IUCN", values_to = "Betas") %>%
  mutate(IUCN = factor(IUCN, 
                       levels = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR")),
         ## This is the key bit about flipping the probabilty from probs of 0 to the probs of 1
         Betas = Betas*-1, Par = "Presence in trade")

Hu_BetaYrs_Summary_TE <- Hu_BetaYrs_TE %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9)%>%
  left_join(Hu_BetaYrs_TE %>% group_by(IUCN) %>%
              group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
  mutate(pd = pd*100, Par = "Presence in trade")

#### PLotting ####
fixef_sum_TE <- rbind(Mu_BetaYrs_Summary_TE, Hu_BetaYrs_Summary_TE)
fixef_sum_pos_TE <- rbind(Mu_BetaYrs_Summary_TE, Hu_BetaYrs_Summary_TE) %>% filter(Betas > 0)
fixef_sum_neg_TE <- rbind(Mu_BetaYrs_Summary_TE, Hu_BetaYrs_Summary_TE) %>% filter(Betas < 0)

fixef_raw_TE <- rbind(Mu_BetaYrs_TE, Hu_BetaYrs_TE)

## So this has hu and mu next to each other and the text numbers show the percentage of the posterior with the same sign
## as the median its like a indication of how certain we are that there is a directional effect.
Fixef_plot_TE <- ggplot(fixef_sum_TE, aes(Betas, IUCN, fill = IUCN)) +
  facet_wrap(~Par, scales = "free_x") +
  geom_density_ridges(data = fixef_raw_TE, alpha = .75, scale = .9, rel_min_height = 0.01) +
  geom_point(data = fixef_sum_TE, aes(Betas, IUCN), size = 2) +
  geom_errorbarh(data = fixef_sum_TE, aes(xmin = .lower, xmax = .upper),
                 height = 0, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = .75) +
  geom_text(data = fixef_sum_neg_TE, aes(x =  .lower - 1.25, y = IUCN, label = pd), 
            nudge_y = .25,  size = 3) +
  geom_text(data = fixef_sum_pos_TE, aes(x =  .upper + 1.25, y = IUCN, label = pd), 
            nudge_y = .25,  size = 3) +
  xlab("Possible parameter values") +
  scale_fill_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_y_discrete(limits = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR"),
                   labels = c("NE", "LC", "NT", "VU", "EN", "CR"))+
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none", strip.text = element_text(size = 9),
        axis.title.y = element_blank(), axis.title.x = element_blank())+ labs(tag = "B")

#### Fixed effects for live ####
#### Mu ####
Mu_Fixefs_LE <- fixef(HG_7_E, summary = FALSE) %>% as.data.frame() %>%
  select(!starts_with("hu_"))

Mu_BetaYrs_LE <- Mu_Fixefs_LE %>%
  # reference year trend for CR species
  summarise(Beta_CR = SYear,
            # reference + EN offset
            Beta_EN = SYear + `Clean_codeEN:SYear`,
            # reference + VU offset
            Beta_VU = SYear + `Clean_codeVU:SYear`,
            # reference + NT offset
            Beta_NT = SYear + `Clean_codeNT:SYear`,
            # reference + LC offset
            Beta_LC = SYear + `Clean_codeLC:SYear`,
            # reference + NE offset
            Beta_NE = SYear + `Clean_codeNE:SYear`) %>%
  # pivot the dataset from seperate columsn per status to long tidy format
  pivot_longer(cols = everything(), names_to = "IUCN", values_to = "Betas") %>%
  mutate(IUCN = factor(IUCN,
                       levels = c("Beta_NE", "Beta_LC", "Beta_NT",
                                  "Beta_VU", "Beta_EN", "Beta_CR")),
         Par = "Quantity if traded")

## Sum
Mu_BetaYrs_Summary_LE <- Mu_BetaYrs_LE %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9) %>%
  left_join(Mu_BetaYrs_LE %>% group_by(IUCN) %>%
              group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
  mutate(pd = pd*100, Par = "Quantity if traded")


#### Hu ####
Hu_Fixefs_LE <- fixef(HG_7_E, summary = FALSE) %>% as.data.frame() %>%
  select(starts_with("hu_"))

Hu_BetaYrs_LE <- Hu_Fixefs_LE %>% 
  # reference year trend for CR species
  summarise(Beta_CR = hu_SYear,
            # reference + EN offset
            Beta_EN = hu_SYear + `hu_Clean_codeEN:SYear`,
            # reference + VU offset
            Beta_VU = hu_SYear + `hu_Clean_codeVU:SYear`,
            # reference + NT offset
            Beta_NT = hu_SYear + `hu_Clean_codeNT:SYear`,
            # reference + LC offset
            Beta_LC = hu_SYear + `hu_Clean_codeLC:SYear`,
            # reference + NE offset
            Beta_NE = hu_SYear + `hu_Clean_codeNE:SYear`) %>%
  # pivot the dataset from seperate columsn per status to long tidy format
  pivot_longer(cols = everything(), names_to = "IUCN", values_to = "Betas") %>%
  mutate(IUCN = factor(IUCN, 
                       levels = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR")),
         ## This is the key bit about flipping the probabilty from probs of 0 to the probs of 1
         Betas = Betas*-1, Par = "Presence in trade")

Hu_BetaYrs_Summary_LE <- Hu_BetaYrs_LE %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9)%>%
  left_join(Hu_BetaYrs_LE %>% group_by(IUCN) %>%
              group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
  mutate(pd = pd*100, Par = "Presence in trade")

#### PLotting ####
fixef_sum_LE <- rbind(Mu_BetaYrs_Summary_LE, Hu_BetaYrs_Summary_LE)
fixef_sum_pos_LE <- rbind(Mu_BetaYrs_Summary_LE, Hu_BetaYrs_Summary_LE) %>% filter(Betas > 0)
fixef_sum_neg_LE <- rbind(Mu_BetaYrs_Summary_LE, Hu_BetaYrs_Summary_LE) %>% filter(Betas < 0)

fixef_raw_LE <- rbind(Mu_BetaYrs_LE, Hu_BetaYrs_LE)

## So this has hu and mu next to each other and the text numbers show the percentage of the posterior with the same sign
## as the median its like a indication of how certain we are that there is a directional effect.
Fixef_plot_LE <- ggplot(fixef_sum_LE, aes(Betas, IUCN, fill = IUCN)) +
  facet_wrap(~Par, scales = "free_x") +
  geom_density_ridges(data = fixef_raw_LE, alpha = .75, scale = .9, rel_min_height = 0.01) +
  geom_point(data = fixef_sum_LE, aes(Betas, IUCN), size = 2) +
  geom_errorbarh(data = fixef_sum_LE, aes(xmin = .lower, xmax = .upper),
                 height = 0, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = .75) +
  geom_text(data = fixef_sum_neg_LE, aes(x =  .lower - 1.25, y = IUCN, label = pd), 
            nudge_y = .25,  size = 3) +
  geom_text(data = fixef_sum_pos_LE, aes(x =  .upper + 1.25, y = IUCN, label = pd), 
            nudge_y = .25,  size = 3) +
  xlab("Possible parameter values") +
  scale_fill_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_y_discrete(limits = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR"),
                   labels = c("NE", "LC", "NT", "VU", "EN", "CR"))+
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(size = 9),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank())+ labs(tag = "D")


#### Fixed effects for products ####
#### Mu ####
Mu_Fixefs_PE <- fixef(HG_6_E, summary = FALSE) %>% as.data.frame() %>%
  select(!starts_with("hu_"))

Mu_BetaYrs_PE <- Mu_Fixefs_PE %>%
  # reference year trend for CR species
  summarise(Beta_CR = SYear,
            # reference + EN offset
            Beta_EN = SYear + `Clean_codeEN:SYear`,
            # reference + VU offset
            Beta_VU = SYear + `Clean_codeVU:SYear`,
            # reference + NT offset
            Beta_NT = SYear + `Clean_codeNT:SYear`,
            # reference + LC offset
            Beta_LC = SYear + `Clean_codeLC:SYear`,
            # reference + NE offset
            Beta_NE = SYear + `Clean_codeNE:SYear`) %>%
  # pivot the dataset from seperate columsn per status to long tidy format
  pivot_longer(cols = everything(), names_to = "IUCN", values_to = "Betas") %>%
  mutate(IUCN = factor(IUCN,
                       levels = c("Beta_NE", "Beta_LC", "Beta_NT",
                                  "Beta_VU", "Beta_EN", "Beta_CR")),
         Par = factor("Mass if traded"))

## Sum
Mu_BetaYrs_Summary_PE <- Mu_BetaYrs_PE %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9) %>%
  left_join(Mu_BetaYrs_PE %>% group_by(IUCN) %>%
              group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
  mutate(pd = pd*100, Par = factor("Mass if traded"))


#### Hu ####
Hu_Fixefs_PE <- fixef(HG_6_E, summary = FALSE) %>% as.data.frame() %>%
  select(starts_with("hu_"))

Hu_BetaYrs_PE <- Hu_Fixefs_PE %>% 
  # reference year trend for CR species
  summarise(Beta_CR = hu_SYear,
            # reference + EN offset
            Beta_EN = hu_SYear + `hu_Clean_codeEN:SYear`,
            # reference + VU offset
            Beta_VU = hu_SYear + `hu_Clean_codeVU:SYear`,
            # reference + NT offset
            Beta_NT = hu_SYear + `hu_Clean_codeNT:SYear`,
            # reference + LC offset
            Beta_LC = hu_SYear + `hu_Clean_codeLC:SYear`,
            # reference + NE offset
            Beta_NE = hu_SYear + `hu_Clean_codeNE:SYear`) %>%
  # pivot the dataset from seperate columsn per status to long tidy format
  pivot_longer(cols = everything(), names_to = "IUCN", values_to = "Betas") %>%
  mutate(IUCN = factor(IUCN, 
                       levels = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR")),
         ## This is the key bit about flipping the probabilty from probs of 0 to the probs of 1
         Betas = Betas*-1, Par = factor("Presence in trade"))

Hu_BetaYrs_Summary_PE <- Hu_BetaYrs_PE %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9)%>%
  left_join(Hu_BetaYrs_PE %>% group_by(IUCN) %>%
              group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
  mutate(pd = pd*100, Par = factor("Presence in trade"))

#### PLotting ####
fixef_sum_PE <- rbind( Hu_BetaYrs_Summary_PE, Mu_BetaYrs_Summary_PE )
levels(fixef_sum_PE$Par)

fixef_sum_pos_PE <- rbind(Hu_BetaYrs_Summary_PE, Mu_BetaYrs_Summary_PE ) %>% filter(Betas > 0)
levels(fixef_sum_pos_PE$Par)
fixef_sum_neg_PE <- rbind(Hu_BetaYrs_Summary_PE , Mu_BetaYrs_Summary_PE  ) %>% filter(Betas < 0)
levels(fixef_sum_neg_PE$Par)

fixef_raw_PE <- rbind(Hu_BetaYrs_PE, Mu_BetaYrs_PE )  

levels(fixef_raw_PE$Par)

## So this has hu and mu next to each other and the text numbers show the percentage of the posterior with the same sign
## as the median its like a indication of how certain we are that there is a directional effect.

Fixef_plot_PE <- ggplot(fixef_sum_PE, aes(Betas, IUCN, fill = IUCN)) +
  facet_wrap(~Par, scales = "free_x") +
  geom_density_ridges(data = fixef_raw_PE, alpha = .75, scale = .9, rel_min_height = 0.01) +
  geom_point(data = fixef_sum_PE, aes(Betas, IUCN), size = 2) +
  geom_errorbarh(data = fixef_sum_PE, aes(xmin = .lower, xmax = .upper),
                 height = 0, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = .75) +
  geom_text(data = fixef_sum_neg_PE, aes(x =  .lower - 1.25, y = IUCN, label = pd), 
            nudge_y = .25, size = 3) +
  geom_text(data = fixef_sum_pos_PE, aes(x =  .upper + 1.25, y = IUCN, label = pd), 
            nudge_y = .25,  size = 3) + 
  xlab("Possible parameter values") +
  scale_fill_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_y_discrete(limits = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR"),
                   labels = c("NE", "LC", "NT", "VU", "EN", "CR"))+
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(size = 9),
        axis.title.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank()) + labs(tag = "F")


p = list(Fixef_plot_TE, Fixef_plot_LE, Fixef_plot_PE) 


bottom <- textGrob("Possible parameter values", gp = gpar(fontsize = 20, col="red"))


bottom = richtext_grob(text= "Possible parameter values")

# Lay out plots
uni_fixplots_E <- grid.arrange(grobs=p, ncol = 3, nrow = 1, 
                               bottom = bottom)


## Plot for Figure S7 of the MS

Figure_S7 <- ggarrange(
  ggarrange(uni_lineplot_E,uni_fixplots_E, nrow = 2, ncol = 1))


ggsave(path = "Outputs/Figures", Figure_S7, filename = "Figure_S7.png",  bg = "white",
       device = "png", width = 25, height = 25, units = "cm")



###############
#############
############
###Species-level coefs 

#### Species-level coefs of timber #### Showing only species traded upper than 100 m3 volume

new_dat_timber <- CITES_IUCN_Timber %>% group_by(Taxon) %>% mutate(Tot_Vol = sum(Volume)) %>%
  group_by(Taxon, Tot_Vol) %>% filter(SYear == max(SYear)) %>% 
  group_by(Taxon, Clean_code, Tot_Vol) %>% tally() %>% mutate(SYear = 1)

Species_int2_timber <- add_linpred_draws(HG_5, newdata = new_dat_timber, re_formula = ~ (1 + SYear|Taxon), dpar = "hu")
Species_to_remove2_timber <- add_linpred_draws(HG_5, newdata = mutate(new_dat_timber, SYear = 0), re_formula = ~ (1 + SYear|Taxon), dpar = "hu")

Species_int_mu2_timber <- add_linpred_draws(HG_5, newdata = new_dat_timber, re_formula = ~ (1 + SYear|Taxon), dpar = "mu")
Species_to_remove_mu2_timber <- add_linpred_draws(HG_5, newdata = mutate(new_dat_timber, SYear = 0), re_formula = ~ (1 + SYear|Taxon), dpar = "mu")

## Get the beta trend per code per order_new
Species_IUCN_Beta_timber <- cbind(select(Species_int2_timber, Taxon) ,data.frame(hu = Species_int2_timber$hu - Species_to_remove2_timber$hu)) %>% 
  mutate(hu = hu*-1) %>% group_by(Taxon, Clean_code, Tot_Vol) %>% 
  mutate(pd = (sum(sign(hu) == sign(median(hu)))/n()*100)) %>%
  group_by(Taxon, Clean_code, Tot_Vol, pd) %>% 
  median_hdci(hu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "hu") %>% mutate(dpar = "Presence in trade")

Species_IUCN_Beta_mu_timber <- cbind(select(Species_int_mu2_timber, Taxon) ,data.frame(mu = Species_int_mu2_timber$mu - Species_to_remove_mu2_timber$mu)) %>% 
  group_by(Taxon, Clean_code, Tot_Vol) %>% 
  mutate(pd = (sum(sign(mu) == sign(median(mu)))/n()*100)) %>%
  group_by(Taxon, Clean_code, Tot_Vol, pd) %>% 
  median_hdci(mu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "mu") %>% mutate(dpar = "Quantity if traded")

Plot_order_timber <- Species_IUCN_Beta_timber %>% arrange(Clean_code, Est)  %>% mutate(Plotting_Order = 1:n()) %>%
  select(Taxon,  Plotting_Order)

Species_coefs_timber_full <- rbind(Species_IUCN_Beta_timber, Species_IUCN_Beta_mu_timber) %>% 
  left_join(Plot_order_timber) %>%
  mutate(Dir = ifelse(pd >= 97.5, "Yes", "No")) 
Species_coefs_timber <- Species_coefs_timber_full %>%
  filter(Tot_Vol >= 100 )

n_distinct(Species_coefs_timber$Taxon) ## 32



#### Species-level coefs of live ####Showing only species traded upper than 100 individuals
new_dat2 <- CITES_IUCN_live %>% group_by(Taxon) %>% mutate(Tot_Vol = sum(n)) %>%
  group_by(Taxon, order_new, Tot_Vol) %>% filter(SYear == max(SYear)) %>% 
  group_by(Taxon, order_new, Clean_code, Tot_Vol) %>% tally() %>% mutate(SYear = 1)

Species_int2 <- add_linpred_draws(HG_7, newdata = new_dat2, re_formula = ~ (1 + SYear|order_new/Taxon), dpar = "hu")
Species_to_remove2 <- add_linpred_draws(HG_7, newdata = mutate(new_dat2, SYear = 0), re_formula = ~ (1 + SYear|order_new/Taxon), dpar = "hu")

Species_int_mu2 <- add_linpred_draws(HG_7, newdata = new_dat2, re_formula = ~ (1 + SYear|order_new/Taxon), dpar = "mu")
Species_to_remove_mu2 <- add_linpred_draws(HG_7, newdata = mutate(new_dat2, SYear = 0), re_formula = ~ (1 + SYear|order_new/Taxon), dpar = "mu")

## Get the beta trend per code per order_new
Species_IUCN_Beta <- cbind(select(Species_int2, order_new, Taxon) ,data.frame(hu = Species_int2$hu - Species_to_remove2$hu)) %>% 
  mutate(hu = hu*-1) %>% group_by(Taxon, order_new, Clean_code, Tot_Vol) %>% 
  mutate(pd = (sum(sign(hu) == sign(median(hu)))/n()*100)) %>%
  group_by(Taxon, order_new, Clean_code, Tot_Vol, pd) %>% 
  median_hdci(hu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "hu") %>% mutate(dpar = "Presence in trade")

Species_IUCN_Beta_mu <- cbind(select(Species_int_mu2, order_new, Taxon) ,data.frame(mu = Species_int_mu2$mu - Species_to_remove_mu2$mu)) %>% 
  group_by(Taxon, order_new, Clean_code, Tot_Vol) %>% 
  mutate(pd = (sum(sign(mu) == sign(median(mu)))/n()*100)) %>%
  group_by(Taxon, order_new, Clean_code, Tot_Vol, pd) %>% 
  median_hdci(mu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "mu") %>% mutate(dpar = "Quantity if traded")

Plot_order <- Species_IUCN_Beta %>% arrange(Clean_code, Est)  %>% mutate(Plotting_Order = 1:n()) %>%
  select(Taxon, order_new, Plotting_Order)

Species_coefs_full <- rbind(Species_IUCN_Beta, Species_IUCN_Beta_mu) %>% 
  left_join(Plot_order) %>%
  mutate(Dir = ifelse(pd >= 97.5, "Yes", "No")) 
Species_coefs <- Species_coefs_full %>%
  filter(Tot_Vol >= 100 )

n_distinct(Species_coefs$Taxon) ## 269



#### Species level product ####Showing only species traded upper than 100 kg mass
new_dat3 <- CITES_IUCN_products %>% group_by(Taxon) %>% mutate(Tot_mass = sum(mass)) %>%
  group_by(Taxon, Tot_mass) %>% filter(SYear == max(SYear)) %>% 
  group_by(Taxon, Clean_code, Tot_mass) %>% tally() %>% mutate(SYear = 1)

Species_int2_prod <- add_linpred_draws(HG_6, newdata = new_dat3, re_formula = ~ (1 + SYear|Taxon), dpar = "hu")
Species_to_remove2_prod <- add_linpred_draws(HG_6, newdata = mutate(new_dat3, SYear = 0), re_formula = ~ (1 + SYear|Taxon), dpar = "hu")

Species_int_mu2_prod <- add_linpred_draws(HG_6, newdata = new_dat3, re_formula = ~ (1 + SYear|Taxon), dpar = "mu")
Species_to_remove_mu2_prod <- add_linpred_draws(HG_6, newdata = mutate(new_dat3, SYear = 0), re_formula = ~ (1 + SYear|Taxon), dpar = "mu")

## Get the beta trend per code per order_new
Species_IUCN_Beta_prod <- cbind(select(Species_int2_prod, Taxon) ,data.frame(hu = Species_int2_prod$hu - Species_to_remove2_prod$hu)) %>% 
  mutate(hu = hu*-1) %>% group_by(Taxon, Clean_code, Tot_mass) %>% 
  mutate(pd = (sum(sign(hu) == sign(median(hu)))/n()*100)) %>%
  group_by(Taxon, Clean_code, Tot_mass, pd) %>% 
  median_hdci(hu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "hu") %>% mutate(dpar = "Presence in trade")

Species_IUCN_Beta_mu_prod <- cbind(select(Species_int_mu2_prod, Taxon) ,data.frame(mu = Species_int_mu2_prod$mu - Species_to_remove_mu2_prod$mu)) %>% 
  group_by(Taxon, Clean_code, Tot_mass) %>% 
  mutate(pd = (sum(sign(mu) == sign(median(mu)))/n()*100)) %>%
  group_by(Taxon, Clean_code, Tot_mass, pd) %>% 
  median_hdci(mu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "mu") %>% mutate(dpar = "Quantity if traded")

Plot_order_prod <- Species_IUCN_Beta_prod %>% arrange(Clean_code, Est)  %>% mutate(Plotting_Order = 1:n()) %>%
  select(Taxon, Plotting_Order)

Species_coefs_prod_full <- rbind(Species_IUCN_Beta_prod, Species_IUCN_Beta_mu_prod) %>% 
  left_join(Plot_order_prod) %>% 
  mutate(Dir = ifelse(pd >= 97.5, "Yes", "No")) 
Species_coefs_prod <- Species_coefs_prod_full %>%
  filter(Tot_mass >= 100)

n_distinct(Species_coefs_prod$Taxon) ## 55



#### Order level live coefs ####
#### Order level ####
new_dat <- CITES_IUCN_live_2 %>% group_by(order_new, Clean_code) %>% tally() %>% mutate(SYear = 1)

Order_int <- add_linpred_draws(HG_7, newdata = new_dat, re_formula = ~ (1 + SYear|order_new), dpar = "hu")
Order_to_remove <- add_linpred_draws(HG_7, newdata = mutate(new_dat, SYear = 0), re_formula = ~ (1 + SYear|order_new), dpar = "hu")

Order_int_mu <- add_linpred_draws(HG_7, newdata = new_dat, re_formula = ~ (1 + SYear|order_new), dpar = "mu")
Order_to_remove_mu <- add_linpred_draws(HG_7, newdata = mutate(new_dat, SYear = 0), re_formula = ~ (1 + SYear|order_new), dpar = "mu")

## Get the beta trend per code per order_new
Order_IUCN_Beta <- cbind(select(Order_int, order_new) ,data.frame(hu = Order_int$hu - Order_to_remove$hu)) %>% 
  mutate(hu = hu*-1) %>% group_by(order_new, Clean_code) %>% 
  mutate(pd = (sum(sign(hu) == sign(median(hu)))/n()*100)) %>%
  group_by(order_new, Clean_code, pd) %>%
  median_hdci(hu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "hu") %>% mutate(dpar = "Presence in trade")

Order_IUCN_Beta_mu <- cbind(select(Order_int_mu, order_new) ,data.frame(mu = Order_int_mu$mu - Order_to_remove_mu$mu)) %>% 
  group_by(order_new, Clean_code) %>% 
  mutate(pd = (sum(sign(mu) == sign(median(mu)))/n()*100)) %>%
  group_by(order_new, Clean_code, pd) %>%
  median_hdci(mu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "mu") %>% mutate(dpar = "Quantity if traded")

Order_coefs <- rbind(Order_IUCN_Beta, Order_IUCN_Beta_mu) %>%  mutate(Dir = ifelse(pd >= 97.5, "Yes", "No"))

#### Data write out ####

## live
write.csv(Species_coefs_full, "Outputs/Summaries/Species_Live_Coef_Summary_FULL.csv")
write.csv(Species_coefs, "Outputs/Summaries/Species_Live_Coef_Summary_morethan100vol.csv")

## live order
write.csv(Order_coefs, "Outputs/Summaries/Order_Live_Coef_Summary_FULL.csv")

## timber
write.csv(Species_coefs_timber_full, "Outputs/Summaries/Species_Timber_Coef_Summary_FULL.csv")
write.csv(Species_coefs_timber, "Outputs/Summaries/Species_Timber_Coef_Summary_morethan100vol.csv")

## live
write.csv(Species_coefs_prod_full, "Outputs/Summaries/Species_Product_Coef_Summary_FULL.csv")
write.csv(Species_coefs_prod, "Outputs/Summaries/Species_Product_Coef_Summary_morethan100vol.csv")

#### Volume x Beta Arrangement NEW STYLE ####

## Timber
Timber_plt <- ggplot(Species_coefs_timber, aes(Tot_Vol, Est, colour = Clean_code, fill = Dir)) +
  facet_wrap(~dpar, scales = "free_y") +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0, alpha = .75) +
  geom_point(alpha = 1, shape = 21) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  scale_x_log10()+
  geom_text(data = data.frame(Tot_Vol = 1000000, Est = 2, dpar = "Quantity if traded",
                              Clean_code = "CR", Dir = "Yes"),label = "n = 32", colour = "black") +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_fill_manual(values = c("white", "black")) +
  xlab( expression(paste("Traded volume ", (m^3)))) +
  ylab("Coeficient") +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

## Live
Live_plt <- ggplot(Species_coefs, aes(Tot_Vol, Est, colour = Clean_code, fill = Dir)) +
  facet_wrap(~dpar, scales = "free_y") +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0, alpha = .75) +
  geom_point(alpha = 1, shape = 21) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  geom_text(data = data.frame(Tot_Vol = 10000000, Est = 2, dpar = "Quantity if traded",
                              Clean_code = "CR", Dir = "Yes"),label = "n = 269", colour = "black") +
  scale_x_log10()+
  #coord_cartesian(xlim = c(1000, 1000000000)) +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_fill_manual(values = c("white", "black")) +
  xlab("Traded individuals") +
  ylab("Coeficient") +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

## Product
Product_plt <- ggplot(Species_coefs_prod, aes(Tot_mass, Est, colour = Clean_code, fill = Dir)) +
  facet_wrap(~dpar, scales = "free_y") +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0, alpha = .75) +
  geom_point(alpha = 1, shape = 21) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  geom_text(data = data.frame(Tot_mass = 10000000, Est = 1.5, dpar = "Quantity if traded",
                              Clean_code = "CR", Dir = "Yes"),label = "n = 55", colour = "black") +
  scale_x_log10()+
  #coord_cartesian(xlim = c(1000, 1000000000)) +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_fill_manual(values = c("white", "black")) +
  xlab("Traded mass (g)") +
  ylab("Coeficient") +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))


library(ggpubr)

Vol_sp_arrangement <- ggarrange(ggarrange(Timber_plt, Product_plt, ncol = 2, labels = c("A.", "B.")),
                                Live_plt, nrow = 2, labels = c("", "C."))
Figure_5 <-Vol_sp_arrangement

ggsave(path = "Outputs/Figures", Figure_5, filename = "Figure_5.png",  bg = "white",
       device = "png", width = 30, height = 20, units = "cm")



#### Beta only plots OLD STYLE ####

Species_coefs_live_plt <- ggplot(Species_coefs_full, aes(Est, reorder(Taxon, Plotting_Order), colour = Clean_code, fill = Dir)) +
  facet_wrap(~dpar, scales = "free_x") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, position=position_dodge(width=0.5), alpha = .5) +
  geom_point(alpha = .5, shape = 21) +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_fill_manual(values = c("white", "black")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Possible parameter values") +
  ylab("Taxon") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(face = "bold", size = 12),
        axis.text.y = element_blank(), panel.grid = element_blank())

Species_coefs_prod_plot <- ggplot(Species_coefs_prod_full, aes(Est, reorder(Taxon, Plotting_Order), colour = Clean_code, fill = Dir)) +
  facet_wrap(~dpar, scales = "free_x") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_point(shape = 21) +
  geom_vline(xintercept = 0,linetype = "dashed") +
  ylab("Taxon") +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_fill_manual(values = c("white", "black")) +
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(face = "italic"))

Order_coefs_live_plt <- ggplot(Order_coefs, aes(Est, order_new, colour = Clean_code, fill = Dir)) +
  facet_wrap(~dpar, scales = "free_x") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, position=position_dodge(width=0.5)) +
  geom_point(position=position_dodge(width=0.5), shape = 21) +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_fill_manual(values = c("white", "black")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(size = 8),
        axis.title.y = element_blank())

Species_coefs_timber_plot <- ggplot(Species_coefs_timber_full, aes(Est, reorder(Taxon, Plotting_Order), colour = Clean_code, fill = Dir)) +
  facet_wrap(~dpar, scales = "free_x") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_point(shape = 21) +
  geom_vline(xintercept = 0,linetype = "dashed") +
  ylab("Taxon") +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_fill_manual(values = c("white", "black")) +
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(face = "italic"))

Figure_S8 <- ggarrange(
  ggarrange(Species_coefs_timber_plot, Order_coefs_live_plt,Species_coefs_prod_plot, 
            nrow = 1, ncol = 3, labels = c("A.","B.", "C.")))


ggsave(path = "Outputs/Figures", Figure_S8, filename = "Figure_S8.png",  bg = "white",
       device = "png", width = 35, height = 25, units = "cm")

ggsave(path = "Outputs/Figures", Species_coefs_live_plt, filename = "Figure_S10.png",  bg = "white",
       device = "png", width = 15, height = 18, units = "cm")




####Raw data for timber, live and products based on importer reported values
####Figures S3 to S6 and S11 and S12###


CITES_IUCN_Timber<- data.table::fread ("Outputs/Data_preparation/Timber/CITES_IUCN_Timber.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_Timber <- CITES_IUCN_Timber %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                  FYear = as.factor(Year))%>% mutate(Volume = Volume) 

Figure_S3<- ggplot(CITES_IUCN_Timber, aes(Year, Volume, colour = Clean_code)) +
  geom_point() + facet_wrap(~Taxon, scales = "free") + theme_bw()

ggsave(path = "Outputs/Figures", Figure_S3, filename = "Figure_S3.png",  bg = "white",
       device = "png", width = 15, height = 18, units = "cm")


CITES_IUCN_live<- data.table::fread ("Outputs/Data_preparation/Live/CITES_IUCN_live.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_live <- CITES_IUCN_live %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                  FYear = as.factor(Year))%>% mutate(Number = n) 
plot_live <- CITES_IUCN_live%>% group_by(order_new, Clean_code, Year) %>% summarise(Number = sum(n)) 

Figure_S11<-ggplot(plot_live, aes(Year, Number, color=Clean_code )) +
  geom_point() + facet_wrap(~order_new, scales = "free") + theme_bw()

ggsave(path = "Outputs/Figures", Figure_S11, filename = "Figure_S11.png",  bg = "white",
       device = "png", width = 15, height = 18, units = "cm")


CITES_IUCN_Products<- data.table::fread ("Outputs/Data_preparation/Products/CITES_IUCN_Products.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_Products <- CITES_IUCN_Products %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                      FYear = as.factor(Year))%>% mutate(Mass = mass) 


Figure_S5<- ggplot(CITES_IUCN_Products, aes(Year, Mass, colour = Clean_code)) +
  geom_point() + facet_wrap(~Taxon, scales = "free") + theme_bw()

ggsave(path = "Outputs/Figures", Figure_S5, filename = "Figure_S5.png",  bg = "white",
       device = "png", width = 15, height = 18, units = "cm")



####Raw data for timber, live and products based on exporter reported values


CITES_IUCN_Timber_E<- data.table::fread ("Outputs/Data_preparation_E/Timber/CITES_IUCN_Timber_E.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_Timber_E <- CITES_IUCN_Timber_E %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                  FYear = as.factor(Year))%>% mutate(Volume = Vol) 

Figure_S4<- ggplot(CITES_IUCN_Timber_E, aes(Year, Volume, colour = Clean_code)) +
  geom_point() + facet_wrap(~Taxon, scales = "free") + theme_bw()

ggsave(path = "Outputs/Figures", Figure_S4, filename = "Figure_S4.png",  bg = "white",
       device = "png", width = 15, height = 18, units = "cm")


CITES_IUCN_live_E<- data.table::fread ("Outputs/Data_preparation_E/Live/CITES_IUCN_live_E.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_live_E <- CITES_IUCN_live_E %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                  FYear = as.factor(Year))%>% mutate(Number = n) 
plot_live_E <- CITES_IUCN_live_E%>% group_by(order_new, Clean_code, Year) %>% summarise(Number = sum(n)) 

Figure_S12<-ggplot(plot_live_E, aes(Year, Number, color=Clean_code )) +
  geom_point() + facet_wrap(~order_new, scales = "free") + theme_bw()

ggsave(path = "Outputs/Figures", Figure_S12, filename = "Figure_S12.png",  bg = "white",
       device = "png", width = 15, height = 18, units = "cm")


CITES_IUCN_Products_E<- data.table::fread ("Outputs/Data_preparation_E/Products/CITES_IUCN_Products_E.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_Products_E <- CITES_IUCN_Products_E %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                      FYear = as.factor(Year))%>% mutate(Mass = mass) 


Figure_S6<- ggplot(CITES_IUCN_Products_E, aes(Year, Mass, colour = Clean_code)) +
  geom_point() + facet_wrap(~Taxon, scales = "free") + theme_bw()

ggsave(path = "Outputs/Figures", Figure_S6, filename = "Figure_S6.png",  bg = "white",
       device = "png", width = 15, height = 18, units = "cm")


######
#####
####

####Old plots###

####Species level###

#### Species-level coefs for timber
All_Taxon_Coef_hu_TE <- rbind(
  ## CR
  (coef(HG_5_E, summary = FALSE)$Taxon[, , "hu_SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "CR"),
  ## EN
  (coef(HG_5_E, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_5_E, summary = FALSE)$Taxon[, , "hu_Clean_codeEN:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "EN"),
  ## VU
  (coef(HG_5_E, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_5_E, summary = FALSE)$Taxon[, , "hu_Clean_codeVU:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "VU"),
  ## NT
  (coef(HG_5_E, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_5_E, summary = FALSE)$Taxon[, , "hu_Clean_codeNT:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "NT"),
  ## LC
  (coef(HG_5_E, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_5_E, summary = FALSE)$Taxon[, , "hu_Clean_codeLC:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "LC"),
  ## NE
  (coef(HG_5_E, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_5_E, summary = FALSE)$Taxon[, , "hu_Clean_codeNE:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "NE")
)
## Summarise

Taxon_Sum_hu_TE <- All_Taxon_Coef_hu_TE %>% group_by(Taxon, IUCN) %>% median_hdci(Betas, .width = .9)
## Get the 2020 statuses per species - the true statuses. So we only keep species that were still listed in 2020 here so 
## we can show the upt to date status (2020)
Taxon_2020 <- CITES_IUCN_Timber_E %>% filter(FYear == 2020) %>% select(Taxon, Clean_code)
Hu_Taxa_betas_TE <- left_join(Taxon_2020, Taxon_Sum_hu_TE, by = c("Taxon", "Clean_code" = "IUCN")) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "LC", "NT", "VU", "EN", "CR")),
         Par = "Presence in trade") %>% arrange(Clean_code, Betas) %>% mutate(Order = 1:n())


##The Species-level mu coeficients

All_Taxon_Coef_mu_TE <- rbind(
  ## CR
  (coef(HG_5_E, summary = FALSE)$Taxon[, , "SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "CR"),
  ## EN
  (coef(HG_5_E, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_5_E, summary = FALSE)$Taxon[, , "Clean_codeEN:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "EN"),
  ## VU
  (coef(HG_5_E, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_5_E, summary = FALSE)$Taxon[, , "Clean_codeVU:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "VU"),
  ## NT
  (coef(HG_5_E, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_5_E, summary = FALSE)$Taxon[, , "Clean_codeNT:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "NT"),
  ## LC
  (coef(HG_5_E, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_5_E, summary = FALSE)$Taxon[, , "Clean_codeLC:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "LC"),
  ## NE
  (coef(HG_5_E, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_5_E, summary = FALSE)$Taxon[, , "Clean_codeNE:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "NE")
)
## Summarise
Taxon_Sum_mu_TE <- All_Taxon_Coef_mu_TE %>% group_by(Taxon, IUCN) %>% median_hdci(Betas, .width = .9)
## Get the 2020 statuses per species - the true statuses
Taxon_2020 <- CITES_IUCN_Timber_E %>% filter(FYear == 2020) %>% select(Taxon, Clean_code)
mu_Taxa_betas_TE <- left_join(Taxon_2020, Taxon_Sum_mu_TE, by = c("Taxon", "Clean_code" = "IUCN")) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "LC", "NT", "VU", "EN", "CR")),
         Par = "Volume if traded") %>% left_join(select(Hu_Taxa_betas_TE, Taxon, Order))

Species_coefs_TE <- rbind(Hu_Taxa_betas_TE, mu_Taxa_betas_TE)

## plot
## Ok so at the minute I kept the species names in as I think they're pretty readable.
## we might be able to do the same for products hopefully and then just for live we'll have to not
Species_plot_TE <- ggplot(Species_coefs_TE, aes(Betas, reorder(Taxon, Order), colour = Clean_code)) +
  facet_wrap(~Par, scales = "free_x") +
  geom_point() +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0,linetype = "dashed") +
  ylab("Taxon") +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(face = "italic"))


###LIve

#### Order level ####
new_dat <- CITES_IUCN_live_2_E %>% group_by(order_new, Clean_code) %>% tally() %>% mutate(SYear = 1)

Order_int <- add_linpred_draws(HG_7_E, newdata = new_dat, re_formula = ~ (1 + SYear|order_new), dpar = "hu")
Order_to_remove <- add_linpred_draws(HG_7_E, newdata = mutate(new_dat, SYear = 0), re_formula = ~ (1 + SYear|order_new), dpar = "hu")

Order_int_mu <- add_linpred_draws(HG_7_E, newdata = new_dat, re_formula = ~ (1 + SYear|order_new), dpar = "mu")
Order_to_remove_mu <- add_linpred_draws(HG_7_E, newdata = mutate(new_dat, SYear = 0), re_formula = ~ (1 + SYear|order_new), dpar = "mu")

## Get the beta trend per code per order_new
Order_IUCN_Beta <- cbind(select(Order_int, order_new) ,data.frame(hu = Order_int$hu - Order_to_remove$hu)) %>% 
  mutate(hu = hu*-1) %>% group_by(order_new, Clean_code) %>% median_hdci(hu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "hu") %>% mutate(dpar = "Presence in trade")

Order_IUCN_Beta_mu <- cbind(select(Order_int_mu, order_new) ,data.frame(mu = Order_int_mu$mu - Order_to_remove_mu$mu)) %>% 
  group_by(order_new, Clean_code) %>% median_hdci(mu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "mu") %>% mutate(dpar = "Quantity if traded")

Order_coefs <- rbind(Order_IUCN_Beta, Order_IUCN_Beta_mu)

Order_coefs_plt <- ggplot(Order_coefs, aes(Est, order_new, colour = Clean_code)) +
  facet_wrap(~dpar, scales = "free_x") +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, position=position_dodge(width=0.5)) +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(size = 8),
        axis.title.y = element_blank()) 


####Products

#### Species-level coefs
All_Taxon_Coef_hu_PE <- rbind(
  ## CR
  (coef(HG_6_E, summary = FALSE)$Taxon[, , "hu_SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "CR"),
  ## EN
  (coef(HG_6_E, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_6_E, summary = FALSE)$Taxon[, , "hu_Clean_codeEN:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "EN"),
  ## VU
  (coef(HG_6_E, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_6_E, summary = FALSE)$Taxon[, , "hu_Clean_codeVU:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "VU"),
  ## NT
  (coef(HG_6_E, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_6_E, summary = FALSE)$Taxon[, , "hu_Clean_codeNT:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "NT"),
  ## LC
  (coef(HG_6_E, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_6_E, summary = FALSE)$Taxon[, , "hu_Clean_codeLC:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "LC"),
  ## NE
  (coef(HG_6_E, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_6_E, summary = FALSE)$Taxon[, , "hu_Clean_codeNE:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "NE")
)
## Summarise


Taxon_Sum_hu_PE <- All_Taxon_Coef_hu_PE %>% group_by(Taxon, IUCN) %>% median_hdci(Betas, .width = .9)
## Get the 2020 statuses per species - the true statuses. So we only keep species that were still listed in 2020 here so 
## we can show the upt to date status (2020)
Taxon_2020 <- CITES_IUCN_products_E %>% filter(FYear == 2020) %>% select(Taxon, Clean_code)
Hu_Taxa_betas_PE <- left_join(Taxon_2020, Taxon_Sum_hu_PE, by = c("Taxon", "Clean_code" = "IUCN")) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "LC", "NT", "VU", "EN", "CR")),
         Par = factor ("Presence in trade")) %>% arrange(Clean_code, Betas) %>% mutate(Order = 1:n())


##The Species-level mu coeficients

All_Taxon_Coef_mu_PE <- rbind(
  ## CR
  (coef(HG_6_E, summary = FALSE)$Taxon[, , "SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "CR"),
  ## EN
  (coef(HG_6_E, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_6_E, summary = FALSE)$Taxon[, , "Clean_codeEN:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "EN"),
  ## VU
  (coef(HG_6_E, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_6_E, summary = FALSE)$Taxon[, , "Clean_codeVU:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "VU"),
  ## NT
  (coef(HG_6_E, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_6_E, summary = FALSE)$Taxon[, , "Clean_codeNT:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "NT"),
  ## LC
  (coef(HG_6_E, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_6_E, summary = FALSE)$Taxon[, , "Clean_codeLC:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "LC"),
  ## NE
  (coef(HG_6_E, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_6_E, summary = FALSE)$Taxon[, , "Clean_codeNE:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "NE")
)
## Summarise
Taxon_Sum_mu_PE <- All_Taxon_Coef_mu_PE %>% group_by(Taxon, IUCN) %>% median_hdci(Betas, .width = .9)
## Get the 2020 statuses per species - the true statuses
Taxon_2020 <- CITES_IUCN_products_E %>% filter(FYear == 2020) %>% select(Taxon, Clean_code)
mu_Taxa_betas_PE <- left_join(Taxon_2020, Taxon_Sum_mu_PE, by = c("Taxon", "Clean_code" = "IUCN")) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "LC", "NT", "VU", "EN", "CR")),
         Par = factor ("Mass if traded")) %>% left_join(select(Hu_Taxa_betas_PE, Taxon, Order))

Species_coefs_PE <- rbind(Hu_Taxa_betas_PE, mu_Taxa_betas_PE)

## plot
## Ok so at the minute I kept the species names in as I think they're pretty readable.
## we might be able to do the same for products hopefully and then just for live we'll have to not
Species_plot_PE <- ggplot(Species_coefs_PE, aes(Betas, reorder(Taxon, Order), colour = Clean_code)) +
  facet_wrap(~Par, scales = "free_x") +
  geom_point() +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0,linetype = "dashed") +
  ylab("Taxon") +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(face = "italic"))


## Plot for Figure S2 of the MS

Figure_S2 <- ggarrange(
  ggarrange(Species_plot_TE, Order_coefs_plt, Species_plot_PE, nrow = 1, ncol = 3, labels = c("A","B", "C")))


ggsave(path = "Outputs/Figures", Figure_3, filename = "FigureS4.png",  bg = "white",
       device = "png", width = 25, height = 25, units = "cm")



####Live trade in species level
##Importer-reported data


#### Species level ####

new_dat2 <- CITES_IUCN_live_2 %>% group_by(Taxon, order_new) %>% filter(SYear == max(SYear)) %>% 
  group_by(Taxon, order_new, Clean_code) %>% tally() %>% mutate(SYear = 1)


Species_int2 <- add_linpred_draws(HG_7, newdata = new_dat2, re_formula = ~ (1 + SYear|order_new/Taxon), dpar = "hu")
Species_to_remove2 <- add_linpred_draws(HG_7, newdata = mutate(new_dat2, SYear = 0), re_formula = ~ (1 + SYear|order_new/Taxon), dpar = "hu")

Species_int_mu2 <- add_linpred_draws(HG_7, newdata = new_dat2, re_formula = ~ (1 + SYear|order_new/Taxon), dpar = "mu")
Species_to_remove_mu2 <- add_linpred_draws(HG_7, newdata = mutate(new_dat2, SYear = 0), re_formula = ~ (1 + SYear|order_new/Taxon), dpar = "mu")

## Get the beta trend per code per order_new
Species_IUCN_Beta <- cbind(select(Species_int2, order_new, Taxon) ,data.frame(hu = Species_int2$hu - Species_to_remove2$hu)) %>% 
  mutate(hu = hu*-1) %>% group_by(Taxon, order_new, Clean_code) %>% median_hdci(hu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "hu") %>% mutate(dpar = "Presence in trade")

Species_IUCN_Beta_mu <- cbind(select(Species_int_mu2, order_new, Taxon) ,data.frame(mu = Species_int_mu2$mu - Species_to_remove_mu2$mu)) %>% 
  group_by(Taxon, order_new, Clean_code) %>% median_hdci(mu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "mu") %>% mutate(dpar = "Quantity if traded")

Plot_order <- Species_IUCN_Beta %>% arrange(Clean_code, Est)  %>% mutate(Plotting_Order = 1:n()) %>%
  select(Taxon, order_new, Plotting_Order)

Species_coefs <- rbind(Species_IUCN_Beta, Species_IUCN_Beta_mu) %>% left_join(Plot_order)



Species_coefs_plt <- ggplot(Species_coefs, aes(Est, reorder(Taxon, Plotting_Order), colour = Clean_code)) +
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 2.5, fill = "grey95", colour = NA) + ## Arecales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 527.5, fill = "white", colour = NA) + ## Asparagales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 527.5, ymax = 591.5, fill = "grey95", colour = NA) + ## Caryophyllales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 591.5, ymax = 603.5, fill = "white", colour = NA) + ## Cyatheales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 603.5, ymax = 632.5, fill = "grey95", colour = NA) + ## Cycadales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 632.5, ymax = 636.5, fill = "white", colour = NA) + ## Dicksoniales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 636.5, ymax = 643.5, fill = "grey95", colour = NA) + ## Ericales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 643.5, ymax = 657.5, fill = "white", colour = NA) + ## Gentianales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 657.5, ymax = 677.5, fill = "grey95", colour = NA) + ## Liliales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 677.5, ymax = 758.5, fill = "white", colour = NA) + ## Malpighiales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 758.5, ymax = 762.5, fill = "grey95", colour = NA) + ## Malvales
#geom_rect(xmin = -Inf, xmax = Inf, ymin = 762.5, ymax = 765.5, fill = "white", colour = NA) + ## Sapindales
facet_wrap(~dpar, scales = "free_x") +
  geom_point(alpha = .5) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, position=position_dodge(width=0.5), alpha = .5) +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(face = "bold", size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(), panel.grid = element_blank())


### Species level for exporter-reported data ####

new_dat2_E <- CITES_IUCN_live_2_E %>% group_by(Taxon, order_new) %>% filter(SYear == max(SYear)) %>% 
  group_by(Taxon, order_new, Clean_code) %>% tally() %>% mutate(SYear = 1)


Species_int2 <- add_linpred_draws(HG_7_E, newdata = new_dat2_E, re_formula = ~ (1 + SYear|order_new/Taxon), dpar = "hu")
Species_to_remove2 <- add_linpred_draws(HG_7_E, newdata = mutate(new_dat2_E, SYear = 0), re_formula = ~ (1 + SYear|order_new/Taxon), dpar = "hu")

Species_int_mu2 <- add_linpred_draws(HG_7_E, newdata = new_dat2_E, re_formula = ~ (1 + SYear|order_new/Taxon), dpar = "mu")
Species_to_remove_mu2 <- add_linpred_draws(HG_7_E, newdata = mutate(new_dat2_E, SYear = 0), re_formula = ~ (1 + SYear|order_new/Taxon), dpar = "mu")

## Get the beta trend per code per order_new
Species_IUCN_Beta <- cbind(select(Species_int2, order_new, Taxon) ,data.frame(hu = Species_int2$hu - Species_to_remove2$hu)) %>% 
  mutate(hu = hu*-1) %>% group_by(Taxon, order_new, Clean_code) %>% median_hdci(hu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "hu") %>% mutate(dpar = "Presence in trade")

Species_IUCN_Beta_mu <- cbind(select(Species_int_mu2, order_new, Taxon) ,data.frame(mu = Species_int_mu2$mu - Species_to_remove_mu2$mu)) %>% 
  group_by(Taxon, order_new, Clean_code) %>% median_hdci(mu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "mu") %>% mutate(dpar = "Quantity if traded")

Plot_order <- Species_IUCN_Beta %>% arrange(Clean_code, Est)  %>% mutate(Plotting_Order = 1:n()) %>%
  select(Taxon, order_new, Plotting_Order)

Species_coefs <- rbind(Species_IUCN_Beta, Species_IUCN_Beta_mu) %>% left_join(Plot_order)



Species_coefs_plt_E <- ggplot(Species_coefs, aes(Est, reorder(Taxon, Plotting_Order), colour = Clean_code)) +
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 2.5, fill = "grey95", colour = NA) + ## Arecales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 527.5, fill = "white", colour = NA) + ## Asparagales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 527.5, ymax = 591.5, fill = "grey95", colour = NA) + ## Caryophyllales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 591.5, ymax = 603.5, fill = "white", colour = NA) + ## Cyatheales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 603.5, ymax = 632.5, fill = "grey95", colour = NA) + ## Cycadales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 632.5, ymax = 636.5, fill = "white", colour = NA) + ## Dicksoniales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 636.5, ymax = 643.5, fill = "grey95", colour = NA) + ## Ericales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 643.5, ymax = 657.5, fill = "white", colour = NA) + ## Gentianales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 657.5, ymax = 677.5, fill = "grey95", colour = NA) + ## Liliales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 677.5, ymax = 758.5, fill = "white", colour = NA) + ## Malpighiales
  #geom_rect(xmin = -Inf, xmax = Inf, ymin = 758.5, ymax = 762.5, fill = "grey95", colour = NA) + ## Malvales
#geom_rect(xmin = -Inf, xmax = Inf, ymin = 762.5, ymax = 765.5, fill = "white", colour = NA) + ## Sapindales
facet_wrap(~dpar, scales = "free_x") +
  geom_point(alpha = .5) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, position=position_dodge(width=0.5), alpha = .5) +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(face = "bold", size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(), panel.grid = element_blank())


## Plot for Figure S5 of the MS

Figure_S5 <- ggarrange(
  ggarrange(Species_coefs_plt, Species_coefs_plt_E, nrow = 1, ncol = 2, labels = c("A","B")))


ggsave(path = "Outputs/Figures", Figure_S5, filename = "FigureS5.png",  bg = "white",
       device = "png", width = 25, height = 25, units = "cm")






#### Species-level coefs of timber
All_Taxon_Coef_hu_T <- rbind(
  ## CR
  (coef(HG_5, summary = FALSE)$Taxon[, , "hu_SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "CR"),
  ## EN
  (coef(HG_5, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_5, summary = FALSE)$Taxon[, , "hu_Clean_codeEN:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "EN"),
  ## VU
  (coef(HG_5, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_5, summary = FALSE)$Taxon[, , "hu_Clean_codeVU:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "VU"),
  ## NT
  (coef(HG_5, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_5, summary = FALSE)$Taxon[, , "hu_Clean_codeNT:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "NT"),
  ## LC
  (coef(HG_5, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_5, summary = FALSE)$Taxon[, , "hu_Clean_codeLC:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "LC"),
  ## NE
  (coef(HG_5, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_5, summary = FALSE)$Taxon[, , "hu_Clean_codeNE:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "NE")
)
## Summarise

Taxon_Sum_hu_T <- All_Taxon_Coef_hu_T %>% group_by(Taxon, IUCN) %>% median_hdci(Betas, .width = .9)
## Get the 2020 statuses per species - the true statuses. So we only keep species that were still listed in 2020 here so 
## we can show the upt to date status (2020)
Taxon_2020 <- CITES_IUCN_Timber %>% filter(FYear == 2020) %>% select(Taxon, Clean_code)
Hu_Taxa_betas_T <- left_join(Taxon_2020, Taxon_Sum_hu_T, by = c("Taxon", "Clean_code" = "IUCN")) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "LC", "NT", "VU", "EN", "CR")),
         Par = "Presence in trade") %>% arrange (Clean_code, Betas) %>% mutate(Order = 1:n())


##The Species-level mu coeficients

All_Taxon_Coef_mu_T <- rbind(
  ## CR
  (coef(HG_5, summary = FALSE)$Taxon[, , "SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "CR"),
  ## EN
  (coef(HG_5, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_5, summary = FALSE)$Taxon[, , "Clean_codeEN:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "EN"),
  ## VU
  (coef(HG_5, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_5, summary = FALSE)$Taxon[, , "Clean_codeVU:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "VU"),
  ## NT
  (coef(HG_5, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_5, summary = FALSE)$Taxon[, , "Clean_codeNT:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "NT"),
  ## LC
  (coef(HG_5, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_5, summary = FALSE)$Taxon[, , "Clean_codeLC:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "LC"),
  ## NE
  (coef(HG_5, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_5, summary = FALSE)$Taxon[, , "Clean_codeNE:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "NE")
)
## Summarise
Taxon_Sum_mu_T <- All_Taxon_Coef_mu_T %>% group_by(Taxon, IUCN) %>% median_hdci(Betas, .width = .9)
## Get the 2020 statuses per species - the true statuses
Taxon_2020 <- CITES_IUCN_Timber %>% filter(FYear == 2020) %>% select(Taxon, Clean_code)
mu_Taxa_betas_T <- left_join(Taxon_2020, Taxon_Sum_mu_T, by = c("Taxon", "Clean_code" = "IUCN")) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "LC", "NT", "VU", "EN", "CR")),
         Par = "Volume if traded") %>% left_join(select(Hu_Taxa_betas, Taxon, Order))

Species_coefs_T <- rbind(Hu_Taxa_betas_T, mu_Taxa_betas_T)

## plot
## Ok so at the minute I kept the species names in as I think they're pretty readable.
## we might be able to do the same for products hopefully and then just for live we'll have to not
Species_plot_T <- ggplot(Species_coefs_T, aes(Betas, reorder(Taxon, Order), colour = Clean_code)) +
  facet_wrap(~Par, scales = "free_x") +
  geom_point() +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0,linetype = "dashed") +
  ylab("Taxon") +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(face = "italic"))


#### Order level ####
new_dat <- CITES_IUCN_live %>% group_by(order_new, Clean_code) %>% tally() %>% mutate(SYear = 1)

Order_int <- add_linpred_draws(HG_7, newdata = new_dat, re_formula = ~ (1 + SYear|order_new), dpar = "hu")
Order_to_remove <- add_linpred_draws(HG_7, newdata = mutate(new_dat, SYear = 0), re_formula = ~ (1 + SYear|order_new), dpar = "hu")

Order_int_mu <- add_linpred_draws(HG_7, newdata = new_dat, re_formula = ~ (1 + SYear|order_new), dpar = "mu")
Order_to_remove_mu <- add_linpred_draws(HG_7, newdata = mutate(new_dat, SYear = 0), re_formula = ~ (1 + SYear|order_new), dpar = "mu")

## Get the beta trend per code per order_new
Order_IUCN_Beta <- cbind(select(Order_int, order_new) ,data.frame(hu = Order_int$hu - Order_to_remove$hu)) %>% 
  mutate(hu = hu*-1) %>% group_by(order_new, Clean_code) %>% median_hdci(hu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "hu") %>% mutate(dpar = "Presence in trade")

Order_IUCN_Beta_mu <- cbind(select(Order_int_mu, order_new) ,data.frame(mu = Order_int_mu$mu - Order_to_remove_mu$mu)) %>% 
  group_by(order_new, Clean_code) %>% median_hdci(mu, .width = .9) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "DD", "LC", "NT", "VU", "EN", "CR"))) %>%
  rename("Est" = "mu") %>% mutate(dpar = "Quantity if traded")

Order_coefs <- rbind(Order_IUCN_Beta, Order_IUCN_Beta_mu)

Order_coefs_plt <- ggplot(Order_coefs, aes(Est, order_new, colour = Clean_code)) +
  facet_wrap(~dpar, scales = "free_x") +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, position=position_dodge(width=0.5)) +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(size = 8),
        axis.title.y = element_blank()) 

HG_6 <- readRDS("Outputs/Models/Products/HG_6.rds")
CITES_IUCN_Products<- data.table::fread ("Outputs/Data_preparation/Products/CITES_IUCN_Products.csv", na.strings = "")

## Standardise year and make a factor variable
CITES_IUCN_Products <- CITES_IUCN_Products %>% mutate(SYear = (Year - mean(Year))/sd(Year),
                                                      FYear = as.factor(Year))

#### Species-level coefs for products###
All_Taxon_Coef_hu_P <- rbind(
  ## CR
  (coef(HG_6, summary = FALSE)$Taxon[, , "hu_SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "CR"),
  ## EN
  (coef(HG_6, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_6, summary = FALSE)$Taxon[, , "hu_Clean_codeEN:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "EN"),
  ## VU
  (coef(HG_6, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_6, summary = FALSE)$Taxon[, , "hu_Clean_codeVU:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "VU"),
  ## NT
  (coef(HG_6, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_6, summary = FALSE)$Taxon[, , "hu_Clean_codeNT:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "NT"),
  ## LC
  (coef(HG_6, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_6, summary = FALSE)$Taxon[, , "hu_Clean_codeLC:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "LC"),
  ## NE
  (coef(HG_6, summary = FALSE)$Taxon[, , "hu_SYear"] +
     coef(HG_6, summary = FALSE)$Taxon[, , "hu_Clean_codeNE:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(Betas = Betas*-1,
           IUCN = "NE")
)
## Summarise


Taxon_Sum_hu_P <- All_Taxon_Coef_hu_P %>% group_by(Taxon, IUCN) %>% median_hdci(Betas, .width = .9)
## Get the 2020 statuses per species - the true statuses. So we only keep species that were still listed in 2020 here so 
## we can show the upt to date status (2020)
Taxon_2020 <- CITES_IUCN_Products %>% filter(FYear == 2020) %>% select(Taxon, Clean_code)
Hu_Taxa_betas_P <- left_join(Taxon_2020, Taxon_Sum_hu_P, by = c("Taxon", "Clean_code" = "IUCN")) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "LC", "NT", "VU", "EN", "CR")),
         Par = factor("Presence in trade")) %>% arrange(Clean_code, Betas) %>% mutate(Order = 1:n())


##The Species-level mu coeficients

All_Taxon_Coef_mu_P <- rbind(
  ## CR
  (coef(HG_6, summary = FALSE)$Taxon[, , "SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "CR"),
  ## EN
  (coef(HG_6, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_6, summary = FALSE)$Taxon[, , "Clean_codeEN:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "EN"),
  ## VU
  (coef(HG_6, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_6, summary = FALSE)$Taxon[, , "Clean_codeVU:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "VU"),
  ## NT
  (coef(HG_6, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_6, summary = FALSE)$Taxon[, , "Clean_codeNT:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "NT"),
  ## LC
  (coef(HG_6, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_6, summary = FALSE)$Taxon[, , "Clean_codeLC:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "LC"),
  ## NE
  (coef(HG_6, summary = FALSE)$Taxon[, , "SYear"] +
     coef(HG_6, summary = FALSE)$Taxon[, , "Clean_codeNE:SYear"]) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "Taxon", values_to = "Betas") %>%
    mutate(IUCN = "NE")
)
## Summarise
Taxon_Sum_mu_P <- All_Taxon_Coef_mu_P %>% group_by(Taxon, IUCN) %>% median_hdci(Betas, .width = .9)
## Get the 2020 statuses per species - the true statuses
Taxon_2020 <- CITES_IUCN_Products %>% filter(FYear == 2020) %>% select(Taxon, Clean_code)
mu_Taxa_betas_P <- left_join(Taxon_2020, Taxon_Sum_mu_P, by = c("Taxon", "Clean_code" = "IUCN")) %>%
  mutate(Clean_code = factor(Clean_code, levels = c("NE", "LC", "NT", "VU", "EN", "CR")),
         Par = factor ( "Mass if traded") ) %>% left_join(select(Hu_Taxa_betas, Taxon, Order))

Species_coefs_P <- rbind(Hu_Taxa_betas_P, mu_Taxa_betas_P)

## plot
## Ok so at the minute I kept the species names in as I think they're pretty readable.
## we might be able to do the same for products hopefully and then just for live we'll have to not
Species_plot_P <- ggplot(Species_coefs_P, aes(Betas, reorder(Taxon, Order), colour = Clean_code)) +
  facet_wrap(~Par, scales = "free_x") +
  geom_point() +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0,linetype = "dashed") +
  ylab("Taxon") +
  scale_color_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text( size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(face = "italic"))

# Plot for Figure 4 of the MS

Figure_4 <- ggarrange(
  ggarrange(Species_plot_T, Order_coefs_plt,Species_plot_P, nrow = 1, ncol = 3, labels = c("A","B", "C")))


ggsave(path = "Outputs/Figures", Figure_3, filename = "Figure3.png",  bg = "white",
       device = "png", width = 25, height = 25, units = "cm")





