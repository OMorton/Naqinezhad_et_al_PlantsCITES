
#### Tally ####
## Convenience for repetitve looks at tallies
tally_dat <- function(data) {
  sum <- data %>% ungroup() %>%
    summarise(Importer= length(unique(Importer)),
              Exporter = length(unique(Exporter)),
              Taxon = length(unique(Taxon)),
              IUCNName = length(unique(IUCNName)))
  return(sum)
}


#### Richness and volume plots ####
## convenience for repetitve plotting
raw_summary <- function(data, type = c("richness", "volume"), return_data = FALSE) {
  
  if (select(data, contains("mass")) %>% ncol() > 0){
    data <- data %>% mutate(Quantity = mass)
  }
  if (select(data, contains("volume")) %>% ncol() > 0){
    data <- data %>% mutate(Quantity = Volume)
  }
  if (length(colnames(data)[colnames(data) == "n"]) > 0){
    data <- data %>% mutate(Quantity = n)
  }
  
  if (type == "volume") {
  IUCN_sum <- data %>% group_by(Clean_code, Year) %>%
    filter(Year>1999) %>% filter(Quantity>0)%>% summarise(Quantity = sum (Quantity))
  
  tot_sum <- data %>% group_by(Year) %>% summarise(Quantity = sum(Quantity)) %>% 
    mutate(Clean_code = "Total")
  
  plt_data <- rbind(IUCN_sum, tot_sum) %>% 
    mutate(Clean_code = factor(Clean_code, levels = c("Total", "CR", "EN","VU", "NT" , "LC", "NE"))) 
  
  plot <- ggplot(plt_data, aes(Year, Quantity, colour = Clean_code)) + 
    geom_line() +  
    geom_point()+
    scale_y_continuous(labels = function(y_value) y_value / 1000) +
    scale_colour_manual(values = c("black","red", "orange3", "goldenrod2",
                                   "skyblue", "dodgerblue",  "grey"))+ 
    guides(colour = guide_legend(nrow = 1)) +
    theme_minimal()+
    theme(legend.position = 'bottom', legend.title = element_blank(),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5))
  
    if (select(data, contains("mass")) %>% ncol() > 0){
      plot <- plot + ylab ("Mass (1000's of kg)")

    }
    if (select(data, contains("volume")) %>% ncol() > 0){
      plot <- plot + labs(y = expression(paste("Volume (1000's of ", " ", m^3, ")")))
    }
    if (length(colnames(data)[colnames(data) == "n"]) > 0){
      plot <- plot + ylab ("Individuals (1000's)")
    }
  }
  
  if (type == "richness") {

    iucn_sum <- data %>% group_by(Clean_code, Year) %>%
      filter(Year>1999) %>% filter(Quantity>0)%>% summarise(Richness = n_distinct(Taxon))%>% 
      mutate(Clean_code = factor(Clean_code, levels = c("CR", "EN","VU", "NT" , "LC", "NE")))
    
    tot_sum <- iucn_sum %>% group_by(Year) %>% summarise(Richness = sum(Richness)) %>% 
      mutate(Clean_code = "Total")
    
    plt_data <- rbind(iucn_sum, tot_sum) %>% 
      mutate(Clean_code = factor(Clean_code, levels = c("Total", "CR", "EN","VU", "NT" , "LC", "NE"))) 
    
    plot <- ggplot(plt_data, aes(Year, Richness, colour = Clean_code)) + 
      geom_line() +  
      geom_point()+ 
      xlab("") +
      coord_cartesian(ylim = c(0, max(plt_data$Richness)*1.2)) +
      scale_colour_manual(values = c("black","red", "orange3", "goldenrod2",
                                     "skyblue", "dodgerblue",  "grey"))+ 
      guides(colour = guide_legend(nrow = 1)) +
      theme_minimal()+
      theme(legend.position = 'bottom', legend.title = element_blank())
  }
  
  if(return_data == FALSE){
  return(plot)} else {
    return(list("plot" = plot, "data" = plt_data))
  }
} 

#### Average temporal trends ####

ave_species_plt <- function(data, model, complex = FALSE, return_summary = FALSE) {
  
  if (select(data, contains("mass")) %>% ncol() > 0){
    data <- data %>% mutate(Quantity = mass)
  }
  if (select(data, contains("volume")) %>% ncol() > 0){
    data <- data %>% mutate(Quantity = Volume)
  }
  if (length(colnames(data)[colnames(data) == "n"]) > 0){
    data <- data %>% mutate(Quantity = n)
  }
  
  data <- data %>%
    mutate(Clean_code = factor(Clean_code, levels = c("CR", "EN", "VU", "NT", "LC", "NE" )))
  
  newdat <- data %>% select(SYear, Year, FYear, Clean_code) %>% distinct()
  
  median_fit <- add_epred_draws(model, newdata = newdat, re_formula = NA, summary = FALSE) %>%
    ## then group and summarise the posterior
    group_by(Year, Clean_code) %>%
    median_hdci(.epred, .width = .9) %>%
    mutate(Clean_code = factor(Clean_code, levels = c("CR", "EN", "VU", "NT", "LC", "NE" )))
  
  if (complex == FALSE) {
  line_plot <- ggplot(median_fit, aes(Year, .epred, colour = Clean_code)) +
    geom_line(size = 1) +  
    scale_color_manual(values = c("red", "orange3", "goldenrod2", "skyblue", "dodgerblue", "grey" )) +
    theme_minimal(base_size = 12) +
    theme(legend.position = 'none' , axis.title.x = element_blank()) 
  }
  
  if (complex == TRUE) {
    median_fit <- add_epred_draws(model, newdata = newdat, re_formula = ~(1|FYear), summary = FALSE) %>%
      ## then group and summarise the posterior
      group_by(Year, Clean_code) %>%
      median_hdci(.epred, .width = .9) %>%
      mutate(Clean_code = factor(Clean_code, levels = c("CR", "EN", "VU", "NT", "LC", "NE" )))
    
    sample_fit <- add_epred_draws(model, newdata = newdat, re_formula = ~(1|FYear), summary = FALSE, ndraws = 25) %>%
      unite("ID", c("Clean_code", ".draw"), remove = FALSE) %>%
      mutate(Clean_code = factor(Clean_code, levels = c("CR", "EN", "VU", "NT", "LC", "NE" )))
    
    line_plot <- ggplot(median_fit, aes(Year, .epred, colour = Clean_code)) +
      geom_point(data = data, aes(Year, Quantity), alpha = .5) +
      geom_line(data = sample_fit, aes(group = ID), alpha = .1) +
      geom_line(size = 1) +  
      scale_color_manual(values = c("red", "orange3", "goldenrod2", "skyblue", "dodgerblue", "grey" )) +
      scale_y_log10() +
      theme_minimal(base_size = 12) +
      theme(legend.position = 'none' , axis.title.x = element_blank())
  }
  
  if (select(data, contains("mass")) %>% ncol() > 0){
    line_plot <- line_plot + ylab ("Mass (kg)")
    
  }
  if (select(data, contains("volume")) %>% ncol() > 0){
    line_plot <- line_plot + labs(y = expression(paste("Volume (", " ", m^3, ")")))
  }
  if (length(colnames(data)[colnames(data) == "n"]) > 0){
    line_plot <- line_plot + ylab ("Individuals")
  }
  
  if (return_summary == FALSE) {
    return(line_plot)
  }
  
  if (return_summary == TRUE) {
    return("plot" = line_plot, "median_summary" = median_fit)
    
  }
}

#### Fixed effects ####

ave_coeficient_plt <- function(model, data, return_summary = FALSE) {
  
  if (select(data, contains("mass")) %>% ncol() > 0){
    label <- "Mass if traded"
  }
  if (select(data, contains("volume")) %>% ncol() > 0){
    label <- "Volume if traded"
  }
  if (length(colnames(data)[colnames(data) == "n"]) > 0){
    label <- "Quantity if traded"
  }
  
  #### Mu ####
  Mu_Fixefs <- fixef(model, summary = FALSE) %>% as.data.frame() %>%
    select(!starts_with("hu_"))
  
  Mu_BetaYrs <- Mu_Fixefs %>%
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
           Par = paste0(label))
  
  ## Sum
  Mu_BetaYrs_Summary <- Mu_BetaYrs %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9) %>%
    left_join(Mu_BetaYrs %>% group_by(IUCN) %>%
                group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
    mutate(pd = pd*100, Par = paste0(label))
  
  
  #### Hu ####
  Hu_Fixefs <- fixef(model, summary = FALSE) %>% as.data.frame() %>%
    select(starts_with("hu_"))
  
  Hu_BetaYrs <- Hu_Fixefs%>% 
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
  
  Hu_BetaYrs_Summary <- Hu_BetaYrs %>% group_by(IUCN) %>% median_hdci(Betas, .width = .9)%>%
    left_join(Hu_BetaYrs %>% group_by(IUCN) %>%
                group_modify(~data.frame(pd = p_direction(.x$Betas)[1]))) %>%
    mutate(pd = pd*100, Par = "Presence in trade")
  
  #### PLotting ####
  fixef_sum <- rbind(Mu_BetaYrs_Summary, Hu_BetaYrs_Summary) %>%
    mutate(Par = factor(Par, levels = c("Presence in trade", paste0(label))))
  fixef_sum_pos <- rbind(Mu_BetaYrs_Summary, Hu_BetaYrs_Summary) %>% filter(Betas > 0) %>%
    mutate(Par = factor(Par, levels = c("Presence in trade", paste0(label))))
  fixef_sum_neg <- rbind(Mu_BetaYrs_Summary, Hu_BetaYrs_Summary) %>% filter(Betas < 0) %>%
    mutate(Par = factor(Par, levels = c("Presence in trade", paste0(label))))
  
  fixef_raw <- rbind(Mu_BetaYrs, Hu_BetaYrs) %>%
    mutate(Par = factor(Par, levels = c("Presence in trade", paste0(label))))
  
  ## So this has hu and mu next to each other and the text numbers show the percentage of the posterior with the same sign
  ## as the median its like a indication of how certain we are that there is a directional effect.
  Fixef_plot <- ggplot(fixef_sum, aes(Betas, IUCN, fill = IUCN)) +
    facet_wrap(~Par, scales = "free_x", ncol = 2) +
    geom_density_ridges(data = fixef_raw, alpha = .75, scale = .9, rel_min_height = 0.01) +
    geom_point(data = fixef_sum, aes(Betas, IUCN), size = 2) +
    geom_errorbarh(data = fixef_sum, aes(xmin = .lower, xmax = .upper),
                   height = 0, size = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", size = .75) +
    geom_text(data = fixef_sum_neg, aes(x =  .lower - 1.25, y = IUCN, label = pd), 
              nudge_y = .25,  size = 3) +
    geom_text(data = fixef_sum_pos, aes(x =  .upper + 1.25, y = IUCN, label = pd), 
              nudge_y = .25,  size = 3) +
    xlab("Possible parameter values") +
    scale_fill_manual(values = c("grey", "dodgerblue", "skyblue", "goldenrod2", "orange3", "red")) +
    scale_y_discrete(limits = c("Beta_NE", "Beta_LC", "Beta_NT", "Beta_VU", "Beta_EN", "Beta_CR"),
                     labels = c("NE", "LC", "NT", "VU", "EN", "CR"))+
    scale_x_continuous(breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", strip.text = element_text(size = 9),
          axis.title.y = element_blank(), axis.title.x = element_blank())
  
  if(return_summary == FALSE){
    return(Fixef_plot)} else {
      return(list("plot" = Fixef_plot, "coef_summary" = fixef_sum))}
}


