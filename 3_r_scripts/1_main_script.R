# Analysis of nestling tree swallow feather quality in 2018 & 2019
# Written by Conor Taff
# Last updated 4/7/2021
# Run under R Studio 1.1.463 R v4.0.2 on Mac OSX 10.11.6


# Load packages ----
  pacman::p_load(tidyverse, here, lme4, scales, rptR, dabestr, ggpubr, grid, gridExtra, sjPlot, lmerTest, ggExtra, viridis, MuMIn)

# Load data ----
    # Data with one row per individual (includes adults and nestlings)
      di <- read.delim(here::here("1_raw_data/data_by_individual.txt"))
    # Data with repeat measurements by different observers for repeatability
      do <- read.delim(here::here("1_raw_data/data_by_observer.txt"))
    # Provisioning data from RFID records
      dp <- read.delim(here::here("1_raw_data/data_for_provisioning.txt"))
    # Data with one row per nest (several nestlings and two adults can share a nest)
      dnest <- read.delim(here::here("1_raw_data/data_by_nest.txt"))
    # Data from Callan et al 2019 downloaded from publicly available Dryad package
      callan <- read.delim(here::here("1_raw_data/callan_et_al_data.txt"))
      
    # Some data wrngling to make columns and join data sets as needed later
      di$full_treatment <- paste(di$treat1, di$treat2, sep = "_")
      
      trt_group <- data.frame(full_treatment = c("_", "Control_Control", "Control_Dull", "Control_Predator",
                                                 "Control_Tape", "Dull_Control", "Dull_Predator", "Dull_Tape",
                                                 "Predator_Control", "Predator_Dull"),
                              trt_group = c("Control", "Control", "Control", "Predator", "Handicap", "Control", 
                                            "Predator", "Handicap", "Predator", "Predator"))
      di <- plyr::join(di, trt_group, "full_treatment")
      
      
 
# Set options ----
  # Plotting and other options that will be used throughout the code below
  
    n_color <- "#56B4E9" # main color for nestlings
    a_color <- "#E69F00" # main color for adults
    
    p_color <- "#009E73" # color for predator treatment
    c_color <- "#0072B2" # color for control treatment
    s_color <- "#D55E00" # color for taping treatment

# Concept figure of tradeoffs ----
    # Drawing a figure showing the Callan data with possible intra-specific patterns that could result in the same inter-specific pattern
      for(i in 1:nrow(callan)){
        callan$tnest[i] <- strsplit(callan$time_nest[i], split = " ")[[1]][1]
      }
    # save just tree swallow
        c2 <- subset(callan, callan$species == "Tachycineta_bicolor")
        c2$sps <- "a"
      p1 <- callan %>%
        filter(is.na(tnest) == FALSE) %>%
        ggplot(mapping = aes(x = as.numeric(tnest), y = plumage_qual, fill = "coral3")) + 
        geom_point(pch = 21) + geom_smooth(method = "lm", fill = "slateblue", col = "gray30") +
        theme_classic() + guides(fill = FALSE) + xlab("Time in nest (days)") + ylab("Plumage quality (nestling / adult barb density)") +
        annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5) +
        #annotate("text", 25, 0.5, label = "Callan et al. result", col = "slateblue") +
        ggtitle("Callan et al. (2019) result") + theme(plot.title = element_text(color = "slateblue")) +
        geom_point(data = c2, mapping = aes(x = as.numeric(tnest), y = plumage_qual), fill = "blue", shape = 23, size = 3)
    
    # Simulating hypothetical data for panels B, C, D, and E
        set.seed(seed = 142)
        sim <- callan[, c("plumage_qual", "tnest")]
        sim <- subset(sim, is.na(sim$tnest) == FALSE & is.na(sim$plumage_qual) == FALSE)
        sim$tnest <- as.numeric(sim$tnest)
        sim$range <- rnorm(nrow(sim), mean = 4, sd = 1)
        sim$min <- sim$tnest - 0.5 * sim$range
        sim$max <- sim$tnest + 0.5 * sim$range
        sim$pos_sl <- rnorm(nrow(sim), 0.012606, 0.002)
        sim$neg_sl <- rnorm(nrow(sim), -0.012606, 0.002)
        sim$no_sl <- rnorm(nrow(sim), 0, 0.001)
        sim$het_sl <- rnorm(nrow(sim), 0, 0.01)
        sim$pos_y1 <- sim$plumage_qual - (sim$tnest - sim$min) * sim$pos_sl
        sim$pos_y2 <- sim$plumage_qual + (sim$max - sim$tnest) * sim$pos_sl
        sim$sps <- as.factor(seq(1:nrow(sim)))
        sim$neg_y1 <- sim$plumage_qual - (sim$tnest - sim$min) * sim$neg_sl
        sim$neg_y2 <- sim$plumage_qual + (sim$max - sim$tnest) * sim$neg_sl
        sim$het_y1 <- sim$plumage_qual - (sim$tnest - sim$min) * sim$het_sl
        sim$het_y2 <- sim$plumage_qual + (sim$max - sim$tnest) * sim$het_sl
        sim$no_y1 <- sim$plumage_qual - (sim$tnest - sim$min) * sim$no_sl
        sim$no_y2 <- sim$plumage_qual + (sim$max - sim$tnest) * sim$no_sl
    
    # Draw each panel
        p2 <- ggplot(data = data.frame(x = c(sim$min, sim$max), y = c(sim$pos_y1, sim$pos_y2), sps = rep(sim$sps, 2)),
              mapping = aes(x = x, y = y, by = sps)) + geom_line(col = "gray40") +
              theme_classic() + xlab("") + ylab("") +
              geom_point(data = sim, mapping = aes(x = tnest, y = plumage_qual), fill = "coral3", pch = 21, size = 0.8) +
          annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5) +
          #annotate("text", x = 28, y = 0.5, label = "Positive", col = "slateblue") + 
          ylim(c(0.4, 1.05)) +
          ggtitle("Positive") + theme(plot.title = element_text(color = "slateblue")) +
          geom_point(data = c2, mapping = aes(x = as.numeric(tnest), y = plumage_qual), fill = "blue", shape = 23, size = 3)
        
        p3 <- ggplot(data = data.frame(x = c(sim$min, sim$max), y = c(sim$neg_y1, sim$neg_y2), sps = rep(sim$sps, 2)),
              mapping = aes(x = x, y = y, by = sps)) + geom_line(col = "gray40") +
              theme_classic() + xlab("") + ylab("") +
              geom_point(data = sim, mapping = aes(x = tnest, y = plumage_qual), fill = "coral3", pch = 21, size = 0.8) +
          annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5) +
          #annotate("text", x = 26, y = 0.5, label = "Negative", col = "slateblue") +
          ylim(c(0.4, 1.05)) +
          ggtitle("Negative") + theme(plot.title = element_text(color = "slateblue")) +
          geom_point(data = c2, mapping = aes(x = as.numeric(tnest), y = plumage_qual), fill = "blue", shape = 23, size = 3)
        
        p4 <- ggplot(data = data.frame(x = c(sim$min, sim$max), y = c(sim$no_y1, sim$no_y2), sps = rep(sim$sps, 2)),
              mapping = aes(x = x, y = y, by = sps)) + geom_line(col = "gray40") +
              theme_classic() + xlab("Time in nest (days)") + ylab("") +
              geom_point(data = sim, mapping = aes(x = tnest, y = plumage_qual), fill = "coral3", pch = 21, size = 0.8) +
          annotate("text", x = -Inf, y = Inf, label = "D", hjust = -0.5, vjust = 1.5) +
          #annotate("text", x = 28, y = 0.5, label = "Absent", col = "slateblue") +
          ylim(c(0.4, 1.05)) +
          ggtitle("Absent") + theme(plot.title = element_text(color = "slateblue")) +
          geom_point(data = c2, mapping = aes(x = as.numeric(tnest), y = plumage_qual), fill = "blue", shape = 23, size = 3)
        
        p5 <- ggplot(data = data.frame(x = c(sim$min, sim$max), y = c(sim$het_y1, sim$het_y2), sps = rep(sim$sps, 2)),
              mapping = aes(x = x, y = y, by = sps)) + geom_line(col = "gray40") +
              theme_classic() + xlab("Time in nest (days)") + ylab("") +
              geom_point(data = sim, mapping = aes(x = tnest, y = plumage_qual), fill = "coral3", pch = 21, size = 0.8) +
          annotate("text", x = -Inf, y = Inf, label = "E", hjust = -0.5, vjust = 1.5) +
          ylim(c(0.4, 1.05)) +
          #annotate("text", x = 23, y = 0.5, label = "Heterogeneous", col = "slateblue") +
          ggtitle("Heterogeneous") + theme(plot.title = element_text(color = "slateblue")) +
          geom_point(data = c2, mapping = aes(x = as.numeric(tnest), y = plumage_qual), fill = "blue", shape = 23, size = 3)
    
    # Save the figure to disk
        ggsave(here::here("3_r_scripts/figure_1.png"),
               grid.arrange(p1, p2, p3, p4, p5, layout_matrix = 
                              rbind(c(1, 1, 1, 1, 2, 2, 3, 3), 
                                    c(1, 1, 1, 1, 2, 2, 3, 3),
                                    c(1, 1, 1, 1, 4, 4, 5, 5),
                                    c(1, 1, 1, 1, 4, 4, 5, 5))),
               device = "png", width = 8, height = 4)
    
# Calculate observer and multi-feather repeatability ----
  # Repeatability for multiple feathers from the same bird
    d_rep <- pivot_longer(di, c("br1_den", "br2_den", "back1_den", "back2_den"), names_to = "type", values_to = "value")
    grp <- data.frame(type = c("br1_den", "br2_den", "back1_den", "back2_den"), grp = c("breast", "breast", "back", "back"))
    d_rep <- plyr::join(d_rep, grp, "type", "left", "first")
    brst_rpt <- subset(d_rep, d_rep$grp == "breast")
    back_rpt <- subset(d_rep, d_rep$grp == "back")
    
    #Overall breast and back repeatability
      # r_br_all <- rpt(value ~ (1|band), grname = "band", data = brst_rpt, datatype = "Gaussian", nboot = 1000, npermut = 0)
      # r_ba_all <- rpt(value ~ (1|band), grname = "band", data = back_rpt, datatype = "Gaussian", nboot = 1000, npermut = 0)
      # 
      # saveRDS(r_br_all, here("5_saved_objects/r_br_all.rds"))
      # saveRDS(r_ba_all, here("5_saved_objects/r_ba_all.rds"))
      
      # r_br_ad <- rpt(value ~ (1|band), grname = "band", data = subset(brst_rpt, brst_rpt$age == "adult", 
      #                 datatype = "Gaussian", nboot = 1000, npermut = 0))
      # r_ba_ad <- rpt(value ~ (1|band), grname = "band", data = subset(back_rpt, back_rpt$age == "adult", 
      #                 datatype = "Gaussian", nboot = 1000, npermut = 0)) 
      
      # saveRDS(r_br_ad, here("5_saved_objects/r_br_ad.rds"))
      # saveRDS(r_ba_ad, here("5_saved_objects/r_ba_ad.rds"))
      # 
      # r_br_ne <- rpt(value ~ (1|band), grname = "band", data = subset(brst_rpt, brst_rpt$age == "nestling", 
      #                 datatype = "Gaussian", nboot = 1000, npermut = 0))
      # r_ba_ne <- rpt(value ~ (1|band), grname = "band", data = subset(back_rpt, back_rpt$age == "nestling", 
      #                 datatype = "Gaussian", nboot = 1000, npermut = 0))
      
      # saveRDS(r_br_ne, here("5_saved_objects/r_br_ne.rds"))
      # saveRDS(r_ba_ne, here("5_saved_objects/r_ba_ne.rds"))
      
  # Repeatability for multiple observers
      # do_rpt <- rpt(density ~ (1|feather_id), grname = "feather_id", data = do, datatype = "Gaussian", nboot = 1000, npermut =)
      # 
      # saveRDS(do_rpt, here("5_saved_objects/do_rpt.rds"))
      
  # Read these results back in. These are produced by code above but I've saved as rds because they take a while to run
      r_br_all <- readRDS(here("5_saved_objects/r_br_all.rds"))
      r_ba_all <- readRDS(here("5_saved_objects/r_ba_all.rds"))
      r_br_ad <- readRDS(here("5_saved_objects/r_br_ad.rds"))
      r_ba_ad <- readRDS(here("5_saved_objects/r_ba_ad.rds"))
      r_br_ne <- readRDS(here("5_saved_objects/r_br_ne.rds"))
      r_ba_ne <- readRDS(here("5_saved_objects/r_ba_ne.rds"))
      do_rpt <- readRDS(here("5_saved_objects/do_rpt.rds"))

# Back vs. breast ----
    
    p1 <- subset(di, di$age == "nestling" | di$age == "adult") %>%
    ggplot(mapping = aes(x = breast_den, y = back_den, fill = age)) + geom_point(pch = 21, alpha = 0.7) +
      theme_classic() + scale_fill_manual(values = c(adult = a_color, nestling = n_color)) + 
      geom_smooth(method = "lm", color = "black") + xlab("Breast barbs per cm") +
      ylab("Back barbs per cm") +
      theme(legend.position = c(0.85, 0.1))+
      theme(axis.title = element_text(size = 16))
    
    ggsave(here::here("3_r_scripts/breast_vs_rump.png"), p1, device = "png", width = 5, height = 5)
    
    
    p1 <- subset(di, di$age == "nestling" | di$age == "adult") %>%
      ggplot(mapping = aes(x = breast_den, y = back_den, colour = age, by = age)) + 
      geom_point(alpha = 0.7) +
      theme_classic() + scale_color_manual(values = c(adult = a_color, nestling = n_color)) + 
      geom_smooth(method = "lm", aes(fill = age), color = "black") + xlab("Breast barbs per cm") +
      ylab("Back barbs per cm") +
      theme(legend.position = c(0.85, 0.13)) +
      scale_fill_manual(values = c(adult = a_color, nestling = n_color)) +
      theme(axis.title = element_text(size = 16))
    ggMarginal(p1, type = "boxplot", groupFill = TRUE, bw = .7)
    
    ggsave(here::here("3_r_scripts/figure_2.png"), 
           ggMarginal(p1, type = "boxplot", groupFill = TRUE, bw = .7), device = "png", width = 5, height = 5)

# Back vs. breast stats ----
    
    cor.test(di$back_den, di$breast_den, use = "complete.obs")
    di_a <- subset(di, di$age == "adult")
    cor.test(di_a$back_den, di_a$breast_den, use = "complete.obs")
    di_n <- subset(di, di$age == "nestling")
    cor.test(di_n$back_den, di_n$breast_den, use = "complete.obs")
    
    m <- lmer(scale(back_den) ~ scale(breast_den)*age + (1|soc_uby), data = di)
    
    mn <- lmer(back_den ~ breast_den + (1|soc_uby) + (1|gen_mom), data = di_n)
    
    m_back <- lmer(back_den ~ age + (1|soc_uby), data = di)
    m_breast <- lmer(breast_den ~ age + (1|soc_uby), data = di)
        
# Fledging age by feathers ----
    
    p1 <- di %>%
      filter(age == "nestling", fled_age > 17) %>%  # the few at 17 days seem to be rfid record problems
      ggplot(mapping = aes(x = fled_age, y = breast_den, color = trt_group, fill = trt_group)) + 
      geom_point(pch = 21, size = 2, color = "gray30") +
      theme_classic() + xlab("Fledge Age (days)") +
      ylab("Breast Barbs Per cm") +
      geom_smooth(method = "lm", mapping = aes(fill = trt_group), color = "gray30") +
      guides(fill = FALSE, color = FALSE) +
      theme(legend.position = c(0.86, 0.14)) +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5) +
      scale_fill_viridis(discrete = TRUE) +
      theme(axis.title = element_text(size = 16))
      #ggMarginal(p1, type = "violin", groupFill = TRUE, bw = .7)
      
    p2 <- di %>%
      filter(age == "nestling", fled_age > 17) %>%  # the few at 17 days seem to be rfid record problems
      ggplot(mapping = aes(x = fled_age, y = back_den, color = trt_group, fill = trt_group)) + 
      geom_point(pch = 21, size = 2, color = "gray30") +
      theme_classic() + xlab("Fledge Age (days)") +
      ylab("Rump Barbs Per cm") +
      geom_smooth(method = "lm", mapping = aes(fill = trt_group), color = "gray30") +
      guides(fill = guide_legend(title = "Treatment"), color = guide_legend(title = "Treatment")) +
      theme(legend.position = c(0.86, 0.14)) +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5) +
      scale_fill_viridis(discrete = TRUE) +
      theme(axis.title = element_text(size = 16))
      #ggMarginal(p2, type = "density", groupFill = TRUE, bw = .7)
    
    ggsave(here::here("3_r_scripts/figure_3.png"),   
      ggarrange(p1, p2),
      device = "png", width = 11, height = 5.6)
    
  # Model
    di_n <- subset(di, di$age == "nestling")
    di_n$Nest <- di_n$soc_uby
    di_n$Mother <- di_n$gen_mom
    m_full <- lmer(fled_age ~ back_den + breast_den + trt_group + back_den*trt_group + breast_den*trt_group + (1|Nest) + (1|Mother), data = di_n)
    m_red <- lmer(fled_age ~ back_den + breast_den + trt_group + (1|Nest) + (1|Mother), data = di_n)
    
    tab_model(m_full, m_red,
              dv.labels = c("Fledge Age (Days)", "Fledge Age (Days)"),
              pred.labels = c("Intercept", "Back Barb Density", "Breast Barb Density", "Handicap", "Predator", 
                              "Back Barb * Handicap", "Back Barb * Predator", "Breast Barb * Handicap", "Breast Barb * Predator"))
    
    tab_model(m_red,
              dv.labels = "Fledge Age (Days)",
              pred.labels = c("Intercept (Control)", "Back Barb Density", "Breast Barb Density", "Handicap", "Predator"))


# Nestling barb by morphology ----
   
  # Figures (only showing plots based on models)   
    di_n <- subset(di, di$age == "nestling")
    
    p1 <- ggplot(data = di_n, mapping = aes(x = breast_den, y = d12_mass, fill = trt_group)) + 
      geom_point(alpha = 0.8, pch = 21) + geom_smooth(method = "lm", aes(fill = trt_group), color = "gray30") +
      theme_classic() + xlab("Breast barbs per cm") + ylab("Mass on day 12 (grams)") +
      guides(color = FALSE, fill = FALSE) +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5) +
      scale_fill_viridis(discrete = TRUE) +
      theme(axis.title = element_text(size = 16))
    
    p2 <- ggplot(data = di_n, mapping = aes(x = breast_den, y = d12_wing, fill = trt_group)) + 
      geom_point(alpha = 0.8, pch = 21) + geom_smooth(method = "lm", aes(fill = trt_group), color = "gray30") +
      theme_classic() + xlab("Breast barbs per cm") + ylab("Wing length on day 12 (mm)") +
      guides(color = FALSE, fill = FALSE) +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5) +
      scale_fill_viridis(discrete = TRUE) +
      theme(axis.title = element_text(size = 16))
     
    p3 <- ggplot(data = di_n, mapping = aes(x = breast_den, y = d15_mass, fill = trt_group)) + 
      geom_point(alpha = 0.8, pch = 21) + geom_smooth(method = "lm", aes(fill = trt_group), color = "gray30") +
      theme_classic() + xlab("Breast barbs per cm") + ylab("Mass on day 15 (grams)") +
      guides(color = guide_legend(title = "Treatment"), fill = guide_legend(title = "Treatment")) +
      annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5) + 
      theme(legend.position = c(x = 0.84, y = 0.17)) +
      scale_fill_viridis(discrete = TRUE) +
      theme(axis.title = element_text(size = 16))
     
    ggsave(here("3_r_scripts/figure_4.png"),
      ggarrange(p1, p2, p3, nrow = 1),
      device = "png",
      width = 11.3, height = 4)
    
   # models 
      di_n$Nest <- di_n$soc_uby
      di_n$Mother <- di_n$gen_mom
      m <- lmer(d12_mass ~ back_den + breast_den*trt_group + (1|Nest) + (1|Mother), data = di_n)
      tab_model(m)
      m2 <- lmer(d12_head ~ back_den + trt_group + breast_den + (1|Nest) + (1|Mother), data = di_n)
      tab_model(m2)
      m3 <- lmer(d12_wing ~ back_den + breast_den*trt_group + (1|Nest) + (1|Mother), data = di_n)
      tab_model(m3)
      m4 <- lmer(d15_mass ~ back_den + breast_den*trt_group + (1|Nest) + (1|Mother), data = di_n)
      tab_model(m4)
      
      tab_model(m, m2, m3, m4, 
                dv.labels = c("Day 12 Mass", "Day 12 Head + Bill", "Day 12 Flat Wing", "Day 15 Mass"),
                pred.labels = c("Intercept", "Back Barb Density", "Breast Barb Density", "Handicap", "Predator", 
                                "Breast Barb * Handicap", "Breast Barb * Predator")
                )
    
    
# ICC for social nest and mom ----
    
    rpt(breast_den ~ 1 + (1|gen_mom) + (1|soc_uby), grname = c("soc_uby", "gen_mom"), 
        data = di_n, datatype = "Gaussian", nboot = 1000, npermut = 0)
    rpt(back_den ~ 1 + (1|gen_mom) + (1|soc_uby), grname = c("soc_uby", "gen_mom"), 
        data = di_n, datatype = "Gaussian", nboot = 1000, npermut = 0)
    
    rpt(breast_den ~ 1 + (1|gen_mom), grname = "gen_mom", 
        data = di_n, datatype = "Gaussian", nboot = 1000, npermut = 0)
    rpt(back_den ~ 1 + (1|gen_mom), grname = "gen_mom", 
        data = di_n, datatype = "Gaussian", nboot = 1000, npermut = 0)
    
    rpt(breast_den ~ 1 + (1|soc_uby), grname = "soc_uby", 
        data = di_n, datatype = "Gaussian", nboot = 1000, npermut = 0)
    rpt(back_den ~ 1 + (1|soc_uby), grname = "soc_uby", 
        data = di_n, datatype = "Gaussian", nboot = 1000, npermut = 0)

    
# Barbs by fledging and treatment ----
    fate <- subset(di_n, di_n$fate != "Unknown")
    fated <- data.frame(fate = c("Died", "Fledged"), nfate = c(0, 1))    
    fate <- plyr::join(fate, fated, "fate")    
    m <- glmer(nfate ~ scale(breast_den) + scale(back_den) + trt_group + (1|soc_uby), 
               data = fate, family = "binomial")    
    m1 <- glmer(nfate ~ trt_group + (1|soc_uby), data = fate, family = "binomial")
    
# Provisioning ----
  # Analyze provisioning rate in relation to feather barbs
    dp <- plyr::join(dp, dnest, "uby", "left", "first")
    dp$doy_y <- paste(dp$doy, dp$year, sep = "_")
    di_a <- di[, c("band", "back_den", "breast_den")]
    colnames(di_a)[1] <- "female"
    dp <- plyr::join(dp, di_a, "female")
    mx <- lmer(f_feed ~ d6_brood + offset + I(offset ^ 2) + (1|uby), data = dp)
    rr <- data.frame(soc_uby = rownames(ranef(mx)$uby), blup = ranef(mx)$uby[, 1],
                     s_prov = mean(na.omit(dp$d6_brood))*fixef(mx)[2] + ranef(mx)$uby[, 1] +
                       fixef(mx)[1] + fixef(mx)[3]*12 + fixef(mx)[4]*144)
    
    di_n <- subset(di, di$age == "nestling")
    di_np <- plyr::join(di_n, rr, "soc_uby")
    
    m <- lmer(breast_den ~ scale(s_prov) + (1|soc_uby) + (1|gen_mom), data = di_np)
    m2 <- lmer(back_den ~ scale(s_prov) + (1|soc_uby) + (1|gen_mom), data = di_np)
    
    tab_model(m, m2)
    
    pp <- ggplot(data = di_np, mapping = aes(x = scale(s_prov), y = breast_den)) + 
      theme_classic() + geom_point(color = n_color) + geom_smooth(method = "lm", fill = n_color) +
      xlab("Female provisioning rate (sd)") + ylab("Breast barbs per cm") +
      theme(axis.title = element_text(size = 16))
    
    ggsave(here::here("3_r_scripts/figure_5.png"),
           pp, device = "png", width = 4, height = 4)
    
    
    # Repeat above using total provisioning (male + female) for subset with male data
      dpm <- subset(dp, is.na(dp$m_feed) == FALSE)
      dpm$t_feed <- dpm$f_feed + dpm$m_feed
      
      mx <- lmer(t_feed ~ d6_brood + offset + I(offset ^ 2) + (1|uby), data = dpm)
      rr <- data.frame(soc_uby = rownames(ranef(mx)$uby), blup = ranef(mx)$uby[, 1],
                       s_prov = mean(na.omit(dpm$d6_brood))*fixef(mx)[2] + ranef(mx)$uby[, 1] +
                         fixef(mx)[1] + fixef(mx)[3]*12 + fixef(mx)[4]*144)
      
      di_n <- subset(di, di$age == "nestling")
      di_np <- plyr::join(di_n, rr, "soc_uby")
      
      m <- lmer(breast_den ~ scale(s_prov) + (1|soc_uby) + (1|gen_mom), data = di_np)
      m2 <- lmer(back_den ~ scale(s_prov) + (1|soc_uby) + (1|gen_mom), data = di_np)
      
      tab_model(m, m2)
      
      pp2 <- ggplot(data = di_np, mapping = aes(x = scale(s_prov), y = breast_den)) + 
        theme_classic() + geom_point(color = n_color) + geom_smooth(method = "lm", fill = n_color) +
        xlab("Combined provisioning rate (sd)") + ylab("Breast barbs per cm") +
        theme(axis.title = element_text(size = 16))
    
## NOT INCLUDED IN PAPER BELOW HERE ----

# Plot multiple feather comparison ----
    
    # This plot is not included in the paper, instead repeatability is reported    
    p1 <- subset(di, di$age == "nestling" | di$age == "adult") %>%
      ggplot(mapping = aes(x = br1_den, y = br2_den, fill = age)) + geom_point(pch = 21, alpha = 0.7) +
      theme_classic() + 
      scale_fill_manual(values = c(adult = a_color, nestling = n_color)) + 
      geom_smooth(method = "lm", color = "black") + 
      guides(color = FALSE) + xlab("Breast barbs per cm feather A") +
      theme(legend.position = c(0.86, 0.14)) +
      ylab("Breast barbs per cm feather B") + xlim(c(10, 47)) + ylim(c(10, 47)) +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5)
    
    p2 <- subset(di, di$age == "nestling" | di$age == "adult") %>%
      ggplot(mapping = aes(x = back1_den, y = back2_den, fill = age)) + geom_point(pch = 21, alpha = 0.7) +
      theme_classic() + 
      scale_fill_manual(values = c(adult = a_color, nestling = n_color)) + 
      geom_smooth(method = "lm", color = "black") + 
      guides(color = FALSE) + xlab("Back barbs per cm feather A") +
      theme(legend.position = c(0.86, 0.14)) +
      ylab("Back barbs per cm feather B") + xlim(c(9, 43)) + ylim(c(9, 43)) +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5)
    
    ggsave(here::here("3_r_scripts/two_feather_comparison.png"), 
           ggarrange(p1, p2), device = "png", width = 9.1, height = 4.6)
    
# Plot multiple observer comparison ----
    
    # This plot not included in paper, instead repeatability is reported
    do2 <- do
    colnames(do2)[2:3] <- c("observer_2", "density_2")
    do3 <- plyr::join(do, do2, "feather_id", match = "all")
    do3 <- subset(do3, do3$observer != do3$observer_2)
    do3$obs_pair <- paste(do3$observer, do3$observer_2, sep = "_")
    do3$sum <- do3$density + do3$density_2
    do3 <- do3[!duplicated(do3$sum), ]
    p1 <- ggplot(data = do3, mapping = aes(x = density, y = density_2)) +
      geom_point(pch = 21, alpha = 0.7, mapping = aes(fill = obs_pair)) + theme_classic() +
      guides(fill = guide_legend(title = "Observer Pair")) +
      geom_smooth(method = "lm") + xlab("Observer 1 Feather Barbs Per cm") +
      ylab("Observer 2 Feather Barbs Per cm") + guides(fill = FALSE) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      xlim(c(14, 40)) + ylim(c(14, 40))
    ggsave(here::here("3_r_scripts/two_observer_comp.png"),
           p1, device = "png", width = 5, height = 5)
    
    
# Plot nestling vs. adult comparison ----
    
    # This plot is not included in paper. Subsequent plot shows the same information.       
    mu_nb <- mean(na.omit(subset(di$breast_den, di$age == "nestling")))
    mu_ab <- mean(na.omit(subset(di$breast_den, di$age == "adult")))
    sd_nb <- sd(na.omit(subset(di$breast_den, di$age == "nestling"))) 
    sd_ab <- sd(na.omit(subset(di$breast_den, di$age == "adult"))) 
    
    p1 <- subset(di, di$age == "nestling" | di$age == "adult") %>%
      ggplot(mapping = aes(x = breast_den, fill = age)) + geom_density(alpha = 0.4) + theme_classic() +
      scale_fill_manual(values = c(adult = a_color, nestling = n_color)) + 
      scale_color_manual(values = c(adult = a_color, nestling = n_color)) +
      geom_rug(mapping = aes(col = age)) +
      xlab("Breast Barbs Per cm") + ylab("Density") +
      geom_line(data = data.frame(x = c(mu_nb - sd_nb, mu_nb + sd_nb), y = rep(0.18, 2), age = c("nestling", "nestling")), 
                mapping = aes(x = x, y = y), size = 1) +
      geom_line(data = data.frame(x = c(mu_ab - sd_ab, mu_ab + sd_ab), y = rep(0.17, 2), age = c("adult", "adult")), 
                mapping = aes(x = x, y = y), size = 1) +
      geom_point(data = data.frame(x = c(mu_nb, mu_ab), y = c(0.18, 0.17), age = c("nestling", "adult")), 
                 mapping = aes(x = x, y = y), pch = 21, size = 3) +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5) +
      theme(legend.position = c(0.8, 0.5))
    
    mu_nb <- mean(na.omit(subset(di$back_den, di$age == "nestling")))
    mu_ab <- mean(na.omit(subset(di$back_den, di$age == "adult")))
    sd_nb <- sd(na.omit(subset(di$back_den, di$age == "nestling"))) 
    sd_ab <- sd(na.omit(subset(di$back_den, di$age == "adult"))) 
    
    p2 <- subset(di, di$age == "nestling" | di$age == "adult") %>%
      ggplot(mapping = aes(x = back_den, fill = age)) + geom_density(alpha = 0.4) + theme_classic() +
      scale_fill_manual(values = c(adult = a_color, nestling = n_color)) + 
      scale_color_manual(values = c(adult = a_color, nestling = n_color)) +
      geom_rug(mapping = aes(col = age)) +
      xlab("Rump Barbs Per cm") + ylab("Density") +
      geom_line(data = data.frame(x = c(mu_nb - sd_nb, mu_nb + sd_nb), y = rep(0.18, 2), age = c("nestling", "nestling")), 
                mapping = aes(x = x, y = y), size = 1) +
      geom_line(data = data.frame(x = c(mu_ab - sd_ab, mu_ab + sd_ab), y = rep(0.17, 2), age = c("adult", "adult")), 
                mapping = aes(x = x, y = y), size = 1) +
      geom_point(data = data.frame(x = c(mu_nb, mu_ab), y = c(0.18, 0.17), age = c("nestling", "adult")), 
                 mapping = aes(x = x, y = y), pch = 21, size = 3) +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5) +
      theme(legend.position = c(0.8, 0.5))
    
    ggsave(here::here("3_r_scripts/adult_vs_nestling.png"), 
           ggarrange(p1, p2), device = "png", width = 9.1, height = 4.6)
    
# Nestling pairwise similarity ----
    # These plots are not included in the paper
    n_sim <- subset(di, di$age == "nestling" & di$parents_knwn == "yes")
    n_sim <- n_sim[, c("band", "soc_uby", "breast_den", "back_den", "gen_mom", "gen_dad", "raised_nest", "year")]
    
    pair_df <- expand.grid(n_sim$band, n_sim$band)
    colnames(pair_df) <- c("n_a", "n_b")
    
    colnames(n_sim) <- c("n_a", "soc_uby_a", "breast_den_a", "back_den_a", "gen_mom_a", "gen_dad_a", "raised_nest_a", "year_a")
    pair_df <- plyr::join(pair_df, n_sim, "n_a", "left", "first")
    colnames(n_sim) <- c("n_b", "soc_uby_b", "breast_den_b", "back_den_b", "gen_mom_b", "gen_dad_b", "raised_nest_b", "year_b")
    pair_df <- plyr::join(pair_df, n_sim, "n_b", "left", "first")
    
    pair_df <- subset(pair_df, pair_df$year_a == pair_df$year_b)
    pair_df <- subset(pair_df, pair_df$n_b != pair_df$n_a)
    
    pair_df$same_mom <- rep(0, nrow(pair_df))
    pair_df$same_dad <- rep(0, nrow(pair_df))
    pair_df$same_box <- rep("different", nrow(pair_df))
    
    for(i in 1:nrow(pair_df)){
      if(pair_df$gen_mom_a[i] == pair_df$gen_mom_b[i]){
        pair_df$same_mom[i] <- 1
      }
      if(pair_df$gen_dad_a[i] == pair_df$gen_dad_b[i]){
        pair_df$same_dad[i] <- 1
      }
      if(pair_df$soc_uby_a[i] == pair_df$soc_uby_b[i]){
        pair_df$same_box[i] <- "same"
      }
    }
    
    pair_df$relation <- as.factor(pair_df$same_mom + pair_df$same_dad)
    
    pair_df$br_diff <- abs(pair_df$breast_den_a - pair_df$breast_den_b)
    pair_df$ba_diff <- abs(pair_df$back_den_a - pair_df$back_den_b)
    
    #ggplot(data = pair_df) + geom_density(aes(x = br_diff), fill = "coral3", alpha = 0.5) + 
    #  facet_wrap(~ same_box + relation, nrow = 2) + theme_classic()
    
    pair_df$group <- paste(pair_df$same_box, pair_df$relation, sep = "_")
    
    p1 <- ggplot(data = pair_df) + 
      geom_density(aes(x = br_diff, fill = group), alpha = 0.4) + 
      theme_classic() + xlab("Breast Barb Abs. Difference cm") +
      ylab("Density") + theme(legend.position = c(0.85, 0.5)) +
      annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5)
    p2 <- ggplot(data = pair_df, mapping = aes(x = relation, y = br_diff)) + theme_classic() +
      geom_jitter(alpha = 0.04, width = 0.1) + 
      geom_boxplot(fill = "coral3", alpha = 0.3, outlier.shape = NA) +
      xlab("Relationship") + ylab("Breast Barb Abs. Difference cm") +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5)
    p3 <- ggplot(data = pair_df, mapping = aes(x = same_box, y = br_diff)) + theme_classic() +
      geom_jitter(alpha = 0.04, width = 0.1) +
      geom_boxplot(fill = "coral3", alpha = 0.3, outlier.shape = NA) +
      xlab("Nest Raised") + ylab("Breast Barb Abs. Difference cm") +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5)
    
    ggsave(here::here("3_r_scripts/breast_pairwise.png"),
           grid.arrange(p1, p2, p3, layout_matrix = rbind(c(2, 1, 1), c(3, 1, 1))),
           device = "png", width = 8.5, height = 5.3)
    
    
    p1 <- ggplot(data = pair_df) + 
      geom_density(aes(x = ba_diff, fill = group), alpha = 0.4) + 
      theme_classic() + xlab("Rump Barb Abs. Difference cm") +
      ylab("Density") + theme(legend.position = c(0.85, 0.5)) +
      annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5)
    p2 <- ggplot(data = pair_df, mapping = aes(x = relation, y = ba_diff)) + theme_classic() +
      geom_jitter(alpha = 0.04, width = 0.1) + 
      geom_boxplot(fill = "coral3", alpha = 0.3, outlier.shape = NA) +
      xlab("Relationship") + ylab("Rump Barb Abs. Difference cm") +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5)
    p3 <- ggplot(data = pair_df, mapping = aes(x = same_box, y = ba_diff)) + theme_classic() +
      geom_jitter(alpha = 0.04, width = 0.1) +
      geom_boxplot(fill = "coral3", alpha = 0.3, outlier.shape = NA) +
      xlab("Nest Raised") + ylab("Rump Barb Abs. Difference cm") +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5)
    
    ggsave(here::here("3_r_scripts/rump_pairwise.png"),
           grid.arrange(p1, p2, p3, layout_matrix = rbind(c(2, 1, 1), c(3, 1, 1))),
           device = "png", width = 8.5, height = 5.3)
    
    
    
# Nestling by genetic parent ----
    #Not included in the paper
    dia <- subset(di, di$age == "adult")
    din <- subset(di, di$age == "nestling" & di$parents_knwn == "yes")
    dim <- subset(dia, dia$sex == "male")
    dim <- dim[, c("band", "breast_den", "back_den")]
    colnames(dim) <- c("gen_dad" ,"breast_den_m", "back_den_m")
    dif <- subset(dia, dia$sex == "female")
    dif <- dif[, c("band", "breast_den", "back_den")]
    colnames(dif) <- c("gen_mom", "breast_den_f", "back_den_f")
    
    din <- plyr::join(din, dim, "gen_dad")
    din <- plyr::join(din, dif, "gen_mom")
    
    for(i in 1:nrow(din)){
      din$breast_midp[i] <- mean(c(din$breast_den_m[i], din$breast_den_f[i]))
      din$back_midp[i] <- mean(c(din$back_den_m[i], din$back_den_f[i]))
    }
    
    p3 <- ggplot(data = din, mapping = aes(x = breast_midp, y = breast_den, fill = "coral3")) +
      geom_point(pch = 21) + theme_classic() + guides(fill = FALSE) +
      xlab("Genetic Midparent Breast Barbs Per cm") + ylab("Nestling Breast Barbs Per cm") +
      geom_smooth(method = "lm", fill = "slateblue", color = "gray30") +
      annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5)
    p1 <- ggplot(data = din, mapping = aes(x = breast_den_f, y = breast_den, fill = "coral3")) +
      geom_point(pch = 21) + theme_classic() + guides(fill = FALSE) +
      xlab("Mother") + ylab("Nestling") +
      geom_smooth(method = "lm", fill = "slateblue", color = "gray30") +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5)
    p2 <- ggplot(data = din, mapping = aes(x = breast_den_m, y = breast_den, fill = "coral3")) +
      geom_point(pch = 21) + theme_classic() + guides(fill = FALSE) +
      xlab("Father") + ylab("Nestling") +
      geom_smooth(method = "lm", fill = "slateblue", color = "gray30") +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5)
    p6 <- ggplot(data = din, mapping = aes(x = back_midp, y = breast_den, fill = "coral3")) +
      geom_point(pch = 21) + theme_classic() + guides(fill = FALSE) +
      xlab("Genetic Midparent Rump Barbs Per cm") + ylab("Nestling Rump Barbs Per cm") +
      geom_smooth(method = "lm", fill = "slateblue", color = "gray30") +
      annotate("text", x = -Inf, y = Inf, label = "E", hjust = -0.5, vjust = 1.5)
    p4 <- ggplot(data = din, mapping = aes(x = back_den_f, y = breast_den, fill = "coral3")) +
      geom_point(pch = 21) + theme_classic() + guides(fill = FALSE) +
      xlab("Mother") + ylab("Nestling") +
      geom_smooth(method = "lm", fill = "slateblue", color = "gray30") +
      annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5)
    p5 <- ggplot(data = din, mapping = aes(x = back_den_m, y = breast_den, fill = "coral3")) +
      geom_point(pch = 21) + theme_classic() + guides(fill = FALSE) +
      xlab("Father") + ylab("Nestling") +
      geom_smooth(method = "lm", fill = "slateblue", color = "gray30") +
      annotate("text", x = -Inf, y = Inf, label = "D", hjust = -0.5, vjust = 1.5)
    
    ggsave(here::here("3_r_scripts/breast_midparent.png"),
           grid.arrange(p1, p2, p3, p4, p5, p6, layout_matrix = 
                          rbind(c(1, 1, 2, 2, 4, 4, 5, 5), 
                                c(1, 1, 2, 2, 4, 4, 5, 5),
                                c(3, 3, 3, 3, 6, 6, 6, 6),
                                c(3, 3, 3, 3, 6, 6, 6, 6),
                                c(3, 3, 3, 3, 6, 6, 6, 6),
                                c(3, 3, 3, 3, 6, 6, 6, 6))),
           device = "png", width = 10.2, height = 7.4)
    
    
    ######## social parents
    
    dia <- subset(di, di$age == "adult")
    din <- subset(di, di$age == "nestling")
    dim <- subset(dia, dia$sex == "male")
    dim <- dim[, c("band", "breast_den", "back_den")]
    colnames(dim) <- c("soc_dad_raised" ,"breast_den_m", "back_den_m")
    dif <- subset(dia, dia$sex == "female")
    dif <- dif[, c("band", "breast_den", "back_den")]
    colnames(dif) <- c("soc_mom", "breast_den_f", "back_den_f")
    
    din <- plyr::join(din, dim, "soc_dad_raised")
    din <- plyr::join(din, dif, "soc_mom")
    
    for(i in 1:nrow(din)){
      din$breast_midp[i] <- mean(c(din$breast_den_m[i], din$breast_den_f[i]))
      din$back_midp[i] <- mean(c(din$back_den_m[i], din$back_den_f[i]))
    }
    
    p3 <- ggplot(data = din, mapping = aes(x = breast_midp, y = breast_den, fill = "coral3")) +
      geom_point(pch = 21) + theme_classic() + guides(fill = FALSE) +
      xlab("Genetic Midparent Breast Barbs Per cm") + ylab("Nestling Breast Barbs Per cm") +
      geom_smooth(method = "lm", fill = "slateblue", color = "gray30") +
      annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5)
    p1 <- ggplot(data = din, mapping = aes(x = breast_den_f, y = breast_den, fill = "coral3")) +
      geom_point(pch = 21) + theme_classic() + guides(fill = FALSE) +
      xlab("Mother") + ylab("Nestling") +
      geom_smooth(method = "lm", fill = "slateblue", color = "gray30") +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5)
    p2 <- ggplot(data = din, mapping = aes(x = breast_den_m, y = breast_den, fill = "coral3")) +
      geom_point(pch = 21) + theme_classic() + guides(fill = FALSE) +
      xlab("Father") + ylab("Nestling") +
      geom_smooth(method = "lm", fill = "slateblue", color = "gray30") +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5)
    p6 <- ggplot(data = din, mapping = aes(x = back_midp, y = breast_den, fill = "coral3")) +
      geom_point(pch = 21) + theme_classic() + guides(fill = FALSE) +
      xlab("Genetic Midparent Rump Barbs Per cm") + ylab("Nestling Rump Barbs Per cm") +
      geom_smooth(method = "lm", fill = "slateblue", color = "gray30") +
      annotate("text", x = -Inf, y = Inf, label = "E", hjust = -0.5, vjust = 1.5)
    p4 <- ggplot(data = din, mapping = aes(x = back_den_f, y = breast_den, fill = "coral3")) +
      geom_point(pch = 21) + theme_classic() + guides(fill = FALSE) +
      xlab("Mother") + ylab("Nestling") +
      geom_smooth(method = "lm", fill = "slateblue", color = "gray30") +
      annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5)
    p5 <- ggplot(data = din, mapping = aes(x = back_den_m, y = breast_den, fill = "coral3")) +
      geom_point(pch = 21) + theme_classic() + guides(fill = FALSE) +
      xlab("Father") + ylab("Nestling") +
      geom_smooth(method = "lm", fill = "slateblue", color = "gray30") +
      annotate("text", x = -Inf, y = Inf, label = "D", hjust = -0.5, vjust = 1.5)
    
    
    ggsave(here::here("3_r_scripts/breast_soc_midparent.png"),
           grid.arrange(p1, p2, p3, p4, p5, p6, layout_matrix = 
                          rbind(c(1, 1, 2, 2, 4, 4, 5, 5), 
                                c(1, 1, 2, 2, 4, 4, 5, 5),
                                c(3, 3, 3, 3, 6, 6, 6, 6),
                                c(3, 3, 3, 3, 6, 6, 6, 6),
                                c(3, 3, 3, 3, 6, 6, 6, 6),
                                c(3, 3, 3, 3, 6, 6, 6, 6))),
           device = "png", width = 10.2, height = 7.4)
    
    
# Social nest by barb density ----
    # Not included in paper 
    ggplot(data = di_n, mapping = aes(x = reorder(soc_uby, breast_den, mean, na.rm = TRUE), y = breast_den)) + 
      geom_boxplot(fill = "coral3") + theme_bw() + xlab("Nest") + ylab("Breast barbs per cm") +
      theme(axis.text.x = element_blank()) + geom_point(col = "gray30") +
      geom_boxplot(mapping = aes(x = reorder(soc_uby, breast_den, mean, na.rm = TRUE), y = back_den), fill = "slateblue")
    
    ggplot(data = di_n, mapping = aes(x = reorder(soc_uby, back_den, mean, na.rm = TRUE), y = back_den)) + 
      geom_boxplot(fill = "coral3") + theme_bw() + xlab("Nest") + ylab("Back barbs per cm") +
      theme(axis.text.x = element_blank()) + geom_point(col = "gray30")
    
    di_n %>%
      group_by(soc_uby) %>%
      summarize(br_mu = mean(breast_den, na.rm = TRUE), ba_mu = mean(back_den, na.rm = TRUE), 
                br_sd = sd(breast_den, na.rm = TRUE), ba_sd = sd(back_den, na.rm = TRUE)) %>%
      ggplot(mapping = aes(x = br_mu, y = ba_mu)) + 
      geom_pointrange(aes(xmin = br_mu - br_sd, xmax = br_mu + br_sd), col = "gray20") +
      geom_pointrange(aes(ymin = ba_mu - ba_sd, ymax = ba_mu + ba_sd), col = "gray20") +
      geom_point(pch = 21, fill = "slateblue") +
      geom_smooth(method = "lm", fill = "coral3", color = "gray30") +
      theme_classic() 
    
    
    di_n2 <- pivot_longer(di_n, cols = c("breast_den", "back_den"), names_to = "region", values_to = "density")
    
    m1 <- lmer(breast_den ~ 1 + (1|soc_uby) , data = di_n, REML = FALSE)
    m2 <- lmer(breast_den ~ 1 + (1|gen_mom) , data = di_n, REML = FALSE)
    m3 <- lmer(breast_den ~ 1 + (1|soc_uby) + (1|gen_mom), data = di_n, REML = FALSE)
    mn1 <- lm(breast_den ~ 1, data = di_n, REML = FALSE)
    
    model.sel(m1, m2, m3, mn1)
    
    m4 <- lmer(back_den ~ 1 + (1|soc_uby), data = di_n, REML = FALSE)
    m5 <- lmer(back_den ~ 1 + (1|gen_mom), data = di_n, REML = FALSE)
    m6 <- lmer(back_den ~ 1 + (1|soc_uby) + (1|gen_mom), data = di_n, REML = FALSE)
    mn2 <- lm(back_den ~ 1, data = di_n, REML = FALSE)
    
    model.sel(m4, m5, m6, mn2)