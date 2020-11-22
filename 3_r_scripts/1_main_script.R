# Analysis of nestling tree swallow feather quality in 2018 & 2019
# Written by Conor Taff
# Last updated 9/14/2020
# Run under R Studio 1.1.463 R v4.0.2 on Mac OSX 10.11.6


# Load packages ----
  pacman::p_load(tidyverse, here, lme4, scales, rptR, dabestr, ggpubr, grid, gridExtra)

# Load data ----
  di <- read.delim(here::here("1_raw_data/data_by_individual.txt"))
  do <- read.delim(here::here("1_raw_data/data_by_observer.txt"))
  dp <- read.delim(here::here("1_raw_data/data_for_provisioning.txt"))
  dnest <- read.delim(here::here("1_raw_data/data_by_nest.txt"))
  
  di$full_treatment <- paste(di$treat1, di$treat2, sep = "_")
  
  trt_group <- data.frame(full_treatment = c("_", "Control_Control", "Control_Dull", "Control_Predator",
                                             "Control_Tape", "Dull_Control", "Dull_Predator", "Dull_Tape",
                                             "Predator_Control", "Predator_Dull"),
                          trt_group = c("Control", "Control", "Control", "Predator", "Handicap", "Control", 
                                        "Predator", "Handicap", "Predator", "Predator"))
  di <- plyr::join(di, trt_group, "full_treatment")
  
  callan <- read.delim(here::here("1_raw_data/callan_et_al_data.txt"))
 
# Set options ----
  # Plotting and other options that will be used throughout the code below
  
    n_color <- "#56B4E9" # main color for nestlings
    a_color <- "#E69F00" # main color for adults
    
    p_color <- "#009E73" # color for predator treatment
    c_color <- "#0072B2" # color for control treatment
    s_color <- "#D55E00" # color for taping treatment

# Concept figure of tradeoffs ----
    
    for(i in 1:nrow(callan)){
      callan$tnest[i] <- strsplit(callan$time_nest[i], split = " ")[[1]][1]
    }
    p1 <- callan %>%
      filter(is.na(tnest) == FALSE) %>%
      ggplot(mapping = aes(x = as.numeric(tnest), y = plumage_qual, fill = "coral3")) + 
      geom_point(pch = 21) + geom_smooth(method = "lm", fill = "slateblue", col = "gray30") +
      theme_classic() + guides(fill = FALSE) + xlab("Time in nest") + ylab("Plumage quality") +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5) +
      annotate("text", 25, 0.5, label = "Callan et al. result", col = "slateblue")
    
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
    
    p2 <- ggplot(data = data.frame(x = c(sim$min, sim$max), y = c(sim$pos_y1, sim$pos_y2), sps = rep(sim$sps, 2)),
          mapping = aes(x = x, y = y, by = sps)) + geom_line(col = "gray40") +
          theme_classic() + xlab("Time In Nest") + ylab("Plumage Quality") +
          geom_point(data = sim, mapping = aes(x = tnest, y = plumage_qual), fill = "coral3", pch = 21) +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5) +
      annotate("text", x = 28, y = 0.5, label = "Positive", col = "slateblue")
    
    p3 <- ggplot(data = data.frame(x = c(sim$min, sim$max), y = c(sim$neg_y1, sim$neg_y2), sps = rep(sim$sps, 2)),
          mapping = aes(x = x, y = y, by = sps)) + geom_line(col = "gray40") +
          theme_classic() + xlab("Time In Nest") + ylab("Plumage Quality") +
          geom_point(data = sim, mapping = aes(x = tnest, y = plumage_qual), fill = "coral3", pch = 21) +
      annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5) +
      annotate("text", x = 28, y = 0.5, label = "Negative", col = "slateblue")
    
    p4 <- ggplot(data = data.frame(x = c(sim$min, sim$max), y = c(sim$no_y1, sim$no_y2), sps = rep(sim$sps, 2)),
          mapping = aes(x = x, y = y, by = sps)) + geom_line(col = "gray40") +
          theme_classic() + xlab("Time In Nest") + ylab("Plumage Quality") +
          geom_point(data = sim, mapping = aes(x = tnest, y = plumage_qual), fill = "coral3", pch = 21) +
      annotate("text", x = -Inf, y = Inf, label = "D", hjust = -0.5, vjust = 1.5) +
      annotate("text", x = 28, y = 0.5, label = "Absent", col = "slateblue")
    
    p5 <- ggplot(data = data.frame(x = c(sim$min, sim$max), y = c(sim$het_y1, sim$het_y2), sps = rep(sim$sps, 2)),
          mapping = aes(x = x, y = y, by = sps)) + geom_line(col = "gray40") +
          theme_classic() + xlab("Time In Nest") + ylab("Plumage Quality") +
          geom_point(data = sim, mapping = aes(x = tnest, y = plumage_qual), fill = "coral3", pch = 21) +
      annotate("text", x = -Inf, y = Inf, label = "E", hjust = -0.5, vjust = 1.5) +
      annotate("text", x = 26, y = 0.5, label = "Heterogeneous", col = "slateblue")
    
    ggsave(here::here("3_r_scripts/tradeoff_concept.png"),
           grid.arrange(p1, p2, p3, p4, p5, layout_matrix = 
                          rbind(c(1, 1, 1, 1, 2, 2, 3, 3), 
                                c(1, 1, 1, 1, 2, 2, 3, 3),
                                c(1, 1, 1, 1, 4, 4, 5, 5),
                                c(1, 1, 1, 1, 4, 4, 5, 5))),
           device = "png", width = 11.2, height = 5.9)
    
# Plot multiple feather comparison ----
    
    p1 <- subset(di, di$age == "nestling" | di$age == "adult") %>%
    ggplot(mapping = aes(x = br1_den, y = br2_den, fill = age)) + geom_point(pch = 21, alpha = 0.7) +
      theme_classic() + 
      scale_fill_manual(values = c(adult = a_color, nestling = n_color)) + 
      geom_smooth(method = "lm", color = "black") + 
      guides(color = FALSE) + xlab("Breast barbules per cm feather A") +
      theme(legend.position = c(0.86, 0.14)) +
      ylab("Breast barbules per cm feather B") + xlim(c(10, 47)) + ylim(c(10, 47)) +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5)
    
    p2 <- subset(di, di$age == "nestling" | di$age == "adult") %>%
    ggplot(mapping = aes(x = back1_den, y = back2_den, fill = age)) + geom_point(pch = 21, alpha = 0.7) +
      theme_classic() + 
      scale_fill_manual(values = c(adult = a_color, nestling = n_color)) + 
      geom_smooth(method = "lm", color = "black") + 
      guides(color = FALSE) + xlab("Back barbules per cm feather A") +
      theme(legend.position = c(0.86, 0.14)) +
      ylab("Back barbules per cm feather B") + xlim(c(9, 43)) + ylim(c(9, 43)) +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5)
    
    ggsave(here::here("3_r_scripts/two_feather_comparison.png"), 
      ggarrange(p1, p2), device = "png", width = 9.1, height = 4.6)
    
# Plot multiple observer comparison ----
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
    
# Calculate Measurement repeatability
    
    
# Plot nestling vs. adult comparison ----
    
    mu_nb <- mean(na.omit(subset(di$breast_den, di$age == "nestling")))
    mu_ab <- mean(na.omit(subset(di$breast_den, di$age == "adult")))
    sd_nb <- sd(na.omit(subset(di$breast_den, di$age == "nestling"))) 
    sd_ab <- sd(na.omit(subset(di$breast_den, di$age == "adult"))) 
    
    p1 <- subset(di, di$age == "nestling" | di$age == "adult") %>%
    ggplot(mapping = aes(x = breast_den, fill = age)) + geom_density(alpha = 0.4) + theme_classic() +
      scale_fill_manual(values = c(adult = a_color, nestling = n_color)) + 
      scale_color_manual(values = c(adult = a_color, nestling = n_color)) +
      geom_rug(mapping = aes(col = age)) +
      xlab("Breast Barbules Per cm") + ylab("Density") +
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
      xlab("Rump Barbules Per cm") + ylab("Density") +
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
    
# Back vs. breast ----
    
    p1 <- subset(di, di$age == "nestling" | di$age == "adult") %>%
    ggplot(mapping = aes(x = breast_den, y = back_den, fill = age)) + geom_point(pch = 21, alpha = 0.7) +
      theme_classic() + scale_fill_manual(values = c(adult = a_color, nestling = n_color)) + 
      geom_smooth(method = "lm", color = "black") + xlab("Breast barbules per cm") +
      ylab("Back barbules per cm") +
      theme(legend.position = c(0.85, 0.1))
    
    ggsave(here::here("3_r_scripts/breast_vs_rump.png"), p1, device = "png", width = 5, height = 5)
    
# Fledging age by feathers ----
    
    p1 <- di %>%
      filter(age == "nestling", fled_age > 17) %>%
      ggplot(mapping = aes(x = fled_age, y = breast_den, color = trt_group)) + 
      geom_point(pch = 21, size = 2, aes(fill = trt_group)) +
      theme_classic() + xlab("Fledge Age (days)") +
      ylab("Breast Barbules Per cm") +
      geom_smooth(method = "lm", mapping = aes(fill = trt_group), color = "gray30") +
      guides(fill = guide_legend(title = "Treatment"), color = guide_legend(title = "Treatment")) +
      theme(legend.position = c(0.86, 0.14)) +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5)
      #ggMarginal(p1, type = "density", groupFill = TRUE, bw = .7)
      
    p2 <- di %>%
      filter(age == "nestling", fled_age > 17) %>%
      ggplot(mapping = aes(x = fled_age, y = back_den, color = trt_group)) + 
      geom_point(pch = 21, size = 2, aes(fill = trt_group)) +
      theme_classic() + xlab("Fledge Age (days)") +
      ylab("Rump Barbules Per cm") +
      geom_smooth(method = "lm", mapping = aes(fill = trt_group), color = "gray30") +
      guides(fill = guide_legend(title = "Treatment"), color = guide_legend(title = "Treatment")) +
      theme(legend.position = c(0.86, 0.14)) +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5)
      #ggMarginal(p2, type = "density", groupFill = TRUE, bw = .7)
    
    ggsave(here::here("3_r_scripts/fled_vs_feather.png"),   
      ggarrange(ggMarginal(p1, type = "density", groupFill = TRUE, bw = .7), ggMarginal(p2, type = "density", groupFill = TRUE, bw = .7)),
      device = "png", width = 11, height = 5.6)

# Nestling pairwise similarity ----
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
      theme_classic() + xlab("Breast Barbule Abs. Difference cm") +
      ylab("Density") + theme(legend.position = c(0.85, 0.5)) +
      annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5)
    p2 <- ggplot(data = pair_df, mapping = aes(x = relation, y = br_diff)) + theme_classic() +
      geom_jitter(alpha = 0.04, width = 0.1) + 
      geom_boxplot(fill = "coral3", alpha = 0.3, outlier.shape = NA) +
      xlab("Relationship") + ylab("Breast Barbule Abs. Difference cm") +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5)
    p3 <- ggplot(data = pair_df, mapping = aes(x = same_box, y = br_diff)) + theme_classic() +
      geom_jitter(alpha = 0.04, width = 0.1) +
      geom_boxplot(fill = "coral3", alpha = 0.3, outlier.shape = NA) +
      xlab("Nest Raised") + ylab("Breast Barbule Abs. Difference cm") +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5)
    
    ggsave(here::here("3_r_scripts/breast_pairwise.png"),
      grid.arrange(p1, p2, p3, layout_matrix = rbind(c(2, 1, 1), c(3, 1, 1))),
      device = "png", width = 8.5, height = 5.3)
    
    
    p1 <- ggplot(data = pair_df) + 
      geom_density(aes(x = ba_diff, fill = group), alpha = 0.4) + 
      theme_classic() + xlab("Rump Barbule Abs. Difference cm") +
      ylab("Density") + theme(legend.position = c(0.85, 0.5)) +
      annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5)
    p2 <- ggplot(data = pair_df, mapping = aes(x = relation, y = ba_diff)) + theme_classic() +
      geom_jitter(alpha = 0.04, width = 0.1) + 
      geom_boxplot(fill = "coral3", alpha = 0.3, outlier.shape = NA) +
      xlab("Relationship") + ylab("Rump Barbule Abs. Difference cm") +
      annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5)
    p3 <- ggplot(data = pair_df, mapping = aes(x = same_box, y = ba_diff)) + theme_classic() +
      geom_jitter(alpha = 0.04, width = 0.1) +
      geom_boxplot(fill = "coral3", alpha = 0.3, outlier.shape = NA) +
      xlab("Nest Raised") + ylab("Rump Barbule Abs. Difference cm") +
      annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5)
    
    ggsave(here::here("3_r_scripts/rump_pairwise.png"),
           grid.arrange(p1, p2, p3, layout_matrix = rbind(c(2, 1, 1), c(3, 1, 1))),
           device = "png", width = 8.5, height = 5.3)
    
# Nestling by genetic parent ----
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
      xlab("Genetic Midparent Breast Barbules Per cm") + ylab("Nestling Breast Barbules Per cm") +
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
      xlab("Genetic Midparent Rump Barbules Per cm") + ylab("Nestling Rump Barbules Per cm") +
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
      xlab("Genetic Midparent Breast Barbules Per cm") + ylab("Nestling Breast Barbules Per cm") +
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
      xlab("Genetic Midparent Rump Barbules Per cm") + ylab("Nestling Rump Barbules Per cm") +
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
    
  