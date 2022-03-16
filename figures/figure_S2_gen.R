# Figure S4

source("0_settings.R")

load("data/ame_results.Rdata")
load("data/pred_results.Rdata")

# Further data setup

ols_pred_frac_ab <- rbind(ols_pred_frac_a, ols_pred_frac_b) # Combine results on A and B sample
ols_ame_frac_ab <- rbind(ols_ame_frac_a, ols_ame_frac_b)
ols_ame_frac_ab$type[ols_ame_frac_ab$type == "Senior researcher"] <- "B. Mid-career researcher" # Fix sample labels
ols_ame_frac_ab$type[ols_ame_frac_ab$type == "Early career researcher"] <- "A. Early career researcher"
ols_pred_frac_ab$type[ols_pred_frac_ab$type == "Senior researcher"] <- "D. Mid-career researcher"
ols_pred_frac_ab$type[ols_pred_frac_ab$type == "Early career researcher"] <- "C. Early career researcher"

# Average marginal effect on fractional publication counts by sample

ols_ame_frac_samples <- ggplot(data = ols_ame_frac_ab, aes(x = as.numeric(time_factor), y = AME, group = 1)) +
  geom_vline(xintercept = 4.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey50", alpha = 1/4) +
  geom_line(size = 1.5)+
  geom_point(shape = 21, fill = "white", size = 2.5, stroke = 2) +
  #geom_text(aes(x = 3.75 , y = 0.05, label = "Lock-down")) +
  scale_x_continuous(label = c("2016", "2017", "2018", "2019", "2020")) +
  scale_y_continuous(limits = c(-0.15, 0.05), breaks = c(-0.15, -0.1, -0.05, 0, 0.05)) +
  labs(x = "", y = "Avg. differences", title = "Differences in publication count") +
  facet_wrap(~ type) +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(hjust = 0.5))

# Average predicted fractional publication counts by sample

labels <- tibble(group = rep(c("Men", "Women"), 2),
                 type = c("C. Early career researcher", "C. Early career researcher", "D. Mid-career researcher", "D. Mid-career researcher"),
                 x = c(-3.5, -3.5, NA, NA),
                 y = c(.2, 0.07, NA, NA))

counterfactual_trend <- tibble(type = c("C. Early career researcher", "C. Early career researcher", "D. Mid-career researcher", "D. Mid-career researcher"),
                               pub_year = c(-1, 0, -1, 0),
                               trend = as.numeric(c(ols_pred_frac_ab[19, 1], ols_pred_frac_ab[19, 1] + 
                                                      (ols_pred_frac_ab[15, 1] - ols_pred_frac_ab[14, 1]), # Early career counterfactual
                                                    ols_pred_frac_ab[9, 1], ols_pred_frac_ab[9, 1] + 
                                                      (ols_pred_frac_ab[5, 1] - ols_pred_frac_ab[4, 1]))) # Senior counterfactual
)

ols_pred_frac_samples <- ols_pred_frac_ab %>% 
  mutate(gender_text = case_when(gender_num == 0 ~ "Men",
                                 gender_num == 1 ~ "Women")) %>% 
  ggplot(aes(x = as.numeric(time_factor), y = n_pubs_frac)) +
  geom_vline(xintercept = -0.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = gender_text), alpha = 1/4) +
  geom_line(aes(colour = gender_text), size = 1.5) +
  geom_line(data = counterfactual_trend, aes(x = pub_year, y = trend), color = orange, linetype = "dashed", size = 1) +
  geom_point(data = counterfactual_trend, aes(x = pub_year, y = trend), color = orange, size = 2.5) +
  geom_point(aes(colour = gender_text, fill = gender_text), shape = 21, fill = "white", size = 2.5, stroke = 2) +
  #geom_text(aes(x = -0.1 , y = 2, label = "Lock-down")) +
  geom_text(data = labels, aes(x = x, y = y, label = group, colour = group), size = 5) +
  scale_x_continuous(label = c("2016", "2017", "2018", "2019", "2020")) +
  scale_y_continuous(limits = c(0, .4)) +
  scale_fill_manual(values = c(orange, maroon)) +
  scale_color_manual(values = c(orange, maroon)) +
  labs(x = "", y = "Avg. predicted\npublication counts", title = "Predicted publication counts") +
  facet_wrap(~ type) +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing = unit(2, "lines"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# Combined figure

figure_S2 <- ols_ame_frac_samples / ols_pred_frac_samples