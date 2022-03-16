# Figure 3

source("0_settings.R")

load("data/ame_results.Rdata")
load("data/pred_results.Rdata")

# Average marginal effect on full publication counts by sample

ols_pred_full_ab <- rbind(ols_pred_full_a, ols_pred_full_b)
ols_ame_full_ab <- rbind(ols_ame_full_a, ols_ame_full_b)
ols_ame_full_ab$type[ols_ame_full_ab$type == "Senior researcher"] <- "B. Mid-career researcher" # Redefine labels
ols_ame_full_ab$type[ols_ame_full_ab$type == "Early career researcher"] <- "A. Early career researcher"
ols_pred_full_ab$type[ols_pred_full_ab$type == "Senior researcher"] <- "D. Mid-career researcher"
ols_pred_full_ab$type[ols_pred_full_ab$type == "Early career researcher"] <- "C. Early career researcher"

ols_ame_full_samples <- ggplot(data = ols_ame_full_ab, aes(x = as.numeric(time_factor), y = AME, group = 1)) +
  geom_vline(xintercept = 4.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey50", alpha = 1/4) +
  geom_line(size = 1.5)+
  geom_point(shape = 21, fill = "white", size = 2.5, stroke = 2) +
  scale_x_continuous(label = c("2016", "2017", "2018", "2019", "2020")) +
  scale_y_continuous(limits = c(-0.65, 0.1), breaks = c(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1)) +
  labs(x = "", y = "Avg. differences", title = "Differences in publication count") +
  facet_wrap(~ type) +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing = unit(2, "lines"),
        plot.title = element_text(hjust = 0.5)
  )


# Average predicted full publication counts by sample

labels <- tibble(group = rep(c("Men", "Women"), 2), # Labels for plot
                 type = c("C. Early career researcher", "C. Early career researcher", "D. Mid-career researcher", "D. Mid-career researcher"),
                 x = c(-3.5, -2.5, NA, NA),
                 y = c(1.15, 0.45, NA, NA))

counterfactual_trend <- tibble(type = c("C. Early career researcher", "C. Early career researcher", "D. Mid-career researcher", "D. Mid-career researcher"),
                               pub_year = c(-1, 0, -1, 0),
                               trend = as.numeric(c(ols_pred_full_ab[19, 1], ols_pred_full_ab[19, 1] + (ols_pred_full_ab[15, 1] - ols_pred_full_ab[14, 1]), # Early career counterfactual
                                                    ols_pred_full_ab[9, 1], ols_pred_full_ab[9, 1] + (ols_pred_full_ab[5, 1] - ols_pred_full_ab[4, 1]))) # Mid-career counterfactual
)

ols_pred_full_samples <- ols_pred_full_ab %>% 
  mutate(gender_text = case_when(gender_num == 0 ~ "Men",
                                 gender_num == 1 ~ "Women")) %>% 
  ggplot(aes(x = as.numeric(time_factor), y = n_pubs)) +
  geom_vline(xintercept = -0.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = gender_text), alpha = 1/4) +
  geom_line(aes(colour = gender_text), size = 1.5) +
  geom_line(data = counterfactual_trend, aes(x = pub_year, y = trend), color = orange, linetype = "dashed", size = 1) +
  geom_point(data = counterfactual_trend, aes(x = pub_year, y = trend), color = orange, size = 2.5) +
  geom_point(aes(colour = gender_text, fill = gender_text), shape = 21, fill = "white", size = 2.5, stroke = 2) +
  geom_text(data = labels, aes(x = x, y = y, label = group, colour = group), size = 5) +
  scale_x_continuous(label = c("2016", "2017", "2018", "2019", "2020")) +
  scale_y_continuous(limits = c(0, 2.5)) +
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

figure_3 <- ols_ame_full_samples / ols_pred_full_samples