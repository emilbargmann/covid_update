# Figure 2

source("0_settings.R")

load("data/ame_results.Rdata")
load("data/pred_results.Rdata")

# Average marginal effect on full publication counts

ame_linear_full <- ggplot(data = ols_ame_full, aes(x = as.numeric(time_factor), y = AME, group = 1)) +
  geom_vline(xintercept = 4.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey50", alpha = 1/4) +
  geom_line(size = 1.5)+
  geom_point(shape = 21, fill = "white", size = 2.5, stroke = 2) +
  #geom_text(aes(x = 3.75 , y = 0.05, label = "Lock-down")) +
  scale_x_continuous(label = c("2016", "2017", "2018", "2019", "2020")) +
  scale_y_continuous(limits = c(-0.6, 0.1), breaks = c(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1)) +
  labs(x = "", y = "Avg. differences", title = "A. Differences in publication count") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0))

# Average predicted full publication counts

labels <- tibble(group = c("Men", "Women"), # Labels for figure
                 x = c(-3.5, -3.5),
                 y = c(1.25, 0.65))

counterfactual_trend <- tibble(pub_year = c(-1, 0), # Effect if women researchers had same drop as men
                               trend = as.numeric(c(ols_pred_full[9, 1], ols_pred_full[9, 1] + (ols_pred_full[5, 1] - ols_pred_full[4, 1])))
)

predicted_linear_full <- ols_pred_full %>% 
  mutate(gender_text = case_when(gender_num == 0 ~ "Men",
                                 gender_num == 1 ~ "Women")) %>% 
  ggplot(aes(x = as.numeric(time_factor), y = n_pubs)) +
  geom_vline(xintercept = -0.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = gender_text), alpha = 1/2) +
  geom_line(aes(colour = gender_text), size = 1.5) +
  geom_line(data = counterfactual_trend, aes(x = pub_year, y = trend), color = orange, linetype = "dashed", size = 1) +
  geom_point(data = counterfactual_trend, aes(x = pub_year, y = trend), color = orange, size = 2.5) +
  geom_point(aes(colour = gender_text, fill = gender_text), shape = 21, fill = "white", size = 2.5, stroke = 2) +
  geom_text(data = labels, aes(x = x, y = y, label = group, colour = group), size = 5) +
  scale_x_continuous(label = c("2016", "2017", "2018", "2019", "2020")) +
  scale_y_continuous(limits = c(0, 2)) +
  scale_fill_manual(values = c(orange, maroon)) +
  scale_color_manual(values = c(orange, maroon)) +
  labs(x = "", y = "Avg. predicted\npublication counts", title = "B. Predicted publication counts") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

# Combined figure

figure_2 <- ame_linear_full + predicted_linear_full