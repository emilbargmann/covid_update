# Figure S3

source("0_settings.R")

load("data/ame_results.Rdata")
load("data/pred_results.Rdata")

# Average marginal effect on full publication counts

ame_linear_counter_full <- ggplot(data = ols_ame_counter, aes(x = as.numeric(time_factor), y = AME, group = 1)) +
  geom_vline(xintercept = 4.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey50", alpha = 1/4) +
  geom_line(size = 1.5)+
  geom_point(shape = 21, fill = "white", size = 2.5, stroke = 2) +
  #geom_text(aes(x = 3.75 , y = 0.05, label = "Lock-down")) +
  scale_x_continuous(label = c("2011", "2012", "2013", "2014", "2015")) +
  scale_y_continuous(limits = c(-0.6, 0.1), breaks = c(-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1)) +
  labs(x = "", y = "Avg. differences", title = "A. Differences in publication count") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0))

# Average marginal effect on fractional publication counts
ame_linear_frac_counter <- ggplot(data = ols_ame_frac_counter, aes(x = as.numeric(time_factor), y = AME, group = 1)) +
  geom_vline(xintercept = 4.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey50", alpha = 1/4) +
  geom_line(size = 1.5)+
  geom_point(shape = 21, fill = "white", size = 2.5, stroke = 2) +
  #geom_text(aes(x = 3.75 , y = 0.05, label = "Lock-down")) +
  scale_x_continuous(label = c("2011", "2012", "2013", "2014", "2015")) +
  scale_y_continuous(limits = c(-0.1, 0.05), breaks = c(-0.1, -0.05, 0, 0.05)) +
  labs(x = "", y = "Avg. differences", title = "B. Fractionalized counts") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0))


# Average predicted publication counts for full counts

labels <- tibble(group = c("Men", "Women"),
                 x = c(-8.5, -8.5),
                 y = c(1.1, 0.4))

counterfactual_trend <- tibble(pub_year = c(-6, -5), # effect if female researchers had same drop as male
                               trend = as.numeric(c(ols_pred_counter[9, 1], ols_pred_counter[9, 1] + 
                                                      (ols_pred_counter[5, 1] - ols_pred_counter[4, 1])))
)

predicted_linear_counter <- ols_pred_counter %>% 
  mutate(gender_text = case_when(gender_num == 0 ~ "Men",
                                 gender_num == 1 ~ "Women")) %>% 
  ggplot(aes(x = as.numeric(time_factor), y = n_pubs)) +
  geom_vline(xintercept = -5.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = gender_text), alpha = 1/2) +
  geom_line(aes(colour = gender_text), size = 1.5) +
  geom_line(data = counterfactual_trend, aes(x = pub_year, y = trend), color = orange, linetype = "dashed", size = 1) +
  geom_point(data = counterfactual_trend, aes(x = pub_year, y = trend), color = orange, size = 2.5) +
  geom_point(aes(colour = gender_text, fill = gender_text), shape = 21, fill = "white", size = 2.5, stroke = 2) +
  #geom_text(aes(x = -0.1 , y = 2, label = "Lock-down")) +
  geom_text(data = labels, aes(x = x, y = y, label = group, colour = group), size = 5) +
  scale_x_continuous(label = c("2011", "2012", "2013", "2014", "2015")) +
  scale_y_continuous(limits = c(0, 2)) +
  scale_fill_manual(values = c(orange, maroon)) +
  scale_color_manual(values = c(orange, maroon)) +
  labs(x = "", y = "Avg. predicted\npublication counts") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

# Average predicted publication counts for fractional counts

labels <- tibble(group = c("Men", "Women"),
                 x = c(-8.5, -8.5),
                 y = c(.25, 0.08))

counterfactual_trend_2 <- tibble(pub_year = c(-6, -5), # effect if female researchers had same drop as male
                                 trend = as.numeric(c(ols_pred_frac_counter[9, 1], ols_pred_frac_counter[9, 1] + 
                                                        (ols_pred_frac_counter[5, 1] - ols_pred_frac_counter[4, 1])))
)

predicted_linear_frac_counter <- ols_pred_frac_counter %>% 
  mutate(gender_text = case_when(gender_num == 0 ~ "Men",
                                 gender_num == 1 ~ "Women")) %>% 
  ggplot(aes(x = as.numeric(time_factor), y = n_pubs_frac)) +
  geom_vline(xintercept = -5.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = gender_text), alpha = 1/4) +
  geom_line(aes(colour = gender_text), size = 1.5) +
  geom_line(data = counterfactual_trend_2, aes(x = pub_year, y = trend), color = orange, linetype = "dashed", size = 1) +
  geom_point(data = counterfactual_trend_2, aes(x = pub_year, y = trend), color = orange, size = 2.5) +
  geom_point(aes(colour = gender_text, fill = gender_text), shape = 21, fill = "white", size = 2.5, stroke = 2) +
  geom_text(data = labels, aes(x = x, y = y, label = group, colour = group), size = 5) +
  scale_x_continuous(label = c("2011", "2012", "2013", "2014", "2015")) +
  scale_y_continuous(limits = c(0, .3)) +
  scale_fill_manual(values = c(orange, maroon)) +
  scale_color_manual(values = c(orange, maroon)) +
  labs(x = "", y = "Avg. predicted\npublication counts") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

# Combined figure

figure_S3 <- (ame_linear_counter_full + ame_linear_frac_counter) / (predicted_linear_counter + predicted_linear_frac_counter)