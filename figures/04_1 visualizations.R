# 1 Initialize ----

load("jpa/ame_results.Rdata")
load("jpa/pred_results.Rdata")
load("jpa/bg_data.Rdata")

source("jpa/00 settings.R")

# 2 Figure 1 ----

plot_data <- author_identified %>% 
  filter(pub_year %in% c(2019, 2020)) %>% 
  group_by(gender_category, pub_year) %>%
  summarise(mean_pubs_full = mean(n_pubs),
            mean_pubs_frac = mean(n_pubs_frac),
            total_pubs_full = sum(n_pubs),
            total_pubs_frac = sum(n_pubs_frac)) 

plot_data$gender_category[plot_data$gender_category == "Male"] <- "Men"
plot_data$gender_category[plot_data$gender_category == "Female"] <- "Women"

labels <- plot_data %>% 
  filter(pub_year == 2020)

growth <- tibble(gender_category = c("Women", "Men"),
                 pub_year = c("2020", "2020"), 
                 mean_growth_full = as.numeric(c((plot_data[2, 3] - plot_data[1, 3])/plot_data[1, 3], (plot_data[4, 3] - plot_data[3, 3])/plot_data[3, 3])),
                 total_growth_full = as.numeric(c((plot_data[2, 5] - plot_data[1, 5])/plot_data[1, 5], (plot_data[4, 5] - plot_data[3, 5])/plot_data[3, 5])),
                 mean_growth_frac = as.numeric(c((plot_data[2, 4] - plot_data[1, 4])/plot_data[1, 4], (plot_data[4, 4] - plot_data[3, 4])/plot_data[3, 4])),
                 total_growth_frac = as.numeric(c((plot_data[2, 6] - plot_data[1, 6])/plot_data[1, 6], (plot_data[4, 6] - plot_data[3, 6])/plot_data[3, 6]))
)

labels <- labels %>% 
  left_join(growth, by = c("gender_category", "pub_year"))

bar_full <- ggplot(plot_data, aes(x = gender_category, y = mean_pubs_full)) + 
  geom_col(aes(fill = pub_year, color = pub_year), position = position_dodge(0.75), width = 0.5) + 
  geom_hline(yintercept = 0, size = 1) +
  geom_text(data = labels, aes(x = gender_category, y = mean_pubs_full, label = paste0(round(mean_growth_full, 3)*100, "%")), nudge_x = 0.2, nudge_y = 0.1) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 1.5, 0.5)) +
  scale_color_manual(values = c("black", "grey30"), guide = "legend") +
  scale_fill_manual(values = c("white", "grey30"), guide = "legend") +
  labs(x = "", y = "Avg. number of publications", title = "Full counts") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.position = c(0.2, 0.9),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

bar_frac <- ggplot(plot_data, aes(x = gender_category, y = mean_pubs_frac)) + 
  geom_col(aes(fill = pub_year, color = pub_year), position = position_dodge(0.75), width = 0.5) + 
  geom_hline(yintercept = 0, size = 1) +
  geom_text(data = labels, aes(x = gender_category, y = mean_pubs_frac, label = paste0(round(mean_growth_frac, 3)*100, "%")), nudge_x = 0.2, nudge_y = 0.015) +
  scale_color_manual(values = c("black", "grey30")) +
  scale_fill_manual(values = c("white", "grey30")) +
  #guides(color = FALSE) +
  labs(x = "", y = "", title = "Fractionalized counts") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

# 3 Average marginal effects ----

## 3.1 AME Full ----

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

## 3.2 AME Frac ----

ame_linear_frac <- ggplot(data = ols_ame_frac, aes(x = as.numeric(time_factor), y = AME, group = 1)) +
  geom_vline(xintercept = 4.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey50", alpha = 1/4) +
  geom_line(size = 1.5)+
  geom_point(shape = 21, fill = "white", size = 2.5, stroke = 2) +
  #geom_text(aes(x = 3.75 , y = 0.05, label = "Lock-down")) +
  scale_x_continuous(label = c("2016", "2017", "2018", "2019", "2020")) +
  scale_y_continuous(limits = c(-0.1, 0.05), breaks = c(-0.1, -0.05, 0, 0.05)) +
  labs(x = "", y = "Avg. differences", title = "A. Differences in publication count") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0))

## 3.3 AME Counterfactual ----
### 3.3.1 Full ----

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

### 3.3.2 Frac ----
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


## 3.4 Average marginal effects by quartile ----

qlabs <- c("1-50%", "51-80%", "81-90%", "91-100%")

ames_full <- ggplot(data = ames, aes(x = as.numeric(time_factor), y = AME, group = as.factor(q))) +
  geom_vline(xintercept = 4.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.factor(q)), alpha = 1/4) +
  geom_line(size = 1.5, aes(color = as.factor(q)))+
  geom_point(shape = 21, fill = "white", size = 2.5, stroke = 2, color = "black") +
  scale_x_continuous(label = c("2016", "2017", "2018", "2019", "2020")) +
  scale_y_continuous(limits = c(-2.1, 0.1), breaks = seq(-2.1, .1, by = .2)) +
  labs(x = "", y = "Avg. differences", title = "A. Differences in publication count") +
  scale_color_manual("Quantile", values = c(orange, maroon, olive, navy, peach, moss), labels = qlabs) +
  scale_fill_manual("Quantile", values = c(orange, maroon, olive, navy, peach, moss), labels = qlabs) +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0),
        legend.position = "bottom")

# 4 Average predicted publication counts ----
## 4.1 Pred full ----

labels <- tibble(group = c("Men", "Women"),
                 x = c(-3.5, -3.5),
                 y = c(1.25, 0.65))

counterfactual_trend <- tibble(pub_year = c(-1, 0), # effect if female researchers had same drop as male
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

## 4.2 Pred frac ----

labels <- tibble(group = c("Men", "Women"),
                 x = c(-3.5, -3.5),
                 y = c(.25, 0.1))

counterfactual_trend <- tibble(pub_year = c(-1, 0), # effect if female researchers had same drop as male
                               trend = as.numeric(c(ols_pred_frac[9, 1], ols_pred_frac[9, 1] + (ols_pred_frac[5, 1] - ols_pred_frac[4, 1])))
)

predicted_linear_frac <- ols_pred_frac %>% 
  mutate(gender_text = case_when(gender_num == 0 ~ "Men",
                                 gender_num == 1 ~ "Women")) %>% 
  ggplot(aes(x = as.numeric(time_factor), y = n_pubs_frac)) +
  geom_vline(xintercept = -0.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = gender_text), alpha = 1/2) +
  geom_line(aes(colour = gender_text), size = 1.5) +
  geom_line(data = counterfactual_trend, aes(x = pub_year, y = trend), color = orange, linetype = "dashed", size = 1) +
  geom_point(data = counterfactual_trend, aes(x = pub_year, y = trend), color = orange, size = 2.5) +
  geom_point(aes(colour = gender_text, fill = gender_text), shape = 21, fill = "white", size = 2.5, stroke = 2) +
  geom_text(data = labels, aes(x = x, y = y, label = group, colour = group), size = 5) +
  scale_x_continuous(label = c("2016", "2017", "2018", "2019", "2020")) +
  scale_y_continuous(limits = c(0, .3)) +
  scale_fill_manual(values = c(orange, maroon)) +
  scale_color_manual(values = c(orange, maroon)) +
  labs(x = "", y = "Avg. predicted\npublication counts", title = "B. Predicted publication counts") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

## 4.3 Pred counterfactual ----

### 4.3.1 Full ----

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

### 4.3.2 Frac ----

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


## 4.4 Pred by quantile ----

preds <- preds %>% mutate(gender_text = case_when(gender_num == 0 ~ "Men",
                                        gender_num == 1 ~ "Women"))

qs = unique(preds$q)
cf_trends <- tibble(pub_year = rep(c(-1, 0), length(qs)), q = rep(qs, each = 2), trend = 0, gender_text = "Women")

for (i in 1:length(qs)) {
  qi <- qs[i]
  x <- preds %>% filter(q == qi & gender_num == 0)
  y <- preds %>% filter(q == qi & gender_num == 1)
  cf_trends$trend[cf_trends$pub_year == -1 & cf_trends$q == qi] <- y$n_pubs[y$time_factor == -1]
  cf_trends$trend[cf_trends$pub_year == 0 & cf_trends$q == qi] <- y$n_pubs[y$time_factor == -1] - (x$n_pubs[x$time_factor == -1] - x$n_pubs[x$time_factor == 0])
}

preds_full <- preds %>% 
  ggplot(aes(x = as.numeric(time_factor), y = n_pubs, group = interaction(gender_text, q))) +
  geom_vline(xintercept = -0.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = as.factor(q)), alpha = 1/4) +
  geom_line(aes(colour = as.factor(q), lty = gender_text), size = 1.5) + 
  geom_line(data = cf_trends, aes(x = pub_year, y = trend, color = as.factor(q)), linetype = "dashed", size = 1, alpha = .5) +
  geom_point(data = cf_trends, aes(x = pub_year, y = trend), color = "black", size = 2.5, stroke = 2) +
  geom_point(aes(colour = as.factor(q)), shape = 21, fill = "white", size = 2.5, stroke = 2) +
  scale_x_continuous(label = c("2016", "2017", "2018", "2019", "2020")) +
  scale_y_continuous(limits = c(0, 6)) +
  scale_fill_manual("Quantile", values = c(orange, maroon, olive, navy, peach, moss), guide = "none") +
  scale_color_manual("Quantile", values = c(orange, maroon, olive, navy, peach, moss), guide = "none") +
  scale_linetype_discrete("Gender") +
  labs(x = "", y = "Avg. predicted\npublication counts", title = "B. Predicted publication counts") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom")

# 5 Career stage subsamples ----

## 5.1 AME ----

ols_pred_full_ab <- rbind(ols_pred_full_a, ols_pred_full_b)
ols_ame_full_ab <- rbind(ols_ame_full_a, ols_ame_full_b)
ols_ame_full_ab$type[ols_ame_full_ab$type == "Senior researcher"] <- "B. Mid-career researcher"
ols_ame_full_ab$type[ols_ame_full_ab$type == "Early career researcher"] <- "A. Early career researcher"
ols_pred_full_ab$type[ols_pred_full_ab$type == "Senior researcher"] <- "D. Mid-career researcher"
ols_pred_full_ab$type[ols_pred_full_ab$type == "Early career researcher"] <- "C. Early career researcher"

ols_ame_full_samples <- ggplot(data = ols_ame_full_ab, aes(x = as.numeric(time_factor), y = AME, group = 1)) +
  geom_vline(xintercept = 4.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey50", alpha = 1/4) +
  geom_line(size = 1.5)+
  geom_point(shape = 21, fill = "white", size = 2.5, stroke = 2) +
  #geom_text(aes(x = 3.75 , y = 0.05, label = "Lock-down")) +
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

## 5.2 Pred ----

labels <- tibble(group = rep(c("Men", "Women"), 2),
                 type = c("C. Early career researcher", "C. Early career researcher", "D. Mid-career researcher", "D. Mid-career researcher"),
                 x = c(-3.5, -2.5, NA, NA),
                 y = c(1.15, 0.45, NA, NA))

counterfactual_trend <- tibble(type = c("C. Early career researcher", "C. Early career researcher", "D. Mid-career researcher", "D. Mid-career researcher"),
                               pub_year = c(-1, 0, -1, 0),
                               trend = as.numeric(c(ols_pred_full_ab[19, 1], ols_pred_full_ab[19, 1] + (ols_pred_full_ab[15, 1] - ols_pred_full_ab[14, 1]), # Early career counterfactual
                                                    ols_pred_full_ab[9, 1], ols_pred_full_ab[9, 1] + (ols_pred_full_ab[5, 1] - ols_pred_full_ab[4, 1]))) # Senior counterfactual
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
  #geom_text(aes(x = -0.1 , y = 2, label = "Lock-down")) +
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

## 5.3 Fractional ----

### 5.3.1 AME ----

ols_pred_frac_ab <- rbind(ols_pred_frac_a, ols_pred_frac_b)
ols_ame_frac_ab <- rbind(ols_ame_frac_a, ols_ame_frac_b)
ols_ame_frac_ab$type[ols_ame_frac_ab$type == "Senior researcher"] <- "B. Mid-career researcher"
ols_ame_frac_ab$type[ols_ame_frac_ab$type == "Early career researcher"] <- "A. Early career researcher"
ols_pred_frac_ab$type[ols_pred_frac_ab$type == "Senior researcher"] <- "D. Mid-career researcher"
ols_pred_frac_ab$type[ols_pred_frac_ab$type == "Early career researcher"] <- "C. Early career researcher"

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

### 5.3.2 Pred ----

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

# 6 Country analysis ----


full_effect <- -0.0933
full_se <- 0.0055

full_counterfactual <- -0.0501
full_counterfactual_se <- 0.0064

t_value <- 2.576 # 99 % (for 99.9 % insert 3.291)

ols_coef_full_country <- ols_model_full_country %>% 
  dplyr::filter(year == 2020) %>% 
  mutate(country = str_to_title(country)) %>% 
  ggplot(aes(y = reorder(country, coef), x = coef))+
  geom_vline(xintercept = full_effect, color = orange, size = 1) +
  geom_vline(xintercept = full_counterfactual, color = olive, size = 1) +
  geom_rect(aes(
    xmin = full_effect-(full_se*t_value), xmax = full_effect+(full_se*t_value), ymin = -Inf, ymax = Inf), 
    fill = orange, alpha = 0.0125)+
  geom_rect(aes(
    xmin = full_counterfactual-(full_counterfactual_se*t_value), xmax = full_counterfactual+(full_counterfactual_se*t_value), ymin = -Inf, ymax = Inf), 
    fill = olive, alpha = 0.0125)+
  geom_vline(xintercept = 0, size = 1, linetype = "dotted") +
  geom_linerange(aes(xmin = ll, xmax = ul), size = 1)+
  geom_point(shape = 21, fill = "white", size = 2, stroke = 1.5) +
  annotate(geom = "segment", x = full_effect-(full_se*t_value), xend = -0.275, y = 29.5, yend = 29.5, color = orange,
           arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  annotate(geom = "segment", x = full_counterfactual+(full_counterfactual_se*t_value), xend = 0.1, y = 3, yend = 3, color = olive,
           arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  geom_label(aes(x = -0.275, y = 29.5, label = "Overall ATT"), color = orange) +
  geom_label(aes(x = 0.115, y = 3, label = "Counterfactual\nATT"), color = olive) +
  scale_x_continuous(limits = c(-0.4, 0.2)) +
  labs(y = "", x = expression("Gender" %*% "2020")) +
  theme_ebm_grid() +
  theme(axis.line.x = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.8, 0.9),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(r = 0, unit = "cm"))

n_country_hist <- ols_model_full_country %>% 
  filter(year == 2020) %>% 
  mutate(country = str_to_title(country)) %>% 
  ggplot(aes(y = reorder(country, coef), x = n/1000)) +
  geom_col() +
  scale_x_continuous(limits = c(0, 110), breaks = c(0, 50, 110), labels = c("0", "50k", "110k"), expand = c(0,0)) +
  labs(x = "Authors", y = "") +
  theme_ebm_bar() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey85", linetype = "dotted"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.05, 1, 0.05, 0, unit = "cm"))

# 7 Discipline analysis ----

ols_coef_full_discipline <- ols_model_full_discipline %>% 
  filter(year == 2020) %>% 
  ggplot(aes(x = coef, y = reorder(oecd, coef))) +
  geom_vline(xintercept = full_effect, color = orange, size = 1) +
  geom_vline(xintercept = full_counterfactual, color = olive, size = 1) +
  geom_rect(aes(
    xmin = full_effect-(full_se*t_value), xmax = full_effect+(full_se*t_value), ymin = -Inf, ymax = Inf), 
    fill = orange, alpha = 0.05)+
  geom_rect(aes(
    xmin = full_counterfactual-(full_counterfactual_se*t_value), 
    xmax = full_counterfactual+(full_counterfactual_se*t_value), ymin = -Inf, ymax = Inf), 
    fill = olive, alpha = 0.05)+
  geom_vline(xintercept = 0, size = 1, linetype = "dotted") +
  geom_linerange(aes(xmin = ll, xmax = ul), size = 1)+
  geom_point(shape = 21, fill = "white", size = 2, stroke = 1.5) +
  annotate(geom = "segment", x = full_effect-(full_se*t_value), xend = -0.16, y = 3.5, yend = 3.5, color = orange,
           arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  annotate(geom = "segment", x = full_counterfactual+(full_counterfactual_se*t_value), xend = 0.05, y = 1.5, yend = 1.5, color = olive,
           arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  geom_label(aes(x = -0.16, y = 3.5, label = "Overall ATT"), color = orange) +
  geom_label(aes(x = 0.05, y = 1.5, label = "Counterfactual\nATT"), color = olive) +
  scale_x_continuous(limits = c(-0.2, 0.1)) +
  labs(y = "", x = expression("Gender" %*% "2020"), title = "A. Discipline gap") +
  theme_ebm_grid() +
  theme(axis.line.x = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.8, 0.9),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(r = 0, unit = "cm"))

n_discipline_hist <- ols_model_full_discipline %>% 
  filter(year == 2020) %>% 
  ggplot(aes(y = reorder(oecd, coef), x = n/1000)) +
  geom_col() +
  scale_x_continuous(limits = c(0, 220), breaks = c(0, 100, 220), labels = c("0", "100k", "220k"), expand = c(0,0)) +
  labs(x = "Authors", y = "") +
  theme_ebm_bar() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey85", linetype = "dotted"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.05, 1, 0.05, 0, unit = "cm"))

ols_coef_full_discipline_hist <- ols_coef_full_discipline + n_discipline_hist + plot_layout(widths = c(3,1))

## 7.1 People per field and decile ----

p1 <- d_author %>% 
  group_by(oecd) %>%
  summarize(n = n())

pd <- d_author %>%
  group_by(oecd, q) %>%
  summarize(nd = n())

pd$ratio <- 0
pd$ratio[pd$oecd == "Basic medicine"] <- pd$nd[pd$oecd == "Basic medicine"] / p1$n[p1$oecd == "Basic medicine"] * 10
pd$ratio[pd$oecd == "Clinical medicine"] <- pd$nd[pd$oecd == "Clinical medicine"] / p1$n[p1$oecd == "Clinical medicine"] * 10
pd$ratio[pd$oecd == "Biology"] <- pd$nd[pd$oecd == "Biology"] / p1$n[p1$oecd == "Biology"] * 10
pd$ratio[pd$oecd == "Chemistry"] <- pd$nd[pd$oecd == "Chemistry"] / p1$n[p1$oecd == "Chemistry"] * 10

a_disc <- pd %>%
  ggplot(aes(x = as.numeric(q), y = nd, group = oecd, fill = oecd)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_x_continuous("Decile, number of total publications", breaks = 1:10) +
  scale_y_continuous("Number of authors per discipline", labels = label_number(suffix = "K", scale = 1e-3)) +
  scale_fill_manual("Discipline", values = c(orange, maroon, olive, navy)) +
  labs(title = "B. Author distribution per discipline") +
  theme_ebm_bar() + 
  theme(
    legend.position = "bottom"
  )



# 8 Figure composition ----

fig1 <- bar_full | bar_frac + plot_layout(guides = "collect") 
fig2 <- ame_linear_full + predicted_linear_full
fig3 <- ols_ame_full_samples / ols_pred_full_samples
fig4 <- ames_full + preds_full
fig5 <- ols_coef_full_country + n_country_hist + plot_layout(widths = c(3,1))

layout <- "
AAAABCCC
AAAABCCC
AAAABCCC
AAAABCCC
DDDDDCCC
#####CCC
"

fig6 <- ols_coef_full_discipline + n_discipline_hist + a_disc + guide_area() + 
  plot_layout(design = layout, guides = "collect") & 
  theme(legend.position = "bottom",
        legend.justification = c("left","bottom"),
        legend.box.just = "right",
        legend.box.margin = margin(0,0,0,0))


figS1 <- ame_linear_frac + predicted_linear_frac
figS2 <- (ame_linear_counter_full + ame_linear_frac_counter) / (predicted_linear_counter + predicted_linear_frac_counter)
figS3 <- ols_ame_frac_samples / ols_pred_frac_samples

agg_tiff("jpa/figures/figure_1.tiff", width = 24, height = 12, units = "cm", res = 300)
fig1
invisible(dev.off())

agg_tiff("jpa/figures/figure_2.tiff", width = 24, height = 12, units = "cm", res = 300)
fig2
invisible(dev.off())

agg_tiff("jpa/figures/figure_3.tiff", width = 20, height = 20, units = "cm", res = 300)
fig3
invisible(dev.off())

agg_tiff("jpa/figures/figure_4.tiff", width = 24, height = 12, units = "cm", res = 300)
fig4
invisible(dev.off())

agg_tiff("jpa/figures/figure_5.tiff", width = 20, height = 20, units = "cm", res = 300)
fig5
invisible(dev.off())

agg_tiff("jpa/figures/figure_6.tiff", width = 34, height = 12, units = "cm", res = 300)
fig6
invisible(dev.off())

agg_tiff("jpa/figures/figure_S1.tiff", width = 24, height = 12, units = "cm", res = 300)
figS1
invisible(dev.off())

agg_tiff("jpa/figures/figure_S2.tiff", width = 20, height = 20, units = "cm", res = 300)
figS2
invisible(dev.off())

agg_tiff("jpa/figures/figure_S3.tiff", width = 20, height = 20, units = "cm", res = 300)
figS3
invisible(dev.off())



agg_png("jpa/figures/figure_1.png", width = 24, height = 12, units = "cm", res = 300)
fig1
invisible(dev.off())

agg_png("jpa/figures/figure_2.png", width = 24, height = 12, units = "cm", res = 300)
fig2
invisible(dev.off())

agg_png("jpa/figures/figure_3.png", width = 20, height = 20, units = "cm", res = 300)
fig3
invisible(dev.off())

agg_png("jpa/figures/figure_4.png", width = 24, height = 12, units = "cm", res = 300)
fig4
invisible(dev.off())

agg_png("jpa/figures/figure_5.png", width = 20, height = 20, units = "cm", res = 300)
fig5
invisible(dev.off())

agg_png("jpa/figures/figure_6.png", width = 34, height = 12, units = "cm", res = 300)
fig6
invisible(dev.off())

agg_png("jpa/figures/figure_S1.png", width = 24, height = 12, units = "cm", res = 300)
figS1
invisible(dev.off())

agg_png("jpa/figures/figure_S2.png", width = 20, height = 20, units = "cm", res = 300)
figS2
invisible(dev.off())

agg_png("jpa/figures/figure_S3.png", width = 20, height = 20, units = "cm", res = 300)
figS3
invisible(dev.off())