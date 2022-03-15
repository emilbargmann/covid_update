# Figure 5

load("data/bg_data.Rdata")

# Reference values from overall analysis
full_effect <- -0.0933
full_se <- 0.0055

full_counterfactual <- -0.0501
full_counterfactual_se <- 0.0064

t_value <- 2.576 # 99 % (for 99.9 % insert 3.291)

# Coefficients plot

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

# Histogram

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

# Combined figure

figure_5 <- ols_coef_full_country + n_country_hist + plot_layout(widths = c(3,1))