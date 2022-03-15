# Figure 1

source("0_settings.R")

load("data/fig1_data.Rdata")

# Change labels

fig1_data$gender_category[fig1_data$gender_category == "Female"] <- "Women"
fig1_data$gender_category[fig1_data$gender_category == "Male"] <- "Men"

# Setup labels for percentage change
labels <- fig1_data %>% 
  filter(pub_year == 2020)

growth <- tibble(gender_category = c("Women", "Men"),
                 pub_year = c("2020", "2020"), 
                 mean_growth_full = as.numeric(c((fig1_data[2, 3] - fig1_data[1, 3])/fig1_data[1, 3], (fig1_data[4, 3] - fig1_data[3, 3])/fig1_data[3, 3])),
                 total_growth_full = as.numeric(c((fig1_data[2, 5] - fig1_data[1, 5])/fig1_data[1, 5], (fig1_data[4, 5] - fig1_data[3, 5])/fig1_data[3, 5])),
                 mean_growth_frac = as.numeric(c((fig1_data[2, 4] - fig1_data[1, 4])/fig1_data[1, 4], (fig1_data[4, 4] - fig1_data[3, 4])/fig1_data[3, 4])),
                 total_growth_frac = as.numeric(c((fig1_data[2, 6] - fig1_data[1, 6])/fig1_data[1, 6], (fig1_data[4, 6] - fig1_data[3, 6])/fig1_data[3, 6]))
)

labels <- labels %>% 
  left_join(growth, by = c("gender_category", "pub_year"))

# Full publication counts

t_value <- 2.576 # 99 % (for 99.9 % insert 3.291)

bar_full <- ggplot(fig1_data, aes(x = gender_category, y = mean_pubs_full)) + 
  geom_col(aes(fill = pub_year, color = pub_year), position = position_dodge(0.75), width = 0.5) + 
  geom_linerange(aes(ymin = mean_pubs_full - (t_value * se_pubs_full), ymax = mean_pubs_full + (t_value * se_pubs_full)), position = position_dodge2(0.75)) +
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

# Fractionalized publication counts

bar_frac <- ggplot(fig1_data, aes(x = gender_category, y = mean_pubs_frac)) + 
  geom_col(aes(fill = pub_year, color = pub_year), position = position_dodge(0.75), width = 0.5) + 
  geom_linerange(aes(ymin = mean_pubs_frac - (t_value * se_pubs_frac), ymax = mean_pubs_frac + (t_value * se_pubs_frac)), position = position_dodge2(0.75)) +
  geom_hline(yintercept = 0, size = 1) +
  geom_text(data = labels, aes(x = gender_category, y = mean_pubs_frac, label = paste0(round(mean_growth_frac, 3)*100, "%")), nudge_x = 0.2, nudge_y = 0.015) +
  scale_color_manual(values = c("black", "grey30")) +
  scale_fill_manual(values = c("white", "grey30")) +
  labs(x = "", y = "", title = "Fractionalized counts") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

figure_1 <- bar_full | bar_frac + plot_layout(guides = "collect")
