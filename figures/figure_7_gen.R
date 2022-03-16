# Figure 7

source("0_settings.R")

load("data/fig7_data.Rdata")
load("data/gender_field_pos.Rdata")

# Figure of placebo DiD-estimates

figure_placebo <- placebo_estimates %>% 
  ggplot(aes(y = est, x = placebo)) +
  geom_hline(yintercept = 0, size = 1) +
  geom_linerange(aes(ymin = ll, ymax = ul), size = 1.5, position = position_dodge(width = 1), color = olive) +
  geom_point(shape = 21, fill = "white", size = 3, stroke = 2, position = position_dodge(width = 1), color = olive) +
  geom_text(aes(label = paste0("p = ", round(p, 3))), nudge_x = 0.3) +
  scale_y_continuous(n.breaks = 5) +
  facet_wrap(~ outcome_f, scales = "free_y") + 
  labs(x = "", y = expression(delta[placebo]), title = "A. Placebo tests") + 
  theme_ebm_bar() +
  theme(strip.text = element_text(face = "bold", size = 14),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
        axis.line.x = element_blank())



# Figure of development in first authorship

figure_ratio <- author_pos %>% 
  pivot_wider(names_from = "first_only", values_from = "n") %>% # N publications by author position
  rename(n_first = `1`, n_all = `0`) %>% 
  pivot_wider(names_from = "gender", values_from = c("n_first", "n_all")) %>% # N publications by gender
  mutate(n_total = n_all_F + n_all_M,
         n_total_first = n_first_F + n_first_M,
         ratio_first_F = n_first_F/n_total_first,
         ratio_all_F = n_all_F/n_total,
         ratio_of_ratio = ratio_first_F/ratio_all_F) %>% # Get total N publications, N total first-authored publications, and ratios of women's publications
  mutate(field = case_when(oecd_minor_code == "1.04" ~ "Chemistry",
                           oecd_minor_code == "1.06" ~ "Biology",
                           oecd_minor_code == "3.01" ~ "Basic medicine",
                           oecd_minor_code == "3.02" ~ "Clinical medicine")
  ) %>% 
  # Plot from here
  ggplot(aes(x = pub_year, y = ratio_of_ratio)) + 
  geom_vline(xintercept = 2019.5, linetype = "dotted", size = 1) +
  #geom_hline(yintercept = 1, size = 1) +
  geom_line(aes(colour = field), size = 1.5) +
  geom_point(shape = 21, fill = "white", size = 2.5, stroke = 2) +
  #scale_y_continuous(limits = c(1.125, 1.25)) +
  scale_color_manual("Discipline", values = c(orange, maroon, olive, navy)) +
  labs(x = "", y = expression(p[women]^first / p[women]^all), title = "B. Share of women first authors") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Combined figure

figure_7 <- figure_placebo / figure_ratio

