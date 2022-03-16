# Figure S4

source("0_settings.R")

load("data/figS4_data.Rdata")


# Full counts

ols_full_country_lockdown_sum <- ols_model_full_country %>% 
  filter(year == 2020) %>% 
  dplyr::select(-ends_with("_grp")) %>% 
  pivot_longer(c1_sum:c6_count_3, names_to = "lockdown_indicators", values_to = "severity") %>%
  separate(lockdown_indicators, c("indicator", "type")) %>% 
  mutate(indicator = case_when(indicator == "c1" ~ "School lockdowns",
                               indicator == "c2" ~ "Workplace lockdowns",
                               indicator == "c6" ~ "Stay at home requirements",
                               indicator == "stringency" ~ "Stringency index"),
         type = str_to_title(type)) %>% 
  filter(type == "Sum") %>% 
  ggplot(aes(x = severity, y = coef)) +
  geom_hline(yintercept = 0, size = 1, linetype = "dotted") +
  geom_linerange(aes(ymin = ll, ymax = ul), size = 1) +
  geom_point(shape = 21, fill = "white", size = 2, stroke = 1.5) +
  labs(x = "", y = "", title = "Full counts") +
  facet_wrap(~ indicator, scales = "free_x", ncol = 1) +
  theme_ebm_grid() +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"))

ols_full_country_lockdown_count <- ols_model_full_country %>% 
  filter(year == 2020) %>% 
  dplyr::select(-ends_with("_grp")) %>% 
  pivot_longer(c1_sum:c6_count_3, names_to = "lockdown_indicators", values_to = "severity") %>%
  separate(lockdown_indicators, c("indicator", "type")) %>% 
  mutate(indicator = case_when(indicator == "c1" ~ "School lockdowns",
                               indicator == "c2" ~ "Workplace lockdowns",
                               indicator == "c6" ~ "Stay at home requirements",
                               indicator == "stringency" ~ "Stringency index"),
         type = str_to_title(type)) %>% 
  filter(type == "Count") %>% 
  ggplot(aes(x = severity, y = coef)) +
  geom_hline(yintercept = 0, size = 1, linetype = "dotted") +
  geom_linerange(aes(ymin = ll, ymax = ul), size = 1) +
  geom_point(shape = 21, fill = "white", size = 2, stroke = 1.5) +
  labs(x = "", y = "", title = "Full counts") +
  facet_wrap(~ indicator, scales = "free_x", ncol = 1) +
  theme_ebm_grid() +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"))

# Fractionalized counts

ols_frac_country_lockdown_sum <- ols_model_frac_country %>% 
  filter(year == 2020) %>% 
  dplyr::select(-ends_with("_grp")) %>% 
  pivot_longer(c1_sum:c6_count_3, names_to = "lockdown_indicators", values_to = "severity") %>%
  separate(lockdown_indicators, c("indicator", "type")) %>% 
  mutate(indicator = case_when(indicator == "c1" ~ "School lockdowns",
                               indicator == "c2" ~ "Workplace lockdowns",
                               indicator == "c6" ~ "Stay at home requirements",
                               indicator == "stringency" ~ "Stringency index"),
         type = str_to_title(type)) %>% 
  filter(type == "Sum") %>% 
  ggplot(aes(x = severity, y = coef)) +
  geom_hline(yintercept = 0, size = 1, linetype = "dotted") +
  geom_linerange(aes(ymin = ll, ymax = ul), size = 1) +
  geom_point(shape = 21, fill = "white", size = 2, stroke = 1.5) +
  scale_y_continuous(position = "right") +
  labs(x = "Severity of lockdown indicator", y = expression("Gender" %*% "2020"), title = "Fractionalized counts") +
  facet_wrap(~ indicator, scales = "free_x", ncol = 1) +
  theme_ebm_grid() +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"))

ols_frac_country_lockdown_count <- ols_model_frac_country %>% 
  filter(year == 2020) %>% 
  dplyr::select(-ends_with("_grp")) %>% 
  pivot_longer(c1_sum:c6_count_3, names_to = "lockdown_indicators", values_to = "severity") %>%
  separate(lockdown_indicators, c("indicator", "type")) %>% 
  mutate(indicator = case_when(indicator == "c1" ~ "School lockdowns",
                               indicator == "c2" ~ "Workplace lockdowns",
                               indicator == "c6" ~ "Stay at home requirements",
                               indicator == "stringency" ~ "Stringency index"),
         type = str_to_title(type)) %>% 
  filter(type == "Count") %>% 
  ggplot(aes(x = severity, y = coef)) +
  geom_hline(yintercept = 0, size = 1, linetype = "dotted") +
  geom_linerange(aes(ymin = ll, ymax = ul), size = 1) +
  geom_point(shape = 21, fill = "white", size = 2, stroke = 1.5) +
  scale_y_continuous(position = "right") +
  labs(x = "", y = "", title = "Fractionalized counts") +
  facet_wrap(~ indicator, scales = "free_x", ncol = 1) +
  theme_ebm_grid() +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"))

# Combined figures

figure_S4_1 <- (ols_full_country_lockdown_sum + labs(x = "Severity of lockdown indicator", y = expression("Gender" %*% "2020")) | ols_frac_country_lockdown_sum)

figure_S4_2 <- (ols_full_country_lockdown_count + labs(x = "Severity of lockdown indicator", y = expression("Gender" %*% "2020")) | ols_frac_country_lockdown_count)
