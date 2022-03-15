# Figure 6

source("0_settings.R")

load("data/bg_data.Rdata")

# Reference values from overall analysis
full_effect <- -0.0933
full_se <- 0.0055

full_counterfactual <- -0.0501
full_counterfactual_se <- 0.0064

t_value <- 2.576 # 99 % (for 99.9 % insert 3.291)

# Discipline analysis

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

# Histogram

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

# People per field and decile

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
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

# Combined figure

layout <- "
AAAAB
AAAAB
AAAAB
CCCCC
CCCCC
DDDDD
"

figure_6 <- ols_coef_full_discipline / n_discipline_hist / a_disc + guide_area() + 
  plot_layout(design = layout, guides = "collect") & 
  theme(legend.position = "bottom",
        legend.justification = c("left","bottom"),
        legend.box.just = "right",
        legend.box.margin = margin(0,0,0,0))

