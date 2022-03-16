# Figure 4

source("0_settings.R")

load("data/ame_results.Rdata")
load("data/pred_results.Rdata")

# Average marginal effects on full publication counts by quartile ----
  
qlabs <- c("1-50%", "51-80%", "81-90%", "91-100%")

ames_full <- ggplot(data = ames, aes(x = as.numeric(time_factor), y = AME, group = as.factor(q))) +
  geom_vline(xintercept = 4.5, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.factor(q)), alpha = 1/4) +
  geom_line(size = 1.5, aes(color = as.factor(q)))+
  geom_point(aes(color = as.factor(q)), shape = 21, fill = "white", size = 2.5, stroke = 2) +
  scale_x_continuous(label = c("2016", "2017", "2018", "2019", "2020")) +
  scale_y_continuous(limits = c(-2.1, 0.1), breaks = seq(-2.1, .1, by = .2)) +
  labs(x = "", y = "Avg. differences", title = "A. Differences in publication count") +
  scale_color_manual("Quantile", values = c(orange, maroon, olive, navy), labels = qlabs) +
  scale_fill_manual("Quantile", values = c(orange, maroon, olive, navy), labels = qlabs) +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0),
        legend.position = "bottom")

# Average predicted full publication counts by quantile ----
  
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
  geom_point(data = cf_trends, aes(x = pub_year, y = trend, color = as.factor(q)), size = 2.5, stroke = 2) +
  geom_point(aes(colour = as.factor(q)), shape = 21, fill = "white", size = 2.5, stroke = 2) +
  scale_x_continuous(label = c("2016", "2017", "2018", "2019", "2020")) +
  scale_y_continuous(limits = c(0, 6)) +
  scale_fill_manual("Quantile", values = c(orange, maroon, olive, navy), guide = "none") +
  scale_color_manual("Quantile", values = c(orange, maroon, olive, navy), guide = "none") +
  scale_linetype_discrete("Gender") +
  labs(x = "", y = "Avg. predicted\npublication counts", title = "B. Predicted publication counts") +
  theme_ebm_bar()+
  theme(axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom")

# Combined figure

figure_4 <- ames_full + preds_full