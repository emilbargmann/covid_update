# ---------------------------------------------------------------------------- #
# Title: Meta-Research: Author-level data confirm the widening gender gap in   #
# publishing rates during COVID-19                                             #
# Code: Regression analyses                                                    #
# Date: 10-03-2021                                                             #
# Author(s): Emil Bargmann Madsen & Jens Peter Andersen                        #
# E-mail: ebm@ps.au.dk & jpa@ps.au.dk                                          # 
# ---------------------------------------------------------------------------- #

list_of_packages <- c("tidyverse", "broom", "fixest")
lapply(list_of_packages, library, character.only = TRUE)


# 0. Data and package setup -----
load("author_data_cleaned.Rdata") # All data at individual author level
load("counterfactual_data_cleaned.Rdata") # Data for counterfactual time period


## 0.1 Gender variables (text and factor) ----
authors <- authors %>%
  mutate(gender_category = case_when(gender == "M" ~ "Male",
                                     gender == "F" ~ "Female",
                                     is.na(gender) ~ "Uncategorized"),
         gender_factor = factor(gender_category),
         gender_num = case_when(gender == "M" ~ 0,
                                gender == "F" ~ 1,
                                is.na(gender) ~ 2))

counterfactual <- counterfactual %>% 
  mutate(gender_category = case_when(gender == "M" ~ "Male",
                                     gender == "F" ~ "Female",
                                     is.na(gender) ~ "Uncategorized"),
         gender_factor = factor(gender_category),
         gender_num = case_when(gender == "M" ~ 0,
                                gender == "F" ~ 1,
                                is.na(gender) ~ 2))

## 0.2 Leaving out non-categorised authors ----

author_identified <- authors %>% 
  filter(gender_category != "Uncategorized") %>% 
  mutate(time_to_treat = as.integer(pub_year) - 2020, # Relative time variable
         time_factor = factor(time_to_treat),# As a factor
         cluster_factor = factor(cluster_id),
         country_factor = factor(country),
         oecd_factor = factor(oecd_minor_code),
         treat_dynamic = case_when(pub_year == "2020" & gender_num == 1 ~ 1,
                                   TRUE ~ 0),
         time_dynamic = case_when(gender_num == 0 ~ 0,
                                  gender_num == 1 ~ time_to_treat))

counterfactual_identified <- counterfactual %>% 
  filter(gender_category != "Uncategorized") %>% 
  mutate(time_to_treat = as.integer(pub_year) - 2020, # Relative time variable
         time_factor = factor(time_to_treat),# As a factor
         cluster_factor = factor(cluster_id),
         country_factor = factor(country),
         oecd_factor = factor(oecd_minor_code),
         treat_dynamic = case_when(pub_year == "2020" & gender_num == 1 ~ 1,
                                   TRUE ~ 0),
         time_dynamic = case_when(gender_num == 0 ~ 0,
                                  gender_num == 1 ~ time_to_treat))


# 1. Overall models -----

# Section 1 includes the overall models for all indentified authors underlying
# Figure 2 in the article. Furthermore, it includes full publication counts 
# modelled with poisson and negative binomial regression models. For linear
# models, we use both full publication counts and fractionalized counts, where
# each publication is divided by the number of authors, such that 
# n_pubs_frac = n_pubs/n_authors

## 1.1 Linear models ----

### Full counts ----
ols_model_full <- feols(n_pubs ~ i(time_to_treat, gender_num, ref = - 1) 
                                                  # Interaction term with ref.
                                                  # year = 2019
                        | cluster_id + pub_year, # Two-way fixed effects
                        cluster = ~ cluster_id, # Clustered standard errors
                        data = author_identified)

etable(ols_model_full, coefstat = "se") # get clustered standard error

etable(ols_model_full, coefstat = "confint", ci = 0.99) # get 99 % confidence interval

### Fractionalized counts ----
ols_model_frac <- feols(n_pubs_frac ~ i(time_to_treat, gender_num, ref = - 1)
                        | cluster_id + pub_year, 
                        cluster = ~ cluster_id,
                        data = author_identified)

etable(ols_model_frac, coefstat = "se") # get clustered standard error

etable(ols_model_frac, coefstat = "confint", ci = 0.99) # get 99 % confidence interval

## 1.2 Poisson models (only full counts) ----

pois_model_full <- feglm(n_pubs ~ i(time_to_treat, gender_num, ref = - 1)
                         | cluster_id + pub_year, 
                         cluster = ~ cluster_id, 
                         family = "poisson",
                         data = author_identified)

etable(pois_model_full, coefstat = "se", fitstat = c("apr2", "bic")) # get clustered standard error

etable(pois_model_full, coefstat = "confint", ci = 0.99, fitstat = c("apr2", "bic")) # get 99 % confidence interval

## 1.3 Negative binomial models (only full counts) ----

negbin_model_full <- fenegbin(n_pubs ~ i(time_to_treat, gender_num, ref = - 1) # Interaction term
                              | cluster_id + pub_year, 
                              cluster = ~ cluster_id, 
                              data = author_identified)

etable(negbin_model_full, coefstat = "se", fitstat = c("apr2", "bic", "theta")) # get clustered standard error

etable(negbin_model_full, coefstat = "confint", ci = 0.99, fitstat = c("apr2", "bic", "theta")) # get 99 % confidence interval


# 2. Career stage models -----

# Section 2 includes the subsample models underlying Figure 3 in the article.
# Again, we use both full publication counts and fractionalized counts, where
# each publication is divided by the number of authors, such that 
# n_pubs_frac = n_pubs/n_authors

## 2.1 Full publication counts ----

ols_model_full_sub <- feols(n_pubs ~ i(time_to_treat, gender_num, ref = - 1) # Interaction term
                            | cluster_id + pub_year, # Two-way fixed effects
                            cluster = ~ cluster_id, # Clustered standard errors
                            split = ~ sample_a, # Split by sample A dummy 
                                                #(e.g. 0 = Early career 
                                                # and 1 = Mid-career)
                            data = author_identified)

etable(ols_model_full_sub, coefstat = "se", fitstat = c("n", "rmse", "ar2", "wr2")) # get clustered standard error

etable(ols_model_full_sub, coefstat = "confint", ci = 0.99) # get 99 % confidence interval

## 2.2 Fractionalized publication counts ----

ols_model_frac_sub <- feols(n_pubs_frac ~ i(time_to_treat, gender_num, ref = - 1) 
                            | cluster_id + pub_year, 
                            cluster = ~ cluster_id, 
                            split = ~ sample_a,
                            data = author_identified)

etable(ols_model_frac_sub, coefstat = "se", fitstat = c("n", "rmse", "ar2", "wr2")) # get clustered standard error

etable(ols_model_frac_sub, coefstat = "confint", ci = 0.99) # get 99 % confidence interval


# 3. Productivity dependent models ----

m <- author_identified %>% filter(gender_num == 0)
f <- author_identified %>% filter(gender_num == 1)


m <- m %>%
  mutate(qo = ntile(n_pubs_total, 10))

f <- f %>%
  mutate(qo = ntile(n_pubs_total, 10))

author_identified_q <- rbind(m,f)

author_identified_q$q <- 0
author_identified_q$q[author_identified_q$qo <= 5] <- 1
author_identified_q$q[author_identified_q$qo <= 8 & author_identified_q$qo > 5] <- 2
author_identified_q$q[author_identified_q$qo == 9] <- 3
author_identified_q$q[author_identified_q$qo == 10] <- 4

author_identified_q %>% group_by(q) %>% summarize(n = n())

rm(ames,preds)

qs <- unique(author_identified_q$q)

for (i in 1:length(qs)) {
  qi <- qs[i]
  cat(paste("Calculating AME and pred. for ",i,". quantile...",sep=""))
  x <- author_identified_q %>% filter(q == qi)
  
  fit <- lm(n_pubs ~ time_factor*gender_num, data = x)
  
  pred <- make_predictions(fit, pred = "gender_num", pred.values = c(0, 1), # Pedicted publication numbers with clustered standard errors
                           at = list(time_factor = c("-4", "-3", "-2", "-1", "0")),
                           int.width = 0.999,
                           robust = "HC1", cluster = "cluster_id", 
                           data = x)
  
  vcov <- vcovCL(fit, cluster = x$cluster_id) 
  
  
  ame <- margins_summary(fit, variables = "gender_num", 
                         at = list(time_factor = c("-4", "-3", "-2", "-1", "0")), 
                         vcov = vcov,
                         level = 0.999)
  
  ame$q <- qi
  pred$q <- qi
  
  if (exists("ames")) {
    ames <- rbind(ames,ame)
    preds <- rbind(preds,pred)
  } else {
    ames <- ame
    preds <- pred
  }
  cat("done.\n")
}

# 4. Country-level regressions -----

country_sample <- author_identified %>% # Selecting 30 countries contributing
  dplyr::filter(pub_year == 2020) %>%   # 90 % of total authors
  group_by(country) %>% 
  summarise(n = n_distinct(cluster_id)) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  mutate(n_total = sum(n),
         n_pct = n/n_total,
         n_cumul = cumsum(n_pct)) %>% 
  filter(n_cumul < 0.905) # For 40 countries (95 % of sample) set to 0.95

df <- author_identified %>% # Data for only specified countries
  filter(country %in% country_sample$country) %>% 
  select(cluster_id, n_pubs, n_pubs_frac, gender_num, pub_year, time_to_treat, country)

df_nested <- df %>% # Nested data frame with data by country
  group_by(country) %>% 
  nest()

t_value <- 2.576 # 99 % (for 99.9 % insert 3.291)

## 4.1 Full publication counts ----

df_nested <- df_nested %>% # Loop over nested data frames using map
  mutate(
    models = map(data, ~ {feols(n_pubs ~ i(time_to_treat, gender_num, ref = - 1) # Interaction term
                                | cluster_id + pub_year, # Two-way fixed effects
                                cluster = ~ cluster_id,
                                data = .x)})) %>% 
  mutate( # Extract coefficients and standard errors
    coef_2016 = map_dbl(models, ~ .x$coeftable[1,1]),
    coef_2017 = map_dbl(models, ~ .x$coeftable[2,1]),
    coef_2018 = map_dbl(models, ~ .x$coeftable[3,1]),
    coef_2020 = map_dbl(models, ~ .x$coeftable[4,1]),
    se_2016 = map_dbl(models, ~ .x$coeftable[1,2]),
    se_2017 = map_dbl(models, ~ .x$coeftable[2,2]),
    se_2018 = map_dbl(models, ~ .x$coeftable[3,2]),
    se_2020 = map_dbl(models, ~ .x$coeftable[4,2])
  )

ols_model_full_country <- df_nested %>% # Data frame of country coefficients and
                                        # std. errors
  select(country, coef_2016:se_2020, n) %>% 
  pivot_longer(cols = coef_2016:se_2020, names_to = c(".value", "year"), names_pattern = "(.*)_(....)") %>% 
  mutate(ll = coef - (se*t_value), # Confidence intervals based on
         ul = coef + (se*t_value)) # clustered std. errors

## 4.2 Fractionalized publication counts

df_nested <- df_nested %>% # Loop over nested data frames using map
  mutate(
    models = map(data, ~ {feols(n_pubs_frac ~ i(time_to_treat, gender_num, ref = - 1) # Interaction term
                                | cluster_id + pub_year, # Two-way fixed effects
                                cluster = ~ cluster_id,
                                data = .x)})) %>% 
  mutate( # Extract coefficients and standard errors
    coef_2016 = map_dbl(models, ~ .x$coeftable[1,1]),
    coef_2017 = map_dbl(models, ~ .x$coeftable[2,1]),
    coef_2018 = map_dbl(models, ~ .x$coeftable[3,1]),
    coef_2020 = map_dbl(models, ~ .x$coeftable[4,1]),
    se_2016 = map_dbl(models, ~ .x$coeftable[1,2]),
    se_2017 = map_dbl(models, ~ .x$coeftable[2,2]),
    se_2018 = map_dbl(models, ~ .x$coeftable[3,2]),
    se_2020 = map_dbl(models, ~ .x$coeftable[4,2])
  )

ols_model_frac_country <- df_nested %>% # Data frame of country coefficients and
  # std. errors
  select(country, coef_2016:se_2020, n) %>% 
  pivot_longer(cols = coef_2016:se_2020, names_to = c(".value", "year"), names_pattern = "(.*)_(....)") %>% 
  mutate(ll = coef - (se*t_value), # Confidence intervals based on
         ul = coef + (se*t_value)) # clustered std. errors

# 5. Disciplinary-specific regression models -----

df <- author_identified %>% 
  dplyr::select(cluster_id, n_pubs, n_pubs_frac, gender_num, pub_year, time_to_treat, oecd)

df_nested <- df %>% 
  group_by(oecd) %>% 
  nest()

n_discipline <- author_identified %>% # Author count by discipline
  group_by(oecd) %>% 
  summarise(n = n_distinct(cluster_id))

## 5.1 Full publication counts ----

df_nested <- df_nested %>% 
  mutate(
    models = map(data, ~ {feols(n_pubs ~ i(time_to_treat, gender_num, ref = - 1) # Interaction term
                                | cluster_id + pub_year, # Two-way fixed effects
                                cluster = ~ cluster_id,
                                data = .x)})) %>% 
  mutate(
    coef_2016 = map_dbl(models, ~ .x$coeftable[1,1]),
    coef_2017 = map_dbl(models, ~ .x$coeftable[2,1]),
    coef_2018 = map_dbl(models, ~ .x$coeftable[3,1]),
    coef_2020 = map_dbl(models, ~ .x$coeftable[4,1]),
    se_2016 = map_dbl(models, ~ .x$coeftable[1,2]),
    se_2017 = map_dbl(models, ~ .x$coeftable[2,2]),
    se_2018 = map_dbl(models, ~ .x$coeftable[3,2]),
    se_2020 = map_dbl(models, ~ .x$coeftable[4,2])
  )

ols_model_full_discipline <- df_nested %>% 
  dplyr::select(oecd, coef_2016:se_2020) %>% 
  pivot_longer(cols = coef_2016:se_2020, names_to = c(".value", "year"), names_pattern = "(.*)_(....)") %>% 
  mutate(ll = coef - (se*t_value),
         ul = coef + (se*t_value)) %>% 
  left_join(n_discipline, by = "oecd")

## 5.1 Fractionalized publication counts ----

df_nested <- df_nested %>% 
  mutate(
    models = map(data, ~ {feols(n_pubs_frac ~ i(time_to_treat, gender_num, ref = - 1) # Interaction term
                                | cluster_id + pub_year, # Two-way fixed effects
                                cluster = ~ cluster_id,
                                data = .x)})) %>% 
  mutate(
    coef_2016 = map_dbl(models, ~ .x$coeftable[1,1]),
    coef_2017 = map_dbl(models, ~ .x$coeftable[2,1]),
    coef_2018 = map_dbl(models, ~ .x$coeftable[3,1]),
    coef_2020 = map_dbl(models, ~ .x$coeftable[4,1]),
    se_2016 = map_dbl(models, ~ .x$coeftable[1,2]),
    se_2017 = map_dbl(models, ~ .x$coeftable[2,2]),
    se_2018 = map_dbl(models, ~ .x$coeftable[3,2]),
    se_2020 = map_dbl(models, ~ .x$coeftable[4,2])
  )

ols_model_frac_discipline <- df_nested %>% 
  dplyr::select(oecd, coef_2016:se_2020) %>% 
  pivot_longer(cols = coef_2016:se_2020, names_to = c(".value", "year"), names_pattern = "(.*)_(....)") %>% 
  mutate(ll = coef - (se*t_value),
         ul = coef + (se*t_value)) %>% 
  left_join(n_discipline, by = "oecd")

# 6. Counterfactual samples and placebo tests

## 6.1 Models for counterfactual sample (2011-2015) ----

# Models underlying the effects and predictions in figure 3 - Supplement 2

### Full publication counts ----
ols_model_full_counter <- feols(n_pubs ~ i(time_to_treat, gender_num, ref = - 6) # Interaction term
                                | cluster_id + pub_year, # Two-way fixed effects
                                cluster = ~ cluster_id, # Clustered standard errors
                                data = counterfactual_identified)

etable(ols_model_full_counter, coefstat = "se") # get clustered standard error

etable(ols_model_full_counter, coefstat = "confint", ci = 0.99) # get 99 % confidence interval

### Fractionalized publication counts ----
ols_model_frac_counter <- feols(n_pubs_frac ~ i(time_to_treat, gender_num, ref = - 6) # Interaction term
                                | cluster_id + pub_year, # Two-way fixed effects
                                cluster = ~ cluster_id, # Clustered standard errors
                                data = counterfactual_identified)

etable(ols_model_frac_counter, coefstat = "se") # get clustered standard error

etable(ols_model_frac_counter, coefstat = "confint", ci = 0.99) # get 99 % confidence interval


## 6.2 Models for placebo tests ----

# Here we estimate four sets of placebo models. One for the difference between 
# 2017 and 2018, and one for 2018 vs. 2019, for both full and fractional
# publication counts. These are the models underlying figure 7A

### Linear models of full publication counts ----

ols_placebo_full_18 <- feols(n_pubs ~ i(time_to_treat, gender_num, ref = - 3) # 2017 as reference
                             | cluster_id + pub_year, # Two-way fixed effects
                             cluster = ~ cluster_id, # Clustered standard errors
                             data = subset(author_identified, pub_year != "2020"))

ols_placebo_full_19 <- feols(n_pubs ~ i(time_to_treat, gender_num, ref = - 2) # 2018 as reference
                             | cluster_id + pub_year, # Two-way fixed effects
                             cluster = ~ cluster_id, # Clustered standard errors
                             data = subset(author_identified, pub_year != "2020"))

### Linear models of fractionalized publication counts ----


ols_placebo_frac_18 <- feols(n_pubs_frac ~ i(time_to_treat, gender_num, ref = - 3) # 2017 as reference
                             | cluster_id + pub_year, # Two-way fixed effects
                             cluster = ~ cluster_id, # Clustered standard errors
                             data = subset(author_identified, pub_year != "2020"))

ols_placebo_frac_19 <- feols(n_pubs_frac ~ i(time_to_treat, gender_num, ref = - 2) # 2018 as reference
                             | cluster_id + pub_year, # Two-way fixed effects
                             cluster = ~ cluster_id, # Clustered standard errors
                             data = subset(author_identified, pub_year != "2020"))