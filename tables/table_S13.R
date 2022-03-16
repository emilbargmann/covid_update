# Table S13

source("0_settings.R")

data <- import("tables/table_S13.txt", header = TRUE)

# HTML-table
table_S13 <- kbl(data, 
                 col.names = c("", "Coef. 2016 (S.E.)", "Coef. 2017 (S.E.)", "Coef. 2018 (S.E.)", "Coef. 2020 (S.E.)"),
                 align = c("l", "c", "c", "c", "c"),
                 caption = "Coefficients and standard errors relative to 2019 for the 30 countries with most authors in the dataset.",
                 escape = FALSE) %>% 
  kable_paper("hover", full_width = FALSE) %>% 
  footnote(general = "Linear regression with author and year fixed effects. Standard errors are HC1 and clustered at the author level.",
           footnote_as_chunk = TRUE)