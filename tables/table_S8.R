# Table S8

source("0_settings.R")

data <- import("tables/table_S8.txt", header = TRUE)

# HTML-table

table_S8 <- kbl(data, 
                col.names = c("", "Coef.", "S.E.", "t", "Pr(T \U2265 |t|)"),
                align = c("l", "c", "c", "c", "c"),
                caption = "OLS linear regression of counterfactual sample, with fractional count as dependent variable.",
                escape = FALSE) %>% 
  kable_paper("hover", full_width = FALSE) %>% 
  footnote(general = "Linear regression with author and year fixed effects. Standard errors are HC1 and clustered at the author level.",
           number = "N<sub>observations</sub> = 1,132,210 and N<sub>authors</sub> = 226,442.",
           symbol = "RMSE = 0.2080, Adj. R<sup>2</sup> = 0.2740, Within R<sup>2</sup> = 0.0008.",
           general_title = "Note: ", number_title = "Observations: ", symbol_title = "Fit statistics: ",
           footnote_as_chunk = TRUE,
           escape = FALSE,
           symbol_manual = c("", "", ""))