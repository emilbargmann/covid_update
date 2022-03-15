# Table S2

source("0_settings.R")

data <- import("tables/table_S2.txt", header = TRUE)

# HTML-table

table_S2 <- kbl(data, 
               col.names = c("", "Coef.", "S.E.", "t", "Pr(T \U2265 |t|)"),
               align = c("l", "c", "c", "c", "c"),
               caption = "OLS linear regression with fractional count as dependent variable",
               escape = FALSE) %>% 
  kable_paper("hover", full_width = FALSE) %>% 
  footnote(general = "Linear regression with author and year fixed effects. Standard errors are HC1 and clustered at the author level.",
           number = "N<sub>observations</sub> = 2,041,260 and N<sub>authors</sub> = 408,252.",
           symbol = "RMSE = 0.2114, R<sup>2</sup> = 0.4770, Within R<sup>2</sup> = 0.0003.",
           general_title = "Note: ", number_title = "Observations: ", symbol_title = "Fit statistics: ",
           footnote_as_chunk = TRUE,
           escape = FALSE,
           symbol_manual = c("", "", ""))