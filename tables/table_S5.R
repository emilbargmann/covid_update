# Table S5

source("0_settings.R")

data <- import("tables/table_S5.txt", header = TRUE)

# HTML-table

table_S5 <- kbl(data, 
                col.names = c("", paste0("Coef. (S.E.)", footnote_marker_symbol(1)), "Pr(T \U2265 |t|)", paste0("Coef. (S.E.)", footnote_marker_symbol(2)), "Pr(T \U2265 |t|)"),
                align = c("l", "c", "c", "c", "c"),
                caption = "OLS linear regression of full and fractional count as dependent variable, placebo test of 2017 vs. 2018.",
                escape = FALSE,
                format = "html") %>% 
  kable_paper("hover", full_width = FALSE) %>% 
  add_header_above(c("", "Full count" = 2, "Fractional count" = 2)) %>% 
  footnote(general = "Linear regression with author and year fixed effects. Standard errors are HC1 and clustered at the author level.",
           number = "N<sub>observations</sub> = 2,041,260 and N<sub>authors</sub> = 408,252.",
           symbol = c("RMSE = 1.0719, R<sup>2</sup> = 0.5161, Within R<sup>2</sup> = 0.0001; ", "RMSE = 0.1961, R<sup>2</sup> = 0.4752, Within R<sup>2</sup> = 0.0001."),
           general_title = "Note: ", number_title = "Observations: ", symbol_title = "Fit statistics: ",
           footnote_as_chunk = TRUE,
           escape = FALSE)