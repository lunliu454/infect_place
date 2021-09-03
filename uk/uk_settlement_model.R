library(stringr)
library(dplyr)

output <- function(m){
  out <- data.frame(char = 0, Estimate = 0)
  for (i in 1 : length(m$coefficients)){
    out[i, 'char'] <- names(m$coefficients)[i]
    b <- format(m$coefficients[i], digits = 3)
    p <- summary(m)$coefficient[i, 4]
    # if (p <= 0.1 & p > 0.05){
    #   b <- paste0(b, '*')
    # }
    if (p <= 0.05 & p > 0.01){
      b <- paste0(b, '*')
    }
    if (p <= 0.01 & p > 0.001){
      b <- paste0(b, '**')
    }
    if (p <= 0.001){
      b <- paste0(b, '***')
    }
    se <- paste0('(', format(summary(m)$coefficient[i, 2], digits = 3), ')')
    out[i, 'Estimate'] <- paste(b, se, sep = '\n')
  }
  out[9 : 11, 'char'] <- c('Observations', 'R-squared', 'Adjusted R-squared')
  out[9 : 11, 'Estimate'] <- c(length(m$residuals), format(summary(m)$r.squared,  digits = 3), 
                               format(summary(m)$adj.r.squared, digits = 3))
  out
}

input <- read.csv("uk/uk_input.csv", stringsAsFactors = F)
lad <- read.csv("uk/lad_info.csv", stringsAsFactors = F)
m_uk <- lm(log_win7_Rt_estimate ~ win7_stay_at_home +
             win7_school_close + win7_childcare_close + win7_shop_close +
             win7_restaurant_close +
             win7_sports_indoor_close +
             areaName + as.factor(date) , input)
uf_uk <- m_uk$coefficients[grepl("areaName", names(m_uk$coefficients))]
uf_uk <- data.frame(lad = names(uf_uk), effect = unlist(uf_uk), row.names = NULL)
uf_uk$lad <- str_replace(uf_uk$lad, "areaName", "")
uf_uk[234, c('lad', 'effect')] <- c('Aberdeen City', 0)
lad <- lad %>% mutate(old = y65to74 + y75, den = density / 1000, gdppc = gdphead2019 / 1000,
                      pop = pop / 1000000) %>%
  select(areaName, pop, den, old, gdppc, asian, black)#gdppc thousand gbp
uf_uk <- merge(uf_uk, lad, by.x = "lad", by.y = "areaName")
m <- lm(effect ~ log(pop) + log(den) + old + gdppc + asian + black,
        filter(uf_uk, lad != 'Mendip')) # largest vif 3.07
#exclude outlier Mendip whose fe is large, pass normality test, log(pop) still sig
summary(m)
shapiro.test(m$residuals)
out_uk <- output(m)
