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

input <- read.csv("br/br_input.csv", stringsAsFactors = F)
mu <- read.csv('br/br_info.csv')
mu <- mu %>% mutate(den = density / 1000, old = y60, income = income_median / 1000, #in thousand BRL 
                    pop = pop / 1000000, gdppc = gdppc / 1000) %>% #in 1000 rila
  select(code, mu, state, pop, den, old, income, gdppc)
m_br <- lm(log_win7_Rt_estimate ~ win7_stay_at_home + win7_school_close +
             win7_office_close + win7_shop_close + win7_restaurant_close +
             win7_bar_close + win7_entertainment_close + win7_cultural_close +
             win7_worship_close + win7_sports_indoor_close + win7_sports_outdoor_close +
             win7_gathering_outside_10lower + win7_gathering_outside_10over + 
             as.factor(code) + as.factor(date), input)
uf_br <- m_br$coefficients[grepl("code", names(m_br$coefficients))]
uf_br <- data.frame(mu_code = names(uf_br), effect = unlist(uf_br), row.names = NULL,
                    stringsAsFactors = F)
uf_br$mu_code <- str_replace(uf_br$mu_code, "as.factor\\(code\\)", "") %>% as.numeric
uf_br[319, ] <- c(130190, 0)
uf_br <- merge(uf_br, mu, by.x = "mu_code", by.y = "code")
m <- lm(effect ~ log(pop) + log(den) + old + income + gdppc, uf_br) 
#largest vif 1.48
shapiro.test(m$residuals)
summary(m)
#if exclude Itumbiara or cities > 1 million, then density not sig, but pop always sig
out_br <- output(m)
