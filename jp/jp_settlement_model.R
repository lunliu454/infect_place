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

input <- read.csv("jp/jp_input.csv", stringsAsFactors = F)
pref <- read.csv('jp/pref_moreinfo.csv', stringsAsFactors = F)
pref <- pref %>% mutate(old = y65to74 + y75, den = largest_density / 1000, pop = mupop_dense / 1000000,
                        gdppc = gdppc * 1000, income = income / 1000) %>% #gdppc thousand usd, income thousand yen
  select(division_eng_name, pop, den, old, income, gdppc)
m_jp <- lm(log_win7_Rt_estimate~win7_stay_at_home+win7_school_close+ #
             win7_childcare_close+win7_office_close+win7_shop_close+
             win7_restaurant_close+win7_bar_close+ 
             win7_entertainment_close+win7_cultural_close+ win7_worship_close+
             win7_sports_indoor_close+
             win7_sports_outdoor_close+ win7_gathering_outside_10lower+ #
             win7_gathering_outside_10over+division_eng_name+as.factor(date), 
           input)
uf_jp <- m_jp$coefficients[grepl("division", names(m_jp$coefficients))]
#43 Aichi as reference, Iwate most Rt NA
uf_jp <- data.frame(pref = names(uf_jp), effect = unlist(uf_jp), row.names = NULL)
uf_jp$pref <- str_replace(uf_jp$pref, "division_eng_name", "")
uf_jp[44, ] <- c('Aichi', 0)
uf_jp <- merge(uf_jp, pref, by.x = "pref", by.y = "division_eng_name")
uf_jp$effect <- as.numeric(uf_jp$effect)
m <- lm(effect ~ log(pop) + log(den) + old + gdppc + income, 
        filter(uf_jp, pref != 'Shimane'))
#Shimane is an outlier with largest fe, if include log(pop) neg sig, since it is small pref
#largest vif 6.9
shapiro.test(m$residuals)
summary(m)
out_jp <- output(m)
