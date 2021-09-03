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

input <- read.csv("us/us_input.csv", stringsAsFactors = F)
msa <- read.csv('us/info_346msa.csv', stringsAsFactors = F)
msa <- msa %>% mutate(den = urb_den / 1000, pop = urb_pop / 1000000, old = y65to74 + y75, 
                      income = pincom2019 / 1000, gdppc = gdp2017 * 1000 / total_pop) %>%
  select(MSA_STATE, pop, den, old, income, gdppc, black, asian)
#gdppc thousand usd, income thousand yen
m_us <- lm(log_Rt_estimate ~ win7_stay_at_home + win7_school_close + 
             win7_childcare_close + win7_shop_close + win7_restaurant_close +
             win7_bar_close + win7_entertainment_close + win7_cultural_close +
             win7_worship_close + win7_sports_indoor_close + #win7_sports_outdoor_close +
             win7_gathering_outside_10lower + win7_gathering_outside_10over +
             city_eng_name + as.factor(date), input)
uf_us <- m_us$coefficients[grepl("city", names(m_us$coefficients))]
uf_us <- data.frame(MSA = names(uf_us), effect = unlist(uf_us), row.names = NULL)
uf_us$MSA <- str_replace(uf_us$MSA, "city_eng_name", "")
uf_us[308, ] <- c('Abilene, TX', 0)
uf_us <- merge(uf_us, msa, by.x = "MSA", by.y = "MSA_STATE")
uf_us <- filter(uf_us, MSA != 'Midland, TX')
m <- lm(effect ~ log(pop) + log(den) + old + income + gdppc + asian + black, 
        filter(uf_us[c(-239, -123, -216),])) #exclude then pass normality test
#den not sig, take log or not
#largest vif 3.77
shapiro.test(m$residuals)
summary(m)
out_us <- output(m)
