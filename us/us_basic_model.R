library(dplyr)
library(lfe)

input <- read.csv("us/us_input.csv", stringsAsFactors = F)

#test correlation
t <- input %>%
  filter(!is.na(log_Rt_estimate)) %>% 
  select(colnames(input)[c(36 : 48, 50)])
corr <- cor(t, method = "kendall") %>% as.data.frame()

#model
m <- felm(log_Rt_estimate~win7_school_close + win7_childcare_close +
            win7_shop_close + win7_restaurant_close + win7_bar_close +
            win7_entertainment_close + win7_cultural_close +
            win7_worship_close + win7_sports_indoor_close + 
            win7_stay_at_home +
            win7_gathering_outside_10lower + win7_gathering_outside_10over |
            city_eng_name + as.factor(date) | 0 | city_eng_name, input)
summary(m)
# output for plot ####
est <- data.frame(intervention = c('School', 'Childcare', 'Retail', 'Restaurant',
                                   'Bar', 'Entertainment', 'Culture', 'Religion', 
                                   'Sports indoor'), 
                  effect = m$beta[1:9],
                  se_effect = m$cse[1:9])
write.csv(est, 'plot/base_us.csv', row.names = F)
# output table ####
out <- data.frame(Intervention = 0, Estimate = 0)
for (i in 1 : length(m$beta)){
  out[i, 'Intervention'] <- dimnames(m$beta)[[1]][i]
  b <- format(m$beta[i], digits = 3)
  p <- m$cpval[i]
  if (p <= 0.1 & p > 0.05){
    b <- paste0(b, '*')
  }
  if (p <= 0.05 & p > 0.01){
    b <- paste0(b, '**')
  }
  if (p <= 0.01){
    b <- paste0(b, '***')
  }
  se <- paste0('(', sprintf("%.3f", round(m$cse[i], 3)), ')')
  out[i, 'Estimate'] <- paste(b, se, sep = '\n')
}
out$Intervention <- c('School', 'Childcare', 'Retail', 'Restaurant',
                      'Bar', 'Entertainment', 'Culture', 'Religion', 
                      'Sports indoor','Stay-at-home', 'Small group out',
                      'Large group out')
out[13 : 15, 'Intervention'] <- c('Observations', 'R-squared', 'Adjusted R-squared')
out[13 : 15, 'Estimate'] <- c(m$N, 
                              sprintf("%.3f", round(summary(m)$r.squared, 3)), 
                              sprintf("%.3f", round(summary(m)$adj.r.squared, 3)))
write.csv(out, 'table/output_base_us.csv')
# joint effect ----
var <- c("win7_school_close", "win7_childcare_close",
         "win7_shop_close", "win7_restaurant_close",
         "win7_bar_close", "win7_entertainment_close",
         'win7_cultural_close', "win7_sports_indoor_close") #worship not pass common trend
n <- length(var)
l <- rep(list(0:1), n)
l <- expand.grid(l)
combined_result <- data.frame(combine = rep(0, 2 ^ n), 
                              coefficient = rep(0, 2 ^ n), 
                              se = rep(0, 2 ^ n),
                              t = rep(0, 2 ^ n))
for (i in 2 : 2 ^ n){
  index_on <- l[i,] %>% unlist()
  int_on <- var[index_on == 1]
  combined_result$combine[i] <- paste(int_on, collapse = "+")
  combined_result$coefficient[i] <- sum(m$beta[dimnames(m$beta)[[1]] %in% int_on])
  se <- m$clustervcv[row.names(m$clustervcv) %in% int_on, colnames(m$clustervcv) %in% int_on] %>% sum
  combined_result$se[i] <- sqrt(se)
  combined_result$t[i] <- combined_result$coefficient[i] / combined_result$se[i]
}
combined_result$number <- sapply(combined_result$combine, function(x){
  str_locate_all(x, "\\+")[[1]] %>% nrow + 1
})
write.csv(combined_result, "us/us_jointeffect_full.csv")
num_max <- combined_result %>% 
  group_by(number) %>% 
  top_n(-1, coefficient) %>%
  relocate(combine, number)
write.csv(num_max, "plot/combined_num_us.csv")
