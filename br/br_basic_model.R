library(dplyr)
library(lfe)

input <- read.csv("br/br_input.csv", stringsAsFactors = F)

m <- felm(log_win7_Rt_estimate ~ win7_stay_at_home + win7_school_close +
            win7_office_close + win7_shop_close + win7_restaurant_close +
            win7_bar_close + win7_entertainment_close+ 
            win7_cultural_close + win7_worship_close +
            win7_sports_indoor_close + win7_sports_outdoor_close +
            win7_gathering_outside_10lower + win7_gathering_outside_10over
          | code + as.factor(date) | 0 | code,
          input)
summary(m)
# output for plot ####
est <- data.frame(intervention = c('School+Childcare', 'Office', 'Retail', 'Restaurant',
                                   'Bar', 'Entertainment', 'Culture', 'Religion',
                                   'Sports indoor', 'Sports outdoor'), 
                  effect = m$beta[2:11],
                  se_effect = m$cse[2:11])
write.csv(est, 'plot/base_bra.csv', row.names = F)
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
out$Intervention <- c('School+Childcare', 'Office', 'Retail',
                      'Restaurant', 'Bar', 'Entertainment',
                      'Culture', 'Religion', 'Sports indoor',
                      'Sports outdoor', 'Stay-at-home', 'Small group in+out',
                      'Large group in+out')
out[14 : 16, 'Intervention'] <- c('Observations', 'R-squared', 'Adjusted R-squared')
out[14 : 16, 'Estimate'] <- c(m$N, 
                              sprintf("%.3f", round(summary(m)$r.squared, 3)), 
                              sprintf("%.3f", round(summary(m)$adj.r.squared, 3)))
write.csv(out, 'table/output_base_bra.csv')

# joint effect ----
var <- c("win7_school_close","win7_office_close",
         "win7_shop_close","win7_restaurant_close","win7_bar_close",
         "win7_entertainment_close", 
         "win7_cultural_close",
         "win7_sports_outdoor_close") #worship sportsindoor not pass common trend
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
write.csv(combined_result, "br/br_jointeffect_full.csv")
num_max <- combined_result %>% 
  group_by(number) %>% 
  top_n(-1, coefficient) %>%
  relocate(combine, number)
write.csv(num_max, "plot/combined_num_bra.csv")