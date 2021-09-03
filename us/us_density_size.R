
library(dplyr)
library(lfe)

input <- read.csv("us/us_input.csv", stringsAsFactors = F)
get_max <- function(m){
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
  combined_result %>% 
    #filter(abs(t) > 1.96) %>% 
    top_n(-1, coefficient)
}
var <- c("win7_school_close", "win7_childcare_close",
         "win7_shop_close","win7_restaurant_close",
         "win7_bar_close","win7_entertainment_close", 
         "win7_cultural_close",
         "win7_sports_indoor_close")
n <- length(var)
l <- rep(list(0:1), n)
l <- expand.grid(l)

# on median ----
m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
            win7_shop_close + win7_restaurant_close + win7_bar_close +
            win7_entertainment_close + win7_cultural_close +
            win7_worship_close + win7_sports_indoor_close + 
            win7_stay_at_home +
            win7_gathering_outside_10lower + win7_gathering_outside_10over |
            city_eng_name + as.factor(date) | 0 | city_eng_name, 
          filter(input, urb_den > 681))
den_high <- get_max(m)

m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
            win7_shop_close + win7_restaurant_close + win7_bar_close +
            win7_entertainment_close + win7_cultural_close +
            win7_worship_close + win7_sports_indoor_close + 
            win7_stay_at_home +
            win7_gathering_outside_10lower + win7_gathering_outside_10over |
            city_eng_name + as.factor(date) | 0 | city_eng_name, 
          filter(input, urb_den <= 681))
den_low <- get_max(m)

m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
            win7_shop_close + win7_restaurant_close + win7_bar_close +
            win7_entertainment_close + win7_cultural_close +
            win7_worship_close + win7_sports_indoor_close + 
            win7_stay_at_home +
            win7_gathering_outside_10lower + win7_gathering_outside_10over |
            city_eng_name + as.factor(date) | 0 | city_eng_name, 
          filter(input, urb_pop > 174980))
pop_high <- get_max(m)

m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
            win7_shop_close + win7_restaurant_close + win7_bar_close +
            win7_entertainment_close + win7_cultural_close +
            win7_worship_close + win7_sports_indoor_close + 
            win7_stay_at_home +
            win7_gathering_outside_10lower + win7_gathering_outside_10over |
            city_eng_name + as.factor(date) | 0 | city_eng_name, 
          filter(input, urb_pop <= 174980))
pop_low <- get_max(m)
r <- rbind(den_low, den_high, pop_low, pop_high)
r$var <- c('den', 'den', 'pop', 'pop')
r$group <- c('low', 'high', 'low', 'high')
write.csv(r, 'plot/median_us.csv')

# continuous threshold ----
hetero_sensi_den <- data.frame(threshold = seq(600, 1000, 100), 
                               #threshold = seq(40, 120, 10), 
                               beta_denlow = 0, se_denlow = 0, 
                               beta_denhigh = 0, se_denhigh = 0)
for (k in 1 : nrow(hetero_sensi_den)){
  print(k)
  threshold <- seq(600, 1000, 100)[k]
  m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close +
              win7_entertainment_close + win7_cultural_close +
              win7_worship_close + win7_sports_indoor_close + 
              win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over |
              city_eng_name + as.factor(date) | 0 | city_eng_name, 
            filter(input, urb_den > threshold))
  t <- get_max(m)
  hetero_sensi_den$beta_denhigh[k] <- t$coefficient[1]
  hetero_sensi_den$se_denhigh[k] <- t$se[1]

  m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close +
              win7_entertainment_close + win7_cultural_close +
              win7_worship_close + win7_sports_indoor_close + 
              win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over |
              city_eng_name + as.factor(date) | 0 | city_eng_name, 
            filter(input, urb_den < threshold))
  t <- get_max(m)
  hetero_sensi_den$beta_denlow[k] <- t$coefficient[1]
  hetero_sensi_den$se_denlow[k] <- t$se[1]
}
write.csv(hetero_sensi_den, "plot/diff_den_us.csv")

hetero_sensi_pop <- data.frame(threshold = seq(90000, 470000, 10000),
                               beta_poplow = 0, se_poplow = 0,
                               beta_pophigh = 0, se_pophigh = 0)
for (k in 1 : nrow(hetero_sensi_pop)){
  print(k)
  threshold <- seq(90000, 470000, 10000)[k]
  m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close +
              win7_entertainment_close + win7_cultural_close +
              win7_worship_close + win7_sports_indoor_close + 
              win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over |
              city_eng_name + as.factor(date) | 0 | city_eng_name, 
            filter(input, urb_pop > threshold))
  t <- get_max(m)
  hetero_sensi_pop$beta_pophigh[k] <- t$coefficient[1]
  hetero_sensi_pop$se_pophigh[k] <- t$se[1]
  m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close +
              win7_entertainment_close + win7_cultural_close +
              win7_worship_close + win7_sports_indoor_close + 
              win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over |
              city_eng_name + as.factor(date) | 0 | city_eng_name, 
            filter(input, urb_pop < threshold))
  t <- get_max(m)
  hetero_sensi_pop$beta_poplow[k] <- t$coefficient[1]
  hetero_sensi_pop$se_poplow[k] <- t$se[1]
}
write.csv(hetero_sensi_pop, "plot/diff_pop_us.csv")
