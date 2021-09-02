library(dplyr)
library(lfe)

input <- read.csv("us/us_input.csv", stringsAsFactors = F)
output <- data.frame(intervention = 1:9)
for (i in 1 : length(unique(input$MSA_STATE))){
  m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close +
              win7_entertainment_close + win7_cultural_close +
              win7_worship_close + win7_sports_indoor_close + 
              win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over |
              city_eng_name + as.factor(date) | 0 | city_eng_name, 
            filter(input, MSA_STATE != unique(input$MSA_STATE)[i]))
  output[ , ncol(output) + 1] <- m$beta[1 : 9]
  print(i)
}
output$intervention <- c('School', 'Childcare', 'Retail', 'Restaurant',
  'Bar', 'Entertainment', 'Culture', 'Religion', 'Sports indoor')
m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
            win7_shop_close + win7_restaurant_close + win7_bar_close +
            win7_entertainment_close + win7_cultural_close +
            win7_worship_close + win7_sports_indoor_close + 
            win7_stay_at_home +
            win7_gathering_outside_10lower + win7_gathering_outside_10over |
            city_eng_name + as.factor(date) | 0 | city_eng_name, input)
output$default <- m$beta[1 : 9]
write.csv(output, 'plot/sensi_withhold_us.csv')
