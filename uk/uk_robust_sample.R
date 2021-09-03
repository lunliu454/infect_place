library(dplyr)
library(lfe)

input <- read.csv("uk/uk_input.csv", stringsAsFactors = F)
output <- data.frame(intervention = 1 : 5)
for (i in 1 : length(unique(input$areaName))){
  m <- felm(log_win7_Rt_estimate ~ win7_stay_at_home + win7_school_close +
              win7_childcare_close + win7_shop_close + win7_restaurant_close +
              win7_sports_indoor_close
            | areaName + as.factor(date) | 0 | areaName
            , filter(input, areaName != unique(input$areaName)[i]))
  output[ , ncol(output) + 1] <- m$beta[2 : 6]
  print(i)
}
output$intervention <- c('School', 'Childcare', 'Retail+\nLarge group in', 
                         'Restaurant+Culture+\nSmall group in',
                         'Sports indoor')
m <- felm(log_win7_Rt_estimate ~ win7_stay_at_home + win7_school_close +
            win7_childcare_close + win7_shop_close + win7_restaurant_close +
            win7_sports_indoor_close
          | areaName + as.factor(date) | 0 | areaName
          , input)
output$default <- m$beta[2 : 6]
write.csv(output, 'plot/sensi_withhold_uk.csv')
