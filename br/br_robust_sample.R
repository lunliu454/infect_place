library(dplyr)
library(lfe)

input <- read.csv("br/br_input.csv", stringsAsFactors = F)
output <- data.frame(intervention = 1 : 10)
for (i in 1 : length(unique(input$code))){
  m <- felm(log_win7_Rt_estimate ~ win7_stay_at_home + win7_school_close +
              win7_office_close + win7_shop_close + win7_restaurant_close +
              win7_bar_close + win7_entertainment_close+ 
              win7_cultural_close + win7_worship_close +
              win7_sports_indoor_close + win7_sports_outdoor_close +
              win7_gathering_outside_10lower + win7_gathering_outside_10over
            | code + as.factor(date) | 0 | code
            , filter(input, code != unique(input$code)[i]))
  output[ , ncol(output) + 1] <- m$beta[2 : 11]
  print(i)
}
output$intervention <- c('School+Childcare', 'Office', 'Retail', 'Restaurant',
                         'Bar', 'Entertainment', 'Culture', 'Religion', 'Sports indoor',
                         'Sports outdoor')
m <- felm(log_win7_Rt_estimate ~ win7_stay_at_home + win7_school_close +
            win7_office_close + win7_shop_close + win7_restaurant_close +
            win7_bar_close + win7_entertainment_close+ 
            win7_cultural_close + win7_worship_close +
            win7_sports_indoor_close + win7_sports_outdoor_close +
            win7_gathering_outside_10lower + win7_gathering_outside_10over
          | code + as.factor(date) | 0 | code, input)
output$default <- m$beta[2 : 11]
write.csv(output, 'plot/sensi_withhold_bra.csv')
