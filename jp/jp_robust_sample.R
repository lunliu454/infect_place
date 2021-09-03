library(dplyr)
library(lfe)

input <- read.csv("jp/jp_input.csv", stringsAsFactors = F)
output <- data.frame(intervention = 1 : 10)
for (i in 1 : length(unique(input$division_eng_name))){
  m <- felm(log_win7_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close + 
              win7_entertainment_close + win7_cultural_close + 
              win7_worship_close + win7_sports_indoor_close +
              win7_sports_outdoor_close + win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over
            | division_eng_name + as.factor(date) | 0 | division_eng_name, 
            filter(input, division_eng_name != unique(input$division_eng_name)[i]))
  output[ , ncol(output) + 1] <- m$beta[1 : 10]
  print(i)
}
output$intervention <- c('School', 'Childcare', 'Retail', 'Restaurant',
                         'Bar', 'Entertainment', 'Culture', 'Religion', 'Sports indoor',
                         'Sports outdoor')
m <- felm(log_win7_Rt_estimate ~ win7_school_close + win7_childcare_close +
            win7_shop_close + win7_restaurant_close + win7_bar_close + 
            win7_entertainment_close + win7_cultural_close + 
            win7_worship_close + win7_sports_indoor_close +
            win7_sports_outdoor_close + win7_stay_at_home +
            win7_gathering_outside_10lower + win7_gathering_outside_10over
          | division_eng_name + as.factor(date) | 0 | division_eng_name, input)
output$default <- m$beta[1 : 10]
write.csv(output, 'plot/sensi_withhold_jp.csv')
