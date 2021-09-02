library(dplyr)
library(lfe)

input <- read.csv("jp/jp_input.csv", stringsAsFactors = F)
var <- c('win7_school_close', 'win7_childcare_close', 'win7_shop_close',
         'win7_restaurant_close', 'win7_bar_close', 'win7_entertainment_close',
         'win7_cultural_close', 'win7_worship_close', 'win7_sports_indoor_close',
         'win7_sports_outdoor_close', "win7_stay_at_home",
          "win7_gathering_outside_10lower", "win7_gathering_outside_10over")
var_tocheck <- var[1 : 10]
var_name <- c('School', 'Childcare', 'Retail', 'Restaurant', 'Bar', 'Entertainment',
              'Culture', 'Religion', 'Sports indoor', 'Sports outdoor', 
              'Stay-at-home', 'Small group in+out', 'Large group in+out')
output <- data.frame(intervention = rep(var_tocheck, 16), 
                     exclude = rep(c(var, 'default', NA, NA), each = 10), 
                     b = 0, se = 0)
for (i in 1 : length(var)){
  form <- paste('log_win7_Rt_estimate ~ ', paste(var[-i], collapse = '+'),
                '| division_eng_name + as.factor(date) | 0 | division_eng_name') %>% 
    as.formula()
  m <- felm(formula = form, input)
  for (j in 1 : length(m$beta)){
    output$b[which(output$exclude == var[i] & 
                     output$intervention == dimnames(m$beta)[[1]][j])] <- m$beta[j]
    output$se[which(output$exclude == var[i] & 
                      output$intervention == dimnames(m$beta)[[1]][j])] <- m$cse[j]
  }
  print(i)
}
m <- felm(log_win7_Rt_estimate ~ win7_school_close + win7_childcare_close +
            win7_shop_close + win7_restaurant_close + win7_bar_close + 
            win7_entertainment_close + win7_cultural_close + 
            win7_worship_close + win7_sports_indoor_close +
            win7_sports_outdoor_close + win7_stay_at_home +
            win7_gathering_outside_10lower + win7_gathering_outside_10over 
            | division_eng_name + as.factor(date) | 0 | division_eng_name, input)
for (j in 1 : length(m$beta)){
  output$b[which(output$exclude == 'default' & 
                   output$intervention == dimnames(m$beta)[[1]][j])] <- m$beta[j]
  output$se[which(output$exclude == 'default' & 
                    output$intervention == dimnames(m$beta)[[1]][j])] <- m$cse[j]
}
output$intervention <- factor(output$intervention, levels = var[1 : 10])
output$exclude <- factor(output$exclude, levels = c('default', var))
output <- arrange(output, intervention, exclude)
output$exclude <- rep(c('Default', var_name, NA, NA), 10)
output <- filter(output, is.na(exclude) | b != 0)
output$intervention <- do.call(paste0, 
                               expand.grid(var_name[1 : 10], c(1 : 5, '', 7 : 15)) %>% arrange(Var1))

output1 <- mutate(output, b = ifelse(exclude == 'Default', b, NA),
                  se = ifelse(exclude == 'Default', se, NA))
output2 <- mutate(output, b = ifelse(exclude != 'Default', b, NA),
                  se = ifelse(exclude != 'Default', se, NA))
write.csv(output1, 'plot/sensi_var_jp1.csv')
write.csv(output2, 'plot/sensi_var_jp2.csv')




