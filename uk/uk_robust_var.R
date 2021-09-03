library(dplyr)
library(lfe)

input <- read.csv("uk/uk_input.csv", stringsAsFactors = F)
var <- c('win7_school_close', 'win7_childcare_close', 
         'win7_shop_close', 'win7_restaurant_close', 'win7_sports_indoor_close',
         'win7_stay_at_home')
var_tocheck <- var[1 : 5]
var_name <- c('School', 'Childcare', 'Retail+\nLarge group in', 
              'Restaurant+Culture+\nSmall group in', 'Sports indoor', 'Stay-at-home')
output <- data.frame(intervention = rep(var_tocheck, 10), 
                     exclude = rep(c(var, 'default', 'add', NA, NA), each = 5), 
                     b = 0, se = 0)
for (i in 1 : length(var)){
  form <- paste('log_win7_Rt_estimate ~ ', paste(var[-i], collapse = '+'),
                '| areaName + as.factor(date) | 0 | areaName') %>% as.formula()
  m <- felm(formula = form, input)
  for (j in 1 : length(m$beta)){
    output$b[which(output$exclude == var[i] & 
                     output$intervention == dimnames(m$beta)[[1]][j])] <- m$beta[j]
    output$se[which(output$exclude == var[i] & 
                     output$intervention == dimnames(m$beta)[[1]][j])] <- m$cse[j]
  }
  print(i)
}
m <- felm(log_win7_Rt_estimate ~ win7_stay_at_home + win7_school_close +
            win7_childcare_close + win7_shop_close + win7_restaurant_close +
            win7_sports_indoor_close | areaName + as.factor(date) | 0 | areaName, input)
for (j in 1 : length(m$beta)){
  output$b[which(output$exclude == 'default' & 
                   output$intervention == dimnames(m$beta)[[1]][j])] <- m$beta[j]
  output$se[which(output$exclude == 'default' & 
                    output$intervention == dimnames(m$beta)[[1]][j])] <- m$cse[j]
}
m <- felm(log_win7_Rt_estimate ~ win7_stay_at_home + win7_school_close +
            win7_childcare_close + win7_shop_close + win7_restaurant_close +
            win7_sports_indoor_close + win7_wear_mask | areaName + as.factor(date) | 0 | areaName, input)
for (j in 1 : length(m$beta)){
  output$b[which(output$exclude == 'add' & 
                   output$intervention == dimnames(m$beta)[[1]][j])] <- m$beta[j]
  output$se[which(output$exclude == 'add' & 
                    output$intervention == dimnames(m$beta)[[1]][j])] <- m$cse[j]
}
output$intervention <- factor(output$intervention, levels = var[1 : 5])
output$exclude <- factor(output$exclude, levels = c('default', var, 'add'))
output <- arrange(output, intervention, exclude)
output$exclude <- rep(c('Default', var_name, 'Add', NA, NA), 5)
output <- filter(output, is.na(exclude) | b != 0)
output$intervention <- do.call(paste0, 
         expand.grid(var_name[1 : 5], c(1 : 3, '', 5 : 9)) %>% arrange(Var1))

output1 <- mutate(output, b = ifelse(exclude == 'Default', b, NA),
                  se = ifelse(exclude == 'Default', se, NA))
output2 <- mutate(output, b = ifelse(exclude != 'Default', b, NA),
                  se = ifelse(exclude != 'Default', se, NA))
write.csv(output1, 'plot/sensi_var_uk1.csv')
write.csv(output2, 'plot/sensi_var_uk2.csv')

