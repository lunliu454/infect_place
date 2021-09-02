
library(dplyr)
library(lfe)

input <- read.csv("br/br_input.csv", stringsAsFactors = F)
input$date <- as.Date(input$date)
input <- arrange(input, code, date)
ind_var <- c('win7_stay_at_home',
             'win7_school_close',
             'win7_office_close',
             'win7_shop_close',
             'win7_restaurant_close',
             "win7_bar_close",
             'win7_entertainment_close',
             'win7_cultural_close',
             'win7_worship_close',
             'win7_sports_indoor_close',
             'win7_sports_outdoor_close',
             'win7_gathering_outside_10lower',
             'win7_gathering_outside_10over')
var_tocheck <- ind_var[2 : 11]
pretrend <- data.frame(var = rep(0, length(var_tocheck)), 
                       beta_pre1 = 0, se_pre1 = 0, p_pre1 = 0, 
                       beta_pre2 = 0, se_pre2 = 0, p_pre2 = 0, 
                       beta_pre3 = 0, se_pre3 = 0, p_pre3 = 0, 
                       beta_pre4 = 0, se_pre4 = 0, p_pre4 = 0, 
                       beta_pre5 = 0, se_pre5 = 0, p_pre5 = 0)
for (k in 1 : length(var_tocheck)){
  i <- var_tocheck[k]
  pretrend$var[k] <- i
  if ("firstvalue" %in% colnames(input)){
    input <- select(input, -firstvalue)
  }
  create_change_before <- sapply(0:15, function(x){
    paste0(i, "_b", x, "= lag(", i , ",", x, 
           ") - lag(", i, ",", x + 1, ")")
  })  %>%
    paste(collapse = ",")
  create_change_before <- paste("group_by(input, code) %>% mutate(", create_change_before, ")")
  input <- eval(parse(text = create_change_before))
  firstvalue <- paste0("firstvalue <- input %>% group_by(code) %>% 
                       filter(date == min(date)) %>% 
                       select(date,", i, ", code) %>% 
                       mutate(firstvalue =", i, ")")
  firstvalue <- eval(parse(text = firstvalue))
  input <- merge(input, firstvalue[ , c("code", "firstvalue")], by = "code")
  create_b16 <- paste0("input %>% group_by(code) %>% arrange(date) %>% mutate(", 
                       i, "_b16 =  lag(", i, ", 16) - firstvalue)")
  input <- eval(parse(text = create_b16))
  change_before_nato0 <- sapply(0:16, function(x){
    paste0(i, "_b", x, "= ifelse(is.na(", i, "_b", 
           x, "), 0, ", i, "_b", x, ")")})  %>%
    paste(collapse = ",")
  change_before_nato0 <- paste("mutate(input, ", change_before_nato0, ")")
  input <- eval(parse(text = change_before_nato0))

  for (j in 1 : 5){
    create_change_after <- 
      paste0(i, "_a = lead(", i, ", j) -", i)
    create_change_after <- paste("group_by(input, code) %>% mutate(", create_change_after, ")")
    input <- eval(parse(text = create_change_after))
    
    create_change_after <- 
      paste0(i, "_a2 = lead(", i, ", 5) - lead(", i, ",j)") ### !!!
    create_change_after <- paste("group_by(input, code) %>% mutate(", create_change_after, ")")
    input <- eval(parse(text = create_change_after))
    
    change_after_nato0 <- paste0(i, "_a = ifelse(is.na(", i, "_a), 0, ", i, "_a)")
    change_after_nato0 <- paste("mutate(input, ", change_after_nato0, ")")
    input <- eval(parse(text = change_after_nato0))
    
    change_after_nato0 <- paste0(i, "_a2 = ifelse(is.na(", i, "_a2), 0, ", i, "_a2)")
    change_after_nato0 <- paste("mutate(input, ", change_after_nato0, ")")
    input <- eval(parse(text = change_after_nato0))
    
    formula_part1 <- sapply(0 : 16, function(x){
      paste0(i, "_b", x)
    }) %>% 
      paste(collapse = "+")
    
    formula_part2 <- paste0(i, "_a+", i, "_a2")
    formula <- paste("log_win7_Rt_estimate ~ ",
                     paste(ind_var[ind_var != i], collapse = "+"),
                     '+', formula_part1, "+", formula_part2,
                     "| code + as.factor(date) | 0 | code") %>%
      as.formula()
    m <- felm(formula = formula, input)

    var <- paste0(i, '_a')
    pretrend[k, j * 3 - 1] <- m$beta[dimnames(m$beta)[[1]] == var]
    pretrend[k, j * 3] <- m$cse[names(m$cse) == var]
    pretrend[k, j * 3 + 1] <- m$cpval[names(m$pval) == var]
  }
}

# output table ####
out <- data.frame(Intervention = 0, '1 day' = 0, '2 days' = 0, '3 days' = 0,
                  '4 days' = 0, '5 days' = 0)
for (i in 1 : nrow(pretrend)){
  out[i, 'Intervention'] <- pretrend$var[i]
  for (j in 1 : 5){
    b <- format(pretrend[i, 3 * j - 1], digits = 3)
    p <- pretrend[i, 3 * j + 1]
    if (p <= 0.05 & p > 0.01){
      b <- paste0(b, '*')
    }
    if (p <= 0.01 & p > 0.001){
      b <- paste0(b, '**')
    }
    if (p <= 0.001){
      b <- paste0(b, '***')
    }
    
    se <- paste0('(', format(pretrend[i, 3 * j], digits = 3), ')')
    out[i, j + 1] <- paste(b, se, sep = '\n')
  }
}
out$Intervention <- c('School+Childcare', 'Office', 'Retail', 'Restaurant',
                      'Bar', 'Entertainment', 'Culture', 
                      'Religion', 'Sports indoor', 'Sports outdoor')
write.csv(out, 'br/pretrend_bra.csv')

