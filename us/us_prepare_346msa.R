library(dplyr)
library(stringr)
library(lfe)
library(car)

setwd("~/Documents/!Work/2020.7 Covid19/2github")
source("intervention_status_code.R")

input_july <- read.csv("us/us_input_239counties.csv", stringsAsFactors = F)
input_july <- input_july %>% 
  select(-X.1, -city_eng_name, -Admin2, -county_state, -Rt_estimate, -Rt_estimate_std,
         -cv, -log_Rt_estimate, -X, -density, -pop) %>%
  distinct()
abbr <- read.csv("us/state_abbr.csv", stringsAsFactors = F)
input_july <- merge(input_july, abbr, by.x = "division_eng_name", by.y = "State.Name", all.x = T) 
input_july$USPS.Abbreviation[is.na(input_july$USPS.Abbreviation)] <- "DC"
msa <- read.csv("us/info_346msa.csv", stringsAsFactors = F)
msa$state_dom <- str_sub(msa$state, 1, 2)
rt <- read.csv("us/us_rt_346msa.csv", stringsAsFactors = F)
rt <- select(rt, -X, -state, -CBSA_code, -MSA)
input <- merge(msa, rt, by = "MSA_STATE")
input <- merge(input, input_july, 
               by.x = c("state_dom", "date"),  #!!!
               by.y = c("USPS.Abbreviation", "date")) #end up with 275 msa
input <- input %>% 
  mutate(cv = Rt_estimate_std / Rt_estimate) %>% 
  mutate(log_Rt_estimate = log(Rt_estimate),
         log_Rt_estimate = ifelse(cv <= 0.3, log_Rt_estimate, NA),
         city_eng_name = MSA_STATE,
         date = as.Date(date))
  
control <- c('win7_stay_at_home',
             'win7_school_close',
             'win7_childcare_close',
             'win7_shop_close',
             'win7_restaurant_close',
             'win7_bar_close',
             'win7_entertainment_close',
             "win7_cultural_close",
             "win7_worship_close",
             'win7_sports_indoor_close',
             'win7_sports_outdoor_close',
             'win7_gathering_outside_10lower',
             'win7_gathering_outside_10over',
             "win7_wear_mask")
cities <- unique(input$city_eng_name)
#code intervention status to 0, 0.5 or 1
input <- input %>% group_by(city_eng_name) %>% arrange(date)
for (p in control){
  for (i in cities){
    input[[p]][which(input$city_eng_name == i)] <- input[[p]][which(input$city_eng_name == i)] %>%
                                                   intervention_status_code()
  }
}
setwd("~/Documents/!Work/2020.7 Covid19/July")
write.csv(input, "us/us_input_msa2.csv")

info <- select(input, MSA_STATE, total_pop, urb_pop, urb_den) %>% distinct
cor.test(input$urb_pop, input$pincom2019)#0.26
cor.test(input$urb_pop, input$gdp2017 / input$total_pop)#0.29
cor.test(input$pincom2019, input$gdp2017 / input$total_pop)#0.8

#0.5 to 1
# for (p in control[2:13]){
#     input[[p]][which(input[[p]] == 0.5)] <- 1
# }

# correlation ####
t <- input %>%
  filter(!is.na(log_Rt_estimate)) %>% 
  select(control)
t <- cor(t[2:15], method = "kendall")
t <- as.data.frame(t)
write.csv(t, "~/Documents/!Work/2020.7 Covid19/July/us_corr_3levels.csv")

#0/1
# m <- felm(log_Rt_estimate~win7_stay_at_home+win7_school_close+
#             win7_childcare_close+
#             win7_shop_close+
#             win7_restaurant_close+
#             win7_bar_close+win7_entertainment_close+
#             #win7_cultural_close+
#             ## culture=ent=sports_in
#             #win7_worship_close+
#             #win7_sports_indoor_close+
#             win7_sports_outdoor_close+
#             win7_gathering_outside_10lower+win7_gathering_outside_10over|
#             city_eng_name+as.factor(date)|0|city_eng_name,
#           #input)
#           filter(input, density<1500)) #
# summary(m)
# basic model ####
#0/0.5/1
m <- felm(log_Rt_estimate~win7_stay_at_home+win7_school_close+
            win7_childcare_close+
            win7_shop_close+
            win7_restaurant_close+
            win7_bar_close+win7_entertainment_close+
            #win7_cultural_close+
            ## culture=ent
            win7_worship_close+
            win7_sports_indoor_close+win7_sports_outdoor_close+
            win7_gathering_outside_10lower+win7_gathering_outside_10over
          + win7_wear_mask|
            city_eng_name+as.factor(date)|0|city_eng_name, input)
summary(m)
lm(log_Rt_estimate~win7_stay_at_home+win7_school_close+
     win7_childcare_close+
     win7_shop_close+
     win7_restaurant_close+
     win7_bar_close+win7_entertainment_close+
     win7_cultural_close+
     ## culture=ent
     win7_worship_close+
     win7_sports_indoor_close+
     win7_sports_outdoor_close+
     win7_gathering_outside_10lower+win7_gathering_outside_10over,
   input) %>% vif()
# basic model 0.5 code--no ####
var <- c('win7_stay_at_home',
         'win7_school_close',
         'win7_childcare_close',
         'win7_shop_close',
         'win7_restaurant_close',
         'win7_bar_close',
         'win7_entertainment_close',
         "win7_worship_close",
         'win7_sports_indoor_close',
         'win7_sports_outdoor_close',
         'win7_gathering_outside_10lower',
         'win7_gathering_outside_10over')
sensi_code <- sapply(var, function(x){paste(x, "= 0")}) %>% paste(collapse = ",")
sensi_code <- paste("data.frame(setting = 1 : 1000, ", sensi_code, ")")
sensi_code <- eval(parse(text = sensi_code))
for (i in 1 : 1000){
  codes <- sample(seq(0.1, 0.9, 0.1), 12, replace = T)
  temp <- sapply(1 : 12, function(x){
    paste(var[x], " = ifelse(", var[x], "== 0.5,", codes[x], ",", var[x], ")")
  }) %>% paste(collapse = ",")
  temp <- paste("mutate(input,", temp, ")")
  temp <- eval(parse(text = temp))
  m <- felm(log_Rt_estimate~win7_stay_at_home+win7_school_close+
              win7_childcare_close+
              win7_shop_close+
              win7_restaurant_close+
              win7_bar_close+win7_entertainment_close+
              win7_worship_close+
              win7_sports_indoor_close+win7_sports_outdoor_close+
              win7_gathering_outside_10lower+win7_gathering_outside_10over|
              city_eng_name+as.factor(date)|0|city_eng_name, temp)
  sensi_code[i, 2:13] <- m$beta
  print(i)
}
hist(sensi_code$win7_school_close)


# event study single ####
ind_var <- c("win7_stay_at_home", "win7_school_close", "win7_childcare_close",
             "win7_shop_close", "win7_restaurant_close", "win7_bar_close",
             "win7_sports_indoor_close", "win7_sports_outdoor_close",
             "win7_gathering_outside_10lower", "win7_gathering_outside_10over")
for (i in ind_var){
  if ("firstvalue" %in% colnames(input)){
    input <- select(input, -firstvalue)
  }
  create_change_before <- sapply(0:15, function(x){
    paste0(i, "_b", x, "= lag(", i , ",", x, 
           ") - lag(", i, ",", x + 1, ")")
  })  %>%
    paste(collapse = ",")
  create_change_before <- paste("group_by(input, city_eng_name) %>% mutate(", create_change_before, ")")
  input <- eval(parse(text = create_change_before))
  firstvalue <- paste0("firstvalue <- input %>% group_by(city_eng_name) %>% 
                       filter(date == min(date)) %>% 
                       select(date,", i, ", city_eng_name) %>% 
                       mutate(firstvalue =", i, ")")
  firstvalue <- eval(parse(text = firstvalue))
  input <- merge(input, firstvalue[ , c("city_eng_name", "firstvalue")], by = "city_eng_name")
  create_b16 <- paste0("input %>% group_by(city_eng_name) %>% arrange(date) %>% mutate(", 
                       i, "_b16 =  lag(", i, ", 16) - firstvalue)")
  input <- eval(parse(text = create_b16))
  change_before_nato0 <- sapply(0:16, function(x){
    paste0(i, "_b", x, "= ifelse(is.na(", i, "_b", 
           x, "), 0, ", i, "_b", x, ")")})  %>%
    paste(collapse = ",")
  change_before_nato0 <- paste("mutate(input, ", change_before_nato0, ")")
  input <- eval(parse(text = change_before_nato0))
  
  create_change_after <- sapply(1:5, function(x){
    paste0(i, "_a", x, "= lead(", i, ",", x, 
           ") - lead(", i, ",", x - 1, ")")
  })  %>%
    paste(collapse = ",")
  create_change_after <- paste("group_by(input, city_eng_name) %>% mutate(", create_change_after, ")")
  input <- eval(parse(text = create_change_after))
  change_after_nato0 <- sapply(1:5, function(x){
    paste0(i, "_a", x, "= ifelse(is.na(", i, "_a", 
           x, "), 0, ", i, "_a", x, ")")})  %>%
    paste(collapse = ",")
  change_after_nato0 <- paste("mutate(input, ", change_after_nato0, ")")
  input <- eval(parse(text = change_after_nato0))
  
  formula_part1 <- sapply(0:16, function(x){
    paste0(i, "_b", x)
  }) %>% 
    paste(collapse = "+")
  formula_part2 <- sapply(1:5, function(x){
    paste0(i, "_a", x)
  }) %>% 
    paste(collapse = "+")
  formula <- paste("log_Rt_estimate ~ ",
                   paste(control[!control %in% c(i, "avg_temp", "win7_cultural_close", "win7_entertainment_close")], 
                         collapse = "+"),
                   '+', formula_part1, "+", formula_part2,
                   "| city_eng_name + as.factor(date) | 0 | city_eng_name") %>%
    as.formula()
  m <- felm(formula = formula, filter(input, pop>200000))
  print(formula)
  print(summary(m))
}
#0/1
#density > 1000, childcare not common trend, pre 5+
#density < 1000, out_10over, pre 1+
#pop < 20w, res, pre 2-
#pop > 20w, sport_out pre 3-, shop pre 2-, childcare pre 2-

# 



# withholding samples ####
cities <- unique(input$city_eng_name)
cols <- sapply(cities, function(x){paste0("'", x, "' =  rep(0, 512)")}) %>% 
  paste(collapse = ",")
cols <- paste("data.frame(combine = rep(0, 512), baseline = rep(0, 512), baseline_p = rep(0, 512),", 
              cols, ")")
combined_result <- eval(parse(text = cols))

for (i in 2:512){
  index_on <- l[i,] %>% unlist()
  int_on <- interventions[index_on == 1]
  combine <- paste("mutate(input, temp =(", paste(int_on, collapse = "+"), 
                   ")/", length(int_on), ")")
  input <- eval(parse(text = combine))
  formula <- paste("log_win7_Rt_estimate ~ ",
                   paste(c("avg_temp", interventions[!interventions %in% int_on]), collapse = "+"),
                   "+temp | city_eng_name + as.factor(date) | 0 | city_eng_name") %>% as.formula()
  t1 <- sapply(int_on, function(x){paste(x, "==0")}) %>% paste(collapse = "&")
  t2 <- sapply(int_on, function(x){paste(x, "==1")}) %>% paste(collapse = "&")
  temp <- paste("filter(input, (", t1, ") | (", t2, "))")
  temp <- eval(parse(text = temp))
  m <- felm(formula = formula, temp)
  combined_result$combine[i] <- paste(int_on, collapse = "+")
  combined_result$baseline[i] <- m$beta[dimnames(m$beta)[[1]] == "temp"]
  combined_result$baseline_p[i] <- m$cpval[names(m$cpval) == "temp"]
  for (j in cities){
    temp2 <- filter(temp, city_eng_name != j)
    m <- felm(formula = formula, temp2)
    j <- str_replace_all(j, c("," = ".", "-" = ".", " " = "."))
    combined_result[i, colnames(combined_result) == j] <- m$beta[dimnames(m$beta)[[1]] == "temp"]
    print(paste(j,i))
  }
}
combined_result$num <- str_count(combined_result$combine, pattern = "\\+") + 1
write.csv(combined_result, "twfe_combined_sensiti1.csv")


m<-lm(log_Rt_estimate~win7_stay_at_home+win7_school_close+
        win7_childcare_close+win7_shop_close+
        win7_restaurant_close+
        win7_bar_close+#win7_entertainment_close+
        #win7_cultural_close+
        win7_sports_indoor_close+win7_sports_outdoor_close+
        win7_gathering_outside_10lower+win7_gathering_outside_10over,input)
vif(m)

m<-lm(log_win7_Rt_estimate~avg_temp+temp+city_eng_name+as.factor(date),input)
input <- mutate(input,temp=win7_office_close+win7_cultural_close+win7_sports_indoor_close)
m<-lm(log_win7_Rt_estimate~avg_temp+temp+
        city_eng_name+as.factor(date),input)


formula <- paste("log_win7_Rt_estimate ~ ",
                 paste(control[!control %in% c("win7_sports_indoor_close", "win7_restaurant_close",
                                               "win7_cultural_close", "win7_entertainment_close")], 
                       collapse = "+"),
                 '+', formula_part1, "+", formula_part2) %>%
  as.formula()
m <- lm(formula = formula, input)
summary(m)
vif(m)
t <- select(input, control[2:17])
t <- cor(t, method = "kendall")

#check
t <- paste("mutate(input, temp=", formula_part1, "+", formula_part2,")")
input <- eval(parse(text = t))
t

