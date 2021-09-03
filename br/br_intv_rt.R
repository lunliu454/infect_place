library(dplyr)
library(stringr)

#cities with more than 100,000 population from wikipedia
wiki <- read.csv("br/brazil_city_info_wiki.csv", stringsAsFactors = F)
colnames(wiki) <- c("rank_pop", "city_chn_name", "city_eng_name", "state", 
                    "pop", "area", "density")
wiki <- wiki %>% mutate(pop = str_replace_all(pop, ",", "") %>% as.numeric(),
                        area = str_replace_all(area, ",", "") %>% as.numeric(),
                        density = str_replace_all(density, ",", "") %>% as.numeric())
wiki <- wiki %>% mutate(division_acro = case_when(state=="圣保罗州" ~ "SP",
                                                  state=="里约热内卢州" ~ "RJ",state=="圣保罗州" ~ "SP",state=="联邦区" ~ "DF",
                                                  state=="巴伊亚州" ~ "BA",state=="塞阿拉州" ~ "CE",state=="米纳斯吉拉斯州" ~ "MG",
                                                  state=="亚马孙州" ~ "AM",state=="巴拉那州" ~ "PR",state=="伯南布哥州" ~ "PE",
                                                  state=="戈亚斯州" ~ "GO",state=="帕拉州" ~ "PA",state=="南里奥格兰德州" ~ "RS",
                                                  state=="马拉尼昂州" ~ "MA",state=="阿拉戈斯州" ~ "AL",state=="南马托格罗索州" ~ "MS",
                                                  state=="北里约格朗德州" ~ "RN",state=="皮奥伊州" ~ "PI",state=="帕拉伊巴州" ~ "PB",
                                                  state=="塞尔希培州" ~ "SE",state=="马托格罗索州" ~ "MT",state=="圣卡塔琳娜州" ~ "SC",
                                                  state=="朗多尼亚州" ~ "RO",state=="圣埃斯皮里图州" ~ "ES",state=="阿马帕州" ~ "AP",
                                                  state=="罗赖马州" ~ "RR",state=="阿克雷州" ~ "AC",state=="托坎廷斯州" ~ "TO"))
wiki <- wiki %>% mutate(division_local_name = case_when(state=="圣保罗州" ~ "São Paulo",
                                                        state=="里约热内卢州" ~ "Rio de Janeiro",state=="圣保罗州" ~ "São Paulo",
                                                        state=="联邦区" ~ "Distrito Federal",state=="巴伊亚州" ~ "Bahia",
                                                        state=="塞阿拉州" ~ "Ceará",state=="米纳斯吉拉斯州" ~ "Minas Gerais",
                                                        state=="亚马孙州" ~ "Amazonas",state=="巴拉那州" ~ "Paraná",
                                                        state=="伯南布哥州" ~ "Pernambuco",state=="戈亚斯州" ~ "Goiás",
                                                        state=="帕拉州" ~ "Pará",state=="南里奥格兰德州" ~ "Rio Grande do Sul",
                                                        state=="马拉尼昂州" ~ "Maranhão",state=="阿拉戈斯州" ~ "Alagoas",
                                                        state=="南马托格罗索州" ~ "MatoGrosso do Sul",state=="北里约格朗德州" ~ "Rio Grande do Norte",
                                                        state=="皮奥伊州" ~ "Piauí",state=="帕拉伊巴州" ~ "Paraíba",
                                                        state=="塞尔希培州" ~ "Sergipe",state=="马托格罗索州" ~ "MatoGrosso",
                                                        state=="圣卡塔琳娜州" ~ "Santa Catarina",state=="朗多尼亚州" ~ "Rondônia",
                                                        state=="圣埃斯皮里图州" ~ "Espírito Santo",state=="阿马帕州" ~ "Amapá",
                                                        state=="罗赖马州" ~ "Roraima",state=="阿克雷州" ~ "Acre",
                                                        state=="托坎廷斯州" ~ "Tocantins"))
#case data
#please unzip brazil_covid19_cities.csv.zip
case <- read.csv("br/brazil_covid19_cities.csv")
city <- merge(wiki, case[,c("state","name","code")] %>% distinct(), 
                  by.x = c("city_eng_name", "division_acro"),
                  by.y = c("name", "state"))
city <- select(city, -city_chn_name, -state)
case <- merge(case, city, by.x = c("name","state"), 
               by.y=c("city_eng_name","division_acro"))
case$name <- droplevels(case$name)
case <- case %>% 
  mutate(code = code.x) %>%
  select(-code.x, -code.y) 
case <- case %>% mutate(date = as.character(date) %>% as.Date()) %>% 
  group_by(code) %>%
  arrange(date) %>%
  mutate(add = cases - lag(cases))
# compute rt ----
library(readr)
library(EpiEstim)
#time window used to estimate Rt
window_wide <- 7  
all_data <- case
all_data$add[all_data$add < 0] <- 0 
num_of_city <- length(unique(all_data$code))  #counting the number of cities of regions
city_name <- unique(all_data$code)  #extracting city names
all_together <- data.frame()                                                               #constructing data frame
for (i in 1:num_of_city){
  city_data_frame <- subset(all_data, code == city_name[i]) %>% arrange(date) %>% arrange(date)                              #extracting one city once
  num_of_days <- nrow(city_data_frame)                                                     #days of observation
  city_data_frame$add <- as.numeric(city_data_frame$add)                                   #make sure that the data type is "numeric"
  day_case <- city_data_frame$add                                                          #everyday case of one city or region
  day_case[(day_case < 0) |(is.na(day_case))] <- 0                                         #maker sure that everyday case is nonnegative 
  parameter <- estimate_R(day_case,method="parametric_si",
                          config = make_config(list(mean_si = 7.5, 
                                                    std_si = 3.4,
                                                    t_start = seq(2, num_of_days-window_wide, 1), 
                                                    t_end = seq(2, num_of_days-window_wide, 1) + window_wide
                          )))                                    #Rt estimate
  city_data_frame$Rt_estimate <- NA                                                        #data_frame initialization
  city_data_frame$Rt_estimate[seq(2, num_of_days-window_wide, 1) + window_wide ] <-
    parameter$R$`Mean(R)`
  city_data_frame$Rt_estimate_std <- NA
  city_data_frame$Rt_estimate_std[seq(2, num_of_days-window_wide, 1) + window_wide ] <-
    parameter$R$`Std(R)`                                                              
  if (i == 1){
    all_together <- city_data_frame
  }else{
    all_together <- rbind(all_together,city_data_frame)
  }
  print(i)
}

# merge with intervention info
intv <- read.csv('br/br_intv.csv', stringsAsFactors = F)
intv <- intv[, -1]
intv$date <- as.Date(intv$date)
input <- merge(all_together, intv, by = c('division_local_name', 'date'))
input <- input %>% 
  mutate(cv = Rt_estimate_std/Rt_estimate,
         log_win7_Rt_estimate = ifelse(cv <= 0.3, log(Rt_estimate), NA),
         division_code = substr(code, 1, 2) %>% as.numeric())
write.csv(input, 'br/br_input.csv') 
