library(dplyr)
library(stringr)

lads <- read.csv("uk/ukpopestimatesmid2020on2021geography.csv",
                 stringsAsFactors = F)
lads <- lads[ , 1:6]
colnames(lads)[4:6] <- c("area", "pop", "density")
lads <- lads %>% 
  mutate(area = str_replace_all(area, ",", "") %>% as.numeric(),
         pop = str_replace_all(pop, ",", "") %>% as.numeric(),
         density = str_replace_all(density, ",", "") %>% as.numeric()) %>% 
  filter(Geography %in% c("Metropolitan District", "Council Area", "Unitary Authority",
                          "London Borough", "Local Government District", "Non-metropolitan District"),
         pop > 100000)
cases <- read.csv("uk/summary_cases_byspecimen_date.csv",
                  stringsAsFactors = F)
cases <- cases %>% 
  select(areaName, LAD17CD, date, newCasesBySpecimenDate, RGN11NM, CTRY11NM) %>%
  mutate(date = as.Date(date)) %>%
  filter(areaName %in% lads$Name, date <= as.Date("2020-08-15"))
cases1 <- cases %>% filter(RGN11NM == 'London') %>%
  aggregate(newCasesBySpecimenDate ~ date, ., sum) %>%
  mutate(LAD17CD = 'E12000007', RGN11NM = 'London', CTRY11NM = 'England',
         areaName = 'London')
cases2 <- cases %>% filter(RGN11NM != 'London')
cases <- rbind(cases1, cases2)
cases <- mutate(cases, add = newCasesBySpecimenDate, eng = areaName)

# compute Rt ----
library(readr)
library(EpiEstim)
#time window used to estimate Rt
window_wide <- 7  
all_data <- cases
all_data$add[all_data$add < 0] <- 0 
num_of_city <- length(unique(all_data$eng))  #counting the number of cities of regions
city_name <- unique(all_data$eng)  #extracting city names
all_together <- data.frame()                                                               #constructing data frame
for (i in 1:num_of_city){
  city_data_frame <- subset(all_data, eng == city_name[i]) %>% arrange(date)                                #extracting one city once
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

#merge with intervention info
intv <- read.csv('uk/uk_intv.csv', stringsAsFactors = F)
intv$date <- as.Date(intv$date)
input <- merge(intv, all_together, by = c('date', 'LAD17CD', 'RGN11NM', 'CTRY11NM', 'areaName'))
input <- input %>% 
  mutate(cv = Rt_estimate_std / Rt_estimate, 
  log_win7_Rt_estimate = ifelse(cv <= 0.3, log(Rt_estimate), NA)) %>%
  select(-eng, -add, -X)

#add other info
info <- read.csv('uk/lad_info.csv', stringsAsFactors = F)
input <- merge(input, info, by = 'LAD17CD')
write.csv(input, 'uk_input.csv')
