library(tidyr)
library(dplyr)

#link with counties
#MSA-county lookup table is from https://apps.bea.gov/regional/docs/msalist.cfm?mlist=45
#https://apps.bea.gov/regional/docs/msalist.cfm
link <- read.csv("us/MSA_county_lookup.csv", header = F, stringsAsFactors = F,
                 col.names = c("MSA_code", "MSA_name", "county_code", "county_name"))
link <- link %>% 
  mutate(MSA_name = str_replace(MSA_name, " \\(Metropolitan Statistical Area\\)", ""),
         state = sapply(MSA_name, function(x){strsplit(x, ",")[[1]][2] %>% str_sub(2, -1)}),
         MSA_STATE = MSA_name,
         MSA_name = sapply(MSA_name, function(x){strsplit(x, ",")[[1]][1]}))
#to match, Puerto Rico not included in link, doesn't matter
link$MSA_name[link$MSA_name == "Minneapolis-St. Paul-Bloomington"] <- "Minneapolis-St Paul-Bloomington"
link$MSA_name[link$MSA_name == "Tampa-St. Petersburg-Clearwater"] <- "Tampa-St Petersburg-Clearwater"
link$MSA_STATE[link$MSA_STATE == "Minneapolis-St. Paul-Bloomington, MN-WI"] <- "Minneapolis-St Paul-Bloomington, MN-WI"
link$MSA_STATE[link$MSA_STATE == "Tampa-St. Petersburg-Clearwater, FL"] <- "Tampa-St Petersburg-Clearwater, FL"
link <- select(link, -state)
msa <- read.csv('us/msa_list.csv', stringsAsFactors = F)
msa_county <- merge(msa, link ,by = "MSA_STATE")
#match case number
case <- read.csv('us/daily_newly_cases_US.csv', stringsAsFactors = F)
case$Admin2[case$Admin2 == "Dona Ana"] <- "Doña Ana"
abbr <- read.csv("us/state_abbr.csv", stringsAsFactors = F)
case <- merge(case, abbr, by.x = "Province_State", by.y = "State.Name") #Puerto Rico Virgin not included
case <- case %>% mutate(county_state = paste(Admin2, USPS.Abbreviation, sep=', '))
msa_county <- msa_county %>% 
  mutate(county_name = str_replace(county_name, " \\(Independent City\\)| Municipality| Borough", ""))
for (i in 1 : nrow(msa_county)){
  a <- msa_county$county_name[i]
  if (grepl("\\+", a) & str_count(a, ",") == 1){
    msa_county[nrow(msa_county) + 1, ] <- msa_county[i, ]
    msa_county$county_name[i] <- paste(strsplit(a, "\\+")[[1]][1] %>% str_sub(1, -2),
                                       strsplit(a, ",")[[1]][2], sep = ",")
    msa_county$county_name[nrow(msa_county)] <- strsplit(a, "\\+")[[1]][2] %>% str_sub(2, -1)
  }
  if (grepl("\\+", a) & str_count(a, ",") == 2){
    msa_county[nrow(msa_county) + 1, ] <- msa_county[i, ]
    msa_county[nrow(msa_county) + 1, ] <- msa_county[i, ]
    msa_county$county_name[i] <- paste(strsplit(a, ",")[[1]][1], strsplit(a, ",")[[1]][3], sep = ",")
    msa_county$county_name[nrow(msa_county) - 1] <- paste(strsplit(a, "\\+|,")[[1]][2] %>% str_sub(2, -2),
                                                          strsplit(a, "\\+|,")[[1]][4], sep = ",")
    msa_county$county_name[nrow(msa_county)] <- strsplit(a, "\\+")[[1]][2] %>% str_sub(2, -1)
  }
} 
msa_county_case <- merge(msa_county, case, by.x = 'county_name', by.y = "county_state") #no DC in case 1109
msa_county_case <- msa_county_case %>% 
  select(-UID, -iso2, -iso3, -code3, -FIPS, -Country_Region, -Lat, -Long_, -Combined_Key)
msa_county_case_long <- gather(msa_county_case, date, new_cases, X2020.01.23:X2020.11.18)
msa_county_case_long$date <- substr(msa_county_case_long$date, 2, nchar(msa_county_case_long$date)) %>% as.Date("%Y.%m.%d")
msa_case <- aggregate(new_cases ~ MSA_STATE + + MSA + state + date, msa_county_case_long, sum)

# compute Rt ----
library(readr)
library(EpiEstim)
#time window used to estimate Rt
window_wide <- 7  
all_data <- msa_case
all_data$add <- all_data$new_cases
all_data$add[all_data$add < 0] <- 0 
all_data$eng <- as.factor(all_data$MSA_STATE)  #label each city or region in a country
num_of_city <- length(levels(all_data$eng))  #counting the number of cities of regions
city_name <- levels(all_data$eng)  #extracting city names
all_together <- data.frame()                                                               #constructing data frame
for (i in 1:num_of_city){
  city_data_frame <- subset(all_data, eng == city_name[i] )                                #extracting one city once
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
    parameter$R$`Std(R)`                                                                   #reporting mean and std of Rt
  
  #write.table(city_data_frame,paste(city_name[i],".csv"),row.names=FALSE,col.names=TRUE,sep=",")
  if (i == 1){
    all_together <- city_data_frame
  }else{
    all_together <- rbind(all_together,city_data_frame)
  }
  print(i)
}

#merge with intervention info ----
intv <- read.csv('us_intv.csv', stringsAsFactors = F)
intv$date <- as.Date(intv$date)
input <- merge(intv, all_together, by = c('state', 'date'))
msa_info <- read.csv('us/msa_info.csv', stringsAsFactors = F)
input <- input %>% filter(MSA_STATE %in% msa_info$MSA_STATE) %>%
  mutate(cv = Rt_estimate_std / Rt_estimate) %>% 
  mutate(log_Rt_estimate = log(Rt_estimate),
         log_Rt_estimate = ifelse(cv <= 0.3, log_Rt_estimate, NA),
         city_eng_name = MSA_STATE) %>%
  select(-X)
write.csv(input, 'us/us_input.csv')