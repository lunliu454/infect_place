library(readr)
library(EpiEstim)

#time window used to estimate Rt
window_wide <- 7  
#all_data <- read.csv("~/Documents/!Work/2020.7 Covid19/July/us_case_239counties.csv",
#                     stringsAsFactors = F)
all_data <- read.csv("us/us_case_346msa.csv",
                     stringsAsFactors = F)
all_data$add <- all_data$new_cases
all_data$add[all_data$add < 0] <- 0 #case report starts from second days
#all_data$eng <- as.factor(all_data$county_state)  #label each city or region in a country
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
  
    
  #jpeg(file=paste(city_name[i],"_plot.jpeg"))                                              #ploting                                           
  #plot(parameter)
  #dev.off()
  print(i)
}

#print("checkpoint")
write.table(all_together,"us/us_rt_239counties.csv",
            row.names=FALSE,
            col.names=TRUE,
            sep=",")   
write.table(all_together,"us/us_rt_346msa.csv",
            row.names=FALSE,
            col.names=TRUE,
            sep=",")  

  
  
  
  
  
  
  
  
