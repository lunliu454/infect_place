#city-county lookup list is from https://www3.epa.gov/ttnairs1/airsaqsORIG/manuals/city_names.pdf
#manually copied to excel
us_city_county <- read.csv("us/us_city_county_list.csv",
                           stringsAsFactors = F)
us_city_county <- filter(us_city_county, State.Name != "" & State.Name != "State Name ") %>%
  mutate(State.Name = str_to_title(State.Name) %>% substr(1, nchar(State.Name) - 1),
         City.Name = str_to_title(City.Name) %>% substr(1, nchar(City.Name) - 1),
         County.Name = str_to_title(County.Name) %>% substr(1, nchar(County.Name) - 1))
#write.csv(us_city_county, "~/Documents/!Work/2020.7 Covid19/!Data/us_city_county_cleaned.csv")

#city population ranking and land area is from wiki, copied using an online tool
#https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population
den <- read.table("us/us_city_density_wiki.txt", 
                  stringsAsFactors = F, sep=",", header = T)
den <- den %>% mutate(land = substr(X2016.land.area.1, 1, nchar(X2016.land.area.1) - 4)) %>%
  mutate(land = str_replace(land, ",", "") %>% as.numeric()) %>%
  mutate(pop = str_replace_all(X2020estimate, ",", "") %>% as.numeric()) %>%
  mutate(density = pop/land, State = State.c.) %>%
  select(City, State, land, pop, density)
den$City <- sapply(den$City, function(x){
  if (is.na(str_locate(x, '\\[')[1, 1])){x}else{substr(x, 1, str_locate(x, '\\[') - 1)}
})
den <- merge(den, us_city_county[,c(1, 4, 6)], 
             by.x = c("City", "State"),
             by.y = c("City.Name", "State.Name"), all.x = T)
# manually look up and fill in failed matches
den$County.Name[36] <- 'Ada'
den$County.Name[186] <- 'Jackson'
den$County.Name[188] <- 'Fayette'
den$County.Name[200] <- 'Hidalgo'
den$County.Name[201] <- 'Collin'
den$County.Name[207] <- 'Dade'
den$County.Name[222] <- 'Davidson'
den$County.Name[225] <- 'New York'
den$County.Name[289] <- 'Ramsey'
den$County.Name[315] <- 'St. Joseph'
den$County.Name[319] <- 'Spokane'
den$County.Name[323] <- 'St. Louis'
den$County.Name[324] <- 'Pinellas'
den$County.Name[357] <- 'Washington'
den$County.Name[362] <- 'Salt Lake'
den$County.Name[369] <- 'Forsyth'
#manually change inconsistent with input
den$County.Name[265] <- 'St. Lucie'
den$County.Name[280] <- 'Richmond'
den$County.Name[206] <- 'Miami-Dade'
den$County.Name[28] <- 'Baltimore'

#for counties having more than one city larger than 100,000
#use the density of the city with the largest population
den$county_state <- paste(den$County.Name, den$State, sep=",")
counties <- unique(den$county_state)
county_density <- data.frame(county_state = counties, city = 0, density = 0, pop = 0)
for (i in 1 : length(counties)){
  temp <- filter(den, county_state == counties[i])
  if (nrow(temp) == 1){
    county_density$density[i] <- temp$density[1]
    county_density$pop[i] <- temp$pop[1]
    county_density$city[i] <- temp$City[1]
  }else{
    county_density$density[i] <- temp$density[which.max(temp$pop)]
    county_density$pop[i] <- temp$pop[which.max(temp$pop)]
    county_density$city[i] <- temp$City[which.max(temp$pop)]
  }
}
write.csv(county_density, "~/Documents/!Work/2020.7 Covid19/July/us_county_density.csv")
