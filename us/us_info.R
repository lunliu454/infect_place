library(tidyr)
library(dplyr)
# check city pop in county pop ####
#the county population table is downloaded from 
#https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv
cpop <- read.csv("us/co-est2019-alldata.csv", stringsAsFactors = F)
cpop <- cpop %>% 
  select(STNAME, CTYNAME, POPESTIMATE2019) %>% 
  mutate(TYPE = case_when(grepl("County", CTYNAME) ~ "county", 
                          grepl("city", CTYNAME) ~ "city"),
         CTYNAME = str_replace(CTYNAME, " County| city| Parish| Municipality", ""), 
         name = paste(CTYNAME, STNAME, sep = ','))
#t <- table(cpop$name) %>% as.data.frame()
#cpop <- merge(cpop, t, by.x = "name", by.y = "Var1")
cpop$name[cpop$name == "District of Columbia,District of Columbia"] <- "Washington,District of Columbia"
cpop$name[cpop$name == "DeKalb,Georgia"] <- "De Kalb,Georgia"
cpop$name[1835] <- "Dona Ana,New Mexico"
cpop$name[cpop$name == "DuPage,Illinois"] <- "Du Page,Illinois"
cpop$name[cpop$name == "McClain,Oklahoma"] <- "Mc Clain,Oklahoma"
cpop$name[cpop$name == "McLennan,Texas"] <- "Mc Lennan,Texas"
cpop <- cpop %>% filter(!(name == "Oklahoma,Oklahoma" & is.na(TYPE)) &
                        !(name == "New York,New York" & is.na(TYPE)) &
                        !(name == "Baltimore,Maryland" & TYPE == "county") &
                        !(name == "Richmond,Virginia" & TYPE == "county") &
                        !(name == "St. Louis,Missouri" & TYPE == "county") &
                        !(name == "Utah,Utah" & is.na(TYPE)))
cpop <- distinct(cpop)

input <- read.csv("us/us_input_239counties.csv", stringsAsFactors = F)
cpop <- filter(cpop, name %in% input$city_eng_name)
citypop <- input %>% select(city_eng_name, density, pop) %>% distinct
cpop <- merge(cpop, citypop, by.x = "name", by.y = "city_eng_name")
cpop$ratio <- cpop$pop / cpop$POPESTIMATE2019

# use MSA ####

#urban area pop and density is from https://www2.census.gov/geo/docs/reference/ua/
#https://www2.census.gov/geo/docs/reference/ua/ua_list_all.txt
ua <- read.csv("us/ua_list_all.csv", stringsAsFactors = F)
ua <- ua %>% 
  select(NAME, POP, AREALAND, AREALANDSQMI, POPDEN) %>%
  mutate(state = sapply(NAME, function(x){strsplit(x, ",")[[1]][2] %>% str_sub(2, -1)}),
         NAME = sapply(NAME, function(x){strsplit(x, ",")[[1]][1]}))
colnames(ua) <- sapply(colnames(ua), function(x){paste0("ua_", x)})

#MSA population is from https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-metro-and-micro-statistical-areas.html#par_textimage_1139876276
#first table
msa <- read.csv("us/msa_pop_2019.csv", stringsAsFactors = F)
msa <- msa %>% 
  mutate(MSA = str_replace_all(MSA, "\\.| Metro Area", ""), 
         pop2019 = str_replace_all(pop2019, ",", "") %>% as.numeric,
         state = sapply(MSA, function(x){strsplit(x, ",")[[1]][2] %>% str_sub(2, -1)}),
         MSA = sapply(MSA, function(x){strsplit(x, ",")[[1]][1]})) %>%
  filter(!grepl("Division", state), pop2019 > 100000) 
msa_ua <- data.frame()
for (i in 1 : nrow(msa)){
  a <- msa$MSA[i]
  pattern1 <- strsplit(a,"-")[[1]] %>% paste(collapse = "|")
  b <- msa$state[i]
  pattern2 <- strsplit(b,"-")[[1]] %>% paste(collapse = "|")
  match <- filter(ua, grepl(pattern1, ua_NAME), grepl(pattern2, ua_state))
  if (nrow(match) > 0){
    match <- cbind(msa[i, ], match)
    msa_ua <- rbind(msa_ua, match)
  }
}
msa <- msa_ua %>%
  group_by(MSA, state) %>%
  summarise(total_pop = mean(pop2019), urb_pop = sum(ua_POP), urb_land = sum(ua_AREALAND),
            urb_landsqmi = sum(ua_AREALANDSQMI), state_dom = ua_state[which(ua_POP == max(ua_POP))]) %>%
  mutate(ratio = urb_pop / total_pop, urb_den = urb_pop * 1000000 / urb_land)

#add age (2010 census)
#from https://www.census.gov/library/publications/2012/dec/c2010sr-01.html
#Chapter 5 data
age <- read.csv("us/msa_age_2010.csv", stringsAsFactors = F)
age$MSA[age$MSA == "Texarkana, TX-Texarkana, AR"] <- "Texarkana, TX-AR"
age <- age[4:nrow(age), 1:11] %>%
  mutate_at(vars(colnames(.)[4:11]), function(x){str_replace_all(x, ",", "") %>% as.numeric}) %>%
  mutate(total = rowSums(.[4:11]),
         state = sapply(MSA, function(x){strsplit(x, ",")[[1]][2] %>% str_sub(2, -1)}),
         MSA_state = MSA,
         MSA = sapply(MSA, function(x){strsplit(x, ",")[[1]][1]}))
age[, 4 : 11] <- age[, 4 : 11] / age[[12]]

msa$name_2010match <- paste(msa$MSA, msa$state, sep = ", ")
#left Arecibo, PR unmatched
msa$name_2010match[msa$name_2010match == "California-Lexington Park, MD"] <- "Lexington Park, MD"
msa$name_2010match[msa$name_2010match == "Myrtle Beach-Conway-North Myrtle Beach, SC-NC"] <- "Myrtle Beach-North Myrtle Beach-Conway, SC"
msa$name_2010match[msa$name_2010match == "Prescott Valley-Prescott, AZ"] <- "Prescott, AZ"
msa$name_2010match[msa$name_2010match == "Urban Honolulu, HI"] <- "Honolulu, HI"
msa$name_2010match[msa$name_2010match == "Weirton-Steubenville, WV-OH"] <- "Steubenville-Weirton, OH-WV"
msa$name_2010match[msa$name_2010match == "Worcester, MA-CT"] <- "Worcester, MA"
msa$name_2010match[msa$name_2010match == "Salisbury, MD-DE"] <- "Salisbury, MD"
msa$name_2010match[msa$name_2010match == "Fayetteville-Springdale-Rogers, AR"] <- "Fayetteville-Springdale-Rogers, AR-MO"
for (i in 1 : nrow(msa)){
  if (!msa$name_2010match[i] %in% age$MSA_state){
    a <- strsplit(msa$MSA[i], "-")[[1]][1]
    match <- age$MSA_state[grepl(a, age$MSA) & age$state == msa$state[i]]
    #match <- age$MSA[grepl(a, age$MSA)]
    if (length(match) == 1){
      msa$name_2010match[i] <- age$MSA_state[grepl(a, age$MSA) & age$state == msa$state[i]]
      } else {msa$name_2010match[i] <- length(match)}
  }
}
age <- select(age, -MSA, -state)
msa <- merge(msa, age, by.x = "name_2010match", by.y = "MSA_state") #352 to 351
msa$MSA_STATE <- paste(msa$MSA, msa$state, sep = ", ")

#add income
#from https://www.bea.gov/data/income-saving/personal-income-county-metro-and-other-areas
#Metropolitan Area Table
inc <- read.csv("us/msa_pincome_2019.csv", stringsAsFactors = F)
inc$MSA[231] <- "Minneapolis-St Paul-Bloomington, MN-WI"
inc$MSA[343] <- "Tampa-St Petersburg-Clearwater, FL"
inc$pincom2019 <- str_replace_all(inc$pincom2019, ",", "") %>% as.numeric
msa <- merge(msa, inc, by.x = "MSA_STATE", by.y = "MSA") #lose 4 Puerto Rico, to 347

#add gdp
#from https://www.bea.gov/news/2018/gross-domestic-product-metropolitan-area-2017
gdp <- read.csv("us/msa_gdp_2017.csv", stringsAsFactors = F)
gdp <- gdp %>% 
  mutate(gdp2017 = str_replace_all(gdp2017, ",", "") %>% as.numeric,
         state = sapply(MSA, function(x){strsplit(x, ",")[[1]][2] %>% str_sub(2, -1)}))
msa$name_2017match <- msa$MSA_STATE
msa$name_2017match[msa$name_2017match == "Fayetteville-Springdale-Rogers, AR"] <- "Fayetteville-Springdale-Rogers, AR-MO"
msa$name_2017match[msa$name_2017match == "Prescott Valley-Prescott, AZ"] <- "Prescott, AZ"
for (i in 1 : nrow(msa)){
  if (!msa$name_2017match[i] %in% gdp$MSA){
    a <- strsplit(msa$MSA[i], "-")[[1]][1]
    match <- gdp$MSA[grepl(a, gdp$MSA) & gdp$state == msa$state[i]]
    #match <- age$MSA[grepl(a, age$MSA)]
    if (length(match) == 1){
      msa$name_2017match[i] <- gdp$MSA[grepl(a, gdp$MSA) & gdp$state == msa$state[i]]
    } else {msa$name_2017match[i] <- length(match)}
  }
}
gdp <- select(gdp, -state)
msa <- merge(msa, gdp, by.x = "name_2017match", by.y = "MSA") #346


#add ethnicity
#2019 data from American Communicty Survey-Demographic and Housing Estimates
#https://data.census.gov/cedsci/table?q=&t=Race%20and%20Ethnicity&g=0100000US.310000&tid=ACSDP5Y2019.DP05
ethn <- read.csv('us/ACSDP5Y2019.DP05_data_with_overlays_2021-07-23T120045.csv', stringsAsFactors = F)
colnames(ethn) <- ethn[1, ]
ethn <- ethn[-1, ]
colnames(ethn)[c(149, 153, 157, 177)] <- c("white", 'black', 'native', 'asian')
ethn <- ethn %>% 
  select(id, 'Geographic Area Name', white, black, native, asian) %>%
  mutate_at(vars(white, black, native, asian), as.numeric) %>%
  mutate(id = str_sub(id, -5, -1) %>% as.numeric,
         'Geographic Area Name' = str_replace(!!sym('Geographic Area Name'), " Metro Area", ""))
msa1 <- merge(msa, ethn, by.x = 'CBSA_code', by.y = 'id') %>%
  select(-'Geographic Area Name')
msa2 <- filter(msa, !CBSA_code %in% ethn$id) %>%
  merge(ethn, by.x = 'MSA_STATE', by.y = 'Geographic Area Name') %>%
  select(-id)
msa <- rbind(msa1, msa2)
#write.csv(msa, "us/info_346msa.csv")
msa <- mutate(msa, y65 = y65to74 + y75)
t <- select(msa, urb_pop, urb_den, pincom2019, white, y65)
cor(t)


#check
m <- lm(urb_den ~ y75 + y65to74 + y18to24 + Under18 + y55to64 + y45to54 + y25to34 + 
          pincom2019, msa)
cor.test(m$residuals, msa$urb_den) #corr pop 0.88, den 0.79

#Rt
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
msa_name <- select(msa, CBSA_code, MSA_STATE, MSA, state, name_2010match, name_2017match)
msa_county <- merge(msa_name, link ,by = "MSA_STATE")


#match case number
case <- read.csv('~/Documents/!Work/2020.7 Covid19/wyzz_美国/daily_newly_cases_US.csv', stringsAsFactors = F)
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
  select(-X, -UID, -iso2, -iso3, -code3, -FIPS, -Country_Region, -Lat, -Long_, -Combined_Key)
msa_county_case_long <- gather(msa_county_case, date, new_cases, X2020.01.23:X2020.11.18)
msa_county_case_long$date <- substr(msa_county_case_long$date, 2, nchar(msa_county_case_long$date)) %>% as.Date("%Y.%m.%d")
msa_case <- aggregate(new_cases ~ MSA_STATE + CBSA_code + MSA + state + date, msa_county_case_long, sum) #346 right
write.csv(msa_case, "us/us_case_346msa.csv")

# add land area from county
ctyla <- read.csv('us/county_landarea.csv')
msa_county1 <- filter(msa_county, county_code %in% ctyla$STCOU)
msa_county2 <- filter(msa_county, !county_code %in% ctyla$STCOU)
ctyla_msa1 <- merge(msa_county1, ctyla[ , c('Areaname', 'STCOU', 'LND010190D')], 
                   by.x = 'county_code', by.y = 'STCOU')
ctyla_msa1 <- select(ctyla_msa1, -Areaname)
ctyla_msa2 <- merge(msa_county2, ctyla[ , c('Areaname', 'STCOU', 'LND010190D')], 
                    by.x = 'county_name', by.y = 'Areaname') 
ctyla_msa2 <- select(ctyla_msa2, -STCOU)
#Roanoke, VA & Franklin, VA have double matches in ctyla, but doesn't matter
ctyla_msa <- rbind(ctyla_msa1, ctyla_msa2)
msa_area <- aggregate(LND010190D ~ MSA_code + MSA_STATE, ctyla_msa, sum)
msa <- merge(msa, msa_area[ , c('MSA_STATE', 'LND010190D')], by = 'MSA_STATE')
msa <- msa %>% mutate(land = LND010190D * 2.6,
                      total_den = total_pop / land)
write.csv(msa, "us/info_346msa.csv")

