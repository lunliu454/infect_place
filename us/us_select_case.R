library(tidyr)
library(dplyr)

case <- read.csv('~/Documents/!Work/2020.7 Covid19/wyzz_美国/daily_newly_cases_US.csv')
case$Admin2 <- case$Admin2 %>% dplyr::recode("DeKalb" = "De Kalb", "District of Columbia" = "Washington",
                                      "DuPage" = "Du Page", "Dade" = "Miami-Dade",
                                      "McClain" = "Mc Clain", "McLennan" = "Mc Lennan")
case$county_state <- paste(case$Admin2, case$Province_State, sep=',')

county_density <- read.csv("~/Documents/!Work/2020.7 Covid19/July/us_county_density.csv")
case <- case %>% filter(county_state %in% county_density$county_state) %>%
  select(-X, -UID, -iso2, -iso3, -code3, -FIPS, -Country_Region, -Lat, -Long_, -Combined_Key)
case_long <- gather(case, date, new_cases, X2020.01.23:X2020.11.18)
case_long$date <- substr(case_long$date, 2, nchar(case_long$date)) %>% as.Date("%Y.%m.%d")
write.csv(case_long, "~/Documents/!Work/2020.7 Covid19/July/us_case_239counties.csv")
