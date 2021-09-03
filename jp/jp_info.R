library(dplyr)

#prefectures' Japanese name and English name lookup from wiki
#https://en.wikipedia.org/wiki/Prefectures_of_Japan
#downloaded with the wikitocsv website
name <- read.csv('jp/pref_name_lookup.csv', stringsAsFactors = F)
name <- select(name, Prefecture, Prefecture.1, Capital, Capital.1, Area..15.)
name$Prefecture <- str_replace_all(name$Prefecture, c(ō = 'o', Ō = 'O'))

#age 2019
#from https://www.e-stat.go.jp/regional-statistics/ssdsview/prefectures
age <- read.csv('jp/pref_age_2019.csv', stringsAsFactors = F)
colnames(age) <- c('pref', colnames(age)[1 : 20])
age <- age[2 : 48, ]
age <- age %>% 
  mutate(pref = str_split(pref, "_", simplify = T)[, 2]) %>%
  mutate_at(vars(starts_with("A1")), function(x){str_replace_all(x, ',', '') %>% as.numeric}) %>%
  mutate(yunder19 = (A1201_0.4歳人口.人. + A1202_5.9歳人口.人. + A1203_10.14歳人口.人. + A1204_15.19歳人口.人.) / A1101_総人口.人.,
         y20to24 = A1205_20.24歳人口.人. / A1101_総人口.人.,
         y25to34 = (A1206_25.29歳人口.人. + A1207_30.34歳人口.人.) / A1101_総人口.人.,
         y35to44 = (A1208_35.39歳人口.人. + A1209_40.44歳人口.人.) / A1101_総人口.人.,
         y45to54 = (A1210_45.49歳人口.人. + A1211_50.54歳人口.人.) / A1101_総人口.人.,
         y55to64 = (A1212_55.59歳人口.人. + A1213_60.64歳人口.人.) / A1101_総人口.人.,
         y65to74 = (A1214_65.69歳人口.人. + A1215_70.74歳人口.人.) / A1101_総人口.人.,
         y75 = 1 - yunder19 - y20to24 - y25to34 - y35to44 - y45to54 - y55to64 - y65to74)
age <- age[, c(1 : 2, 22 : 29)]
pref <- merge(name, age, by.x = 'Prefecture.1', by.y = 'pref')

#gdp
#from https://stats.oecd.org/
#Regions and Cities/Regional Economy/Gross Domestic Product, Small regions TL3
gdp <- read.csv('jp/pref_gdp_2017.csv', stringsAsFactors = F)
gdp <- gdp %>% 
  mutate(gdp2017 = X2017,
         Year = str_split(Year, ": ", simplify = T)[, 2],
         pref = Year) %>%
  select(pref, gdp2017)
gdp$pref[gdp$pref == 'Gumma'] <- 'Gunma'
#divide pop to get gdp per capita
#pop2017 same source as age
pop2017 <- read.csv('jp/pref_pop2017.csv', stringsAsFactors = F)
colnames(pop2017) <- c('pref', "pop2017")
pop2017 <- pop2017 %>% 
  mutate(pref = str_split(pref, "_", simplify = T)[, 2],
         pop2017 = str_replace_all(pop2017, ',', '') %>% as.numeric) 
pref <- merge(pref, gdp, by.x = 'Prefecture', by.y = 'pref')
pref <- merge(pref, pop2017, by.x = 'Prefecture.1', by.y = 'pref')
pref$gdppc <- pref$gdp2017 / pref$pop2017

#income
#from https://www.e-stat.go.jp/en/stat-search/file-download?statInfId=000032079768&fileKind=0
#on https://www.e-stat.go.jp/en/stat-search/files?page=1&layout=datalist&toukei=00200564&tstat=000001139024&cycle=0&tclass1=000001150335&tclass2=000001150336&tclass3=000001153349&tclass4val=0
income <- read.csv('jp/pref_income.csv', stringsAsFactors = F)
income <- income[ , 1 : 68] %>% t %>% as.data.frame
income$place <- row.names(income)
row.names(income) <- NULL
income <- income [-1, ]
colnames(income)[1] <- 'income'
income <- income %>% 
  mutate(income = income %>% as.character %>% str_replace_all(',', '') %>% as.numeric,
         place = str_split(place, "_", simplify = T)[, 2],
         place = str_replace_all(place, '\\.ken|\\.fu|\\.to', '')) %>%
  distinct
pref <- merge(pref, income, by.x = 'Prefecture', by.y = 'place')

#pref municiaplities
#municipality population and urban-related info from
#https://www.e-stat.go.jp/en/regional-statistics/ssdsview/municipality
#2015data, select A1101 A1801 A1802
mu <- read.csv('jp/mu_info_2015.csv', stringsAsFactors = F)
mu <- mu %>% 
  mutate(mupop = A1101_Total.population..Both.sexes..person.,
         mupop_dense = A1801_Densely.inhabited.districts.population.person.,
         muarea_dense = A1802_Densely.inhabited.districts.area.km2.) %>%
  select(AREA.Code, AREA, mupop, mupop_dense, muarea_dense) %>%
  mutate_at(vars(mupop, mupop_dense, muarea_dense),
            function(x){
              if(x == '-'){x = NA}
              x = str_replace_all(x, ',', '') %>% as.numeric
            }) %>%
  filter(str_count(AREA,' ') <= 1) %>%
  mutate(pref = str_split(AREA, ' ', simplify = T)[, 1],
         mu = str_split(AREA, ' ', simplify = T)[, 2])
t <- mu %>% group_by(pref) %>% 
  summarise(mean_mupop = mean(mupop), 
            mean_mupop_dense = mean(mupop_dense, na.rm = TRUE), 
            sum_mupop = sum(mupop),
            sum_mupop_dense = sum(mupop_dense, na.rm = TRUE),
            sum_muarea_dense = sum(muarea_dense, na.rm = TRUE))
s <- mu %>% group_by(pref) %>% slice(which.max(mupop))
pref_mu <- merge(t, s, by = 'pref')
pref_mu[pref_mu$pref == 'Tokyo-to', c('mupop', 'mupop_dense', 'muarea_dense', 'mean_mupop')] <- 
  pref_mu[pref_mu$pref == 'Tokyo-to', c('sum_mupop', 'sum_mupop_dense', 'sum_muarea_dense', 'sum_mupop')]
#pref_mu$ratio <- pref_mu$mupop_dense / pref_mu$sum_mupop_dense
pref_mu$mean_density <- pref_mu$sum_mupop_dense / pref_mu$sum_muarea_dense
pref_mu$largest_density <- pref_mu$mupop_dense / pref_mu$muarea_dense
pref_mu$pref <- str_replace(pref_mu$pref, "-ken|-fu|-to", "")
pref_mu$pref[pref_mu$pref == 'Gumma'] <- 'Gunma'
pref <- merge(pref, pref_mu, by.x = 'Prefecture', by.y = 'pref')
pref$division_eng_name <- pref$Prefecture
#mean_mupop correlate with city_pop exclude Tokyo 0.45, 
#because city_pop only take mean of selected large cities
write.csv(pref, 'jp/pref_moreinfo.csv')
