input <- read.csv("uk/uk_input_234lda_final.csv", stringsAsFactors = F)
lda <- select(input, CTRY11NM, LAD17CD, areaName, RGN11NM, Geography, area, pop, density) %>% distinct
lda$LAD17CD[lda$areaName == "London"] <- "E12000007"
lda$Geography[lda$areaName == "London"] <- "Region"

#add age
#from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
#latest dataset, all include 
age <- read.csv("uk/lda_age_2020.csv", stringsAsFactors = F)
age <- age[, 1:95]
agecol <- function(x){paste0("X",x)}
age <- age %>% 
  mutate_at(vars(colnames(age)[4:95]), function(x){str_replace_all(x, ",", "") %>% as.numeric}) %>%
  mutate(Under18 = rowSums(.[sapply(0:17, agecol)]) / All.ages,
         y18to24 = rowSums(.[sapply(18:24, agecol)]) / All.ages,
         y25to34 = rowSums(.[sapply(25:34, agecol)]) / All.ages,
         y35to44 = rowSums(.[sapply(35:44, agecol)]) / All.ages,
         y45to54 = rowSums(.[sapply(45:54, agecol)]) / All.ages,
         y55to64 = rowSums(.[sapply(55:64, agecol)]) / All.ages,
         y65to74 = rowSums(.[sapply(65:74, agecol)]) / All.ages,
         y75 = rowSums(.[sapply(c(75:89, "90."), agecol)]) / All.ages) %>%
  select(Code, Name, All.ages, Under18, y18to24, y25to34, y35to44, y45to54,
         y55to64, y65to74, y75) %>%
  filter(Code != "")
lda <- merge(lda, age, by.x = "LAD17CD", by.y = "Code", all.x = T)

#add gdp
#from https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductlocalauthorities
gdp <- read.csv("uk/lda_gdpperhead_2019.csv", stringsAsFactors = F)
gdp <- gdp %>% 
  mutate(gdphead2019 = str_replace_all(X2019, ",", "") %>% as.numeric) %>%
  filter(ITL1.Region != '')
#mean(gdp$gdphead2019[grepl("London", gdp$ITL1.Region)]) #268853 #strange!!
#use plain mean instead of population weighted, this won't make much difference
gdp[nrow(gdp) + 1, ] <- list("London", "E12000007", "London", 56199, 56199)
#look up from https://www.statista.com/statistics/1168072/uk-gdp-per-head-by-region/
gdp <- select(gdp, LA.code, gdphead2019)
lda <- merge(lda, gdp, by.x = "LAD17CD", by.y = "LA.code", all.x = T)
#correlation gdp per head and population is 0.8, but all for london

#add ethinicity
#from https://www.nomisweb.co.uk/census/2011/ks201uk, 2011census
#download .csv using two local authority choices
e1 <- read.csv("uk/ethnicity_1.csv", stringsAsFactors = F)
e2 <- read.csv("uk/ethnicity_2.csv", stringsAsFactors = F)
ethn <- rbind(e1, e2) %>% distinct
#make london
gdp <- read.csv("uk/lda_gdpperhead_2019.csv", stringsAsFactors = F) %>%
  filter(ITL1.Region == "London") %>% select(ITL1.Region, LA.code, LA.name)
ethn_london <- filter(ethn, geography.code %in% gdp$LA.code)
ethn_london[34, 4 : 14] <- sapply(4 : 14, function(x){sum(ethn_london[1 : 33, x])})
ethn_london[34, 1 : 3] <- list(2011, "London", "E12000007")
ethn <- rbind(ethn, ethn_london[34,])
ethn <- ethn %>% 
  mutate(white = Ethnic.Group..White..measures..Value / .[[4]],
         asian = rowSums(.[,grepl("Asian", colnames(.))]) / Ethnic.Group..All.categories..Ethnic.group..measures..Value,
         black = .[[13]] / .[[4]]) %>%
  select(geography, geography.code, white, asian, black)
lda <- merge(lda, ethn, by.x = "LAD17CD", by.y = "geography.code", all.x = T)
#other info
#disposable household income: https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/datasets/regionalgrossdisposablehouseholdincomebylocalauthoritiesbynuts1region
#lda urban rural pop (England): https://www.gov.uk/government/statistics/2011-rural-urban-classification


#check
m <- lm(pop~y75 + y65to74 + y18to24 + Under18 + y55to64 + y45to54 + y25to34 + white + black + gdphead2019, filter(lda, areaName != "London"))
cor.test(m$residuals, lda$pop[lda$areaName != "London"])

lda$y65 <- lda$y65to74+lda$y75
t <- select(lda, pop, density, y65, gdphead2019, white)
cor(t)

write.csv(lad, "uk/lda_full_info.csv")
