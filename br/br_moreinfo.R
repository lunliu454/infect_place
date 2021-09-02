library(dplyr)

input <- read.csv("bra/brazil_input_319cities_final.csv", stringsAsFactors = F)
mu <- input %>% 
  select(division_code, name, state, pop, area, density, division_local_name, 
         code, division_eng_name) %>%
  distinct
mu <- mu[-320, ] #strange replication of mu == "Altamira"

#gdp
#from https://ftp.ibge.gov.br/Pib_Municipios/2018/base/base_de_dados_2010_2018_xls.zip
#on https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html?=&t=downloads
gdp <- read.csv('bra/muni_gdp_2018.csv', stringsAsFactors = F)
colnames(gdp) <- c("year", "region_id", "region", "state_id", "state_abbr", "state", 
                   "mu_code", "mu", "gdp", "gdppc")
gdp <- gdp %>% 
  filter(year == 2018) %>%
  mutate_at(vars("gdp", "gdppc"), function(x){str_replace_all(x, ",", "") %>% as.numeric}) %>%
  mutate(mu_code = str_sub(mu_code, 1, 6) %>% as.numeric)
mu <- merge(mu, gdp[, c("mu_code", "gdp", "gdppc")], by.x = 'code', by.y = 'mu_code')

#age
#from 2010 census https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/indicadores_sociais_municipais/Brasil.zip
#on https://www.ibge.gov.br/en/statistics/social/population/18391-2010-population-census.html?=&t=downloads
#indicatores_sociais_municipais/Brasil.zip
age <- read.csv("bra/mu_age_2010.csv", stringsAsFactors = F)
colnames(age) <- c("mu_code", "staet_abbr", "mu", "y0to5", "y6to14", "y15to24",
                   "y25to39", "y40to59", "y60")
age <- age %>% mutate(mu_code = str_sub(mu_code, 1, 6) %>% as.numeric,
                      yunder15 = y0to5 + y6to14) %>%
  mutate_at(vars(starts_with("y")), function(x){x / 100})
mu <- merge(mu, age, by.x = "code", by.y = "mu_code")

#proportion of urban/rural population
#source same as age
prop <- read.csv("bra/mu_propurb_2010.csv", stringsAsFactors = F)
prop <- prop[, 1 : 5]
colnames(prop) <- c("mu_code", "state_abbr", 'mu', 'urban', 'rural')
prop <- prop %>%
  mutate(rural = ifelse(rural == '-', 0, rural) %>% as.numeric,
         urban = urban / 100,
         rural = rural / 100,
         mu_code = str_sub(mu_code, 1, 6) %>% as.numeric) %>%
  select(-state_abbr, -mu)
mu <- merge(mu, prop, by.x = "code", by.y = "mu_code") 
#summary(mu$urban) #high proportion of urban population, median = 0.96

#income
#only at state level, from https://ftp.ibge.gov.br/Indicadores_Sociais/Sintese_de_Indicadores_Sociais/Sintese_de_Indicadores_Sociais_2020/xls/2_Rendimento_xls.zip
#on https://www.ibge.gov.br/en/statistics/social/income-expenditure-and-consumption/18704-summary-of-social-indicators.html?=&t=downloads
#2020/Rendimento_xls.zip
income <- read.csv('bra/state_income_2019.csv', stringsAsFactors = F)
colnames(income) <- c("state", "income_mean", "income_median")
income <- mutate_at(income, vars('mean', 'median'), function(x){str_replace_all(x, " ", "") %>% as.numeric})
income <- income[1 : 59, ]
income <- income[c(-41, -43), ]
mu$division_local_name[mu$division_local_name == "MatoGrosso"] <- "Mato Grosso"
mu$division_local_name[mu$division_local_name == "MatoGrosso do Sul"] <- "Mato Grosso do Sul"
mu <- merge(mu, income, by.x = 'division_local_name', by.y = 'state')

write.csv(mu, 'bra/mu_info.csv')
t<-select(mu, pop, density, y60, income_median)
cor(t)

#population data in wiki from https://www.ibge.gov.br/en/statistics/social/population/18448-estimates-of-resident-population-for-municipalities-and-federation-units.html?=&t=downloads
