library(dplyr)
library(stringr)

#cities with more than 100,000 population from wikipedia
#https://www.ibge.gov.br/en/statistics/social/population/18448-estimates-of-resident-population-for-municipalities-and-federation-units.html?=&t=downloads
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
#please unzip brazil_covid19_cities.csv.zip
case <- read.csv("br/brazil_covid19_cities.csv")
mu <- merge(wiki, case[,c("state","name","code")] %>% distinct(), 
              by.x = c("city_eng_name", "division_acro"),
              by.y = c("name", "state"))
mu <- select(mu, -city_chn_name, -state)

#gdp
#from https://ftp.ibge.gov.br/Pib_Municipios/2018/base/base_de_dados_2010_2018_xls.zip
#on https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html?=&t=downloads
gdp <- read.csv('br/muni_gdp_2018.csv', stringsAsFactors = F)
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
age <- read.csv("br/mu_age_2010.csv", stringsAsFactors = F)
colnames(age) <- c("mu_code", "staet_abbr", "mu", "y0to5", "y6to14", "y15to24",
                   "y25to39", "y40to59", "y60")
age <- age %>% mutate(mu_code = str_sub(mu_code, 1, 6) %>% as.numeric,
                      yunder15 = y0to5 + y6to14) %>%
  mutate_at(vars(starts_with("y")), function(x){x / 100})
mu <- merge(mu, age, by.x = "code", by.y = "mu_code")

#proportion of urban/rural population
#source same as age
prop <- read.csv("br/mu_propurb_2010.csv", stringsAsFactors = F)
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
income <- read.csv('br/state_income_2019.csv', stringsAsFactors = F)
colnames(income) <- c("state", "income_mean", "income_median")
income <- mutate_at(income, vars('income_mean', 'income_median'), function(x){str_replace_all(x, " ", "") %>% as.numeric})
income <- income[1 : 59, ]
income <- income[c(-41, -43), ]
mu$division_local_name[mu$division_local_name == "MatoGrosso"] <- "Mato Grosso"
mu$division_local_name[mu$division_local_name == "MatoGrosso do Sul"] <- "Mato Grosso do Sul"
mu <- merge(mu, income, by.x = 'division_local_name', by.y = 'state')

write.csv(mu, 'br/br_info.csv')