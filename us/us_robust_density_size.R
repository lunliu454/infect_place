# heterogeneity sensitivity to threshold ####
library(dplyr)
library(lfe)
setwd("~/Documents/!Work/2020.7 Covid19/July")

input <- read.csv("us/us_input_msa2.csv", stringsAsFactors = F)
msa <- read.csv('us/info_346msa.csv', stringsAsFactors = F)
msa <- msa %>% mutate(den = urb_den / 1000, pop = urb_pop / 1000000, old = y65to74 + y75, 
                      income = pincom2019 / 1000, gdppc = gdp2017 * 1000 / total_pop) %>%
  select(MSA_STATE, pop, den, old, income, gdppc, black, asian)
input <- merge(input, msa, by = 'MSA_STATE')

#t <- select(input, city_eng_name, urb_pop, urb_den, total_pop, total_den) %>% distinct()
#summary(t$total_den)
get_max <- function(m){
  combined_result <- data.frame(combine = rep(0, 2 ^ n), 
                                coefficient = rep(0, 2 ^ n), 
                                se = rep(0, 2 ^ n),
                                t = rep(0, 2 ^ n))
  for (i in 2 : 2 ^ n){
    index_on <- l[i,] %>% unlist()
    int_on <- var[index_on == 1]
    combined_result$combine[i] <- paste(int_on, collapse = "+")
    combined_result$coefficient[i] <- sum(m$beta[dimnames(m$beta)[[1]] %in% int_on])
    se <- m$clustervcv[row.names(m$clustervcv) %in% int_on, colnames(m$clustervcv) %in% int_on] %>% sum
    combined_result$se[i] <- sqrt(se)
    combined_result$t[i] <- combined_result$coefficient[i] / combined_result$se[i]
  }
  combined_result %>% 
    #filter(abs(t) > 1.96) %>% 
    top_n(-1, coefficient)
}
var <- c("win7_school_close", "win7_childcare_close",
         "win7_shop_close","win7_restaurant_close",
         "win7_bar_close","win7_entertainment_close", 
         "win7_cultural_close",
         #"win7_worship_close",
         "win7_sports_indoor_close"#,"win7_sports_outdoor_close"
         )
n <- length(var)
l <- rep(list(0:1), n)
l <- expand.grid(l)

# total_pop ----
hetero_sensi <- data.frame(threshold = seq(160000, 670000, 20000), 
                               beta_low = 0, se_low = 0, 
                               beta_high = 0, se_high = 0)
for (k in 1 : nrow(hetero_sensi)){
  print(k)
  threshold <- seq(160000, 670000, 20000)[k]
  m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close +
              win7_entertainment_close + win7_cultural_close +
              win7_worship_close + win7_sports_indoor_close + 
              #win7_sports_outdoor_close + 
              win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over |
              city_eng_name + as.factor(date) | 0 | city_eng_name, 
            filter(input, total_pop > threshold))
  t <- get_max(m)
  hetero_sensi$beta_high[k] <- t$coefficient[1]
  hetero_sensi$se_high[k] <- t$se[1]

  m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close +
              win7_entertainment_close + win7_cultural_close +
              win7_worship_close + win7_sports_indoor_close + 
              #win7_sports_outdoor_close + 
              win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over |
              city_eng_name + as.factor(date) | 0 | city_eng_name, 
            filter(input, total_pop < threshold))
  t <- get_max(m)
  hetero_sensi$beta_low[k] <- t$coefficient[1]
  hetero_sensi$se_low[k] <- t$se[1]
}
# plot
p1 <- 
  ggplot(hetero_sensi, aes(x = threshold)) + 
  geom_line(aes(y = beta_low), col = 'blue', size = 0.5) + 
  geom_ribbon(aes(ymin = beta_low - 1.96 * se_low, 
                  ymax = beta_low + 1.96 * se_low), 
              alpha = 0.2, fill = 'blue') +
  geom_line(aes(y = beta_high), col='red') + 
  geom_ribbon(aes(ymin = beta_high - 1.96 * se_high, 
                  ymax = beta_high + 1.96 * se_high), 
              alpha = 0.2, fill = 'red') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 22,face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 16),#hjust = 0.5, 
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) + 
  xlab('Threshold\n(perons)') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_continuous(limit = c(160000, 670000),
                     breaks= seq(200000, 600000, 100000)) +
  scale_y_continuous(limit = c(-2, 0.1), 
                     breaks = seq(-2, 0, 1)) +
  ggtitle("\nTotal population in MSA") +
  coord_fixed(176767) #10*5*490000/(66*2.1)

# total_den ----
hetero_sensi <- data.frame(threshold = seq(40, 120, 10), 
                           beta_low = 0, se_low = 0, 
                           beta_high = 0, se_high = 0)
for (k in 1 : nrow(hetero_sensi)){
  print(k)
  threshold <- seq(40, 120, 10)[k] #total_den
  m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close +
              win7_entertainment_close + win7_cultural_close +
              win7_worship_close + win7_sports_indoor_close + 
              #win7_sports_outdoor_close + 
              win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over |
              city_eng_name + as.factor(date) | 0 | city_eng_name, 
            filter(input, total_den > threshold))
  t <- get_max(m)
  hetero_sensi$beta_high[k] <- t$coefficient[1]
  hetero_sensi$se_high[k] <- t$se[1]
  
  m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close +
              win7_entertainment_close + win7_cultural_close +
              win7_worship_close + win7_sports_indoor_close + 
              #win7_sports_outdoor_close + 
              win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over |
              city_eng_name + as.factor(date) | 0 | city_eng_name, 
            filter(input, total_den < threshold))
  t <- get_max(m)
  hetero_sensi$beta_low[k] <- t$coefficient[1]
  hetero_sensi$se_low[k] <- t$se[1]
}
p2 <- 
  ggplot(hetero_sensi, aes(x = threshold)) + 
  geom_line(aes(y = beta_low), col = 'blue', size = 0.5) + 
  geom_ribbon(aes(ymin = beta_low - 1.96 * se_low, 
                  ymax = beta_low + 1.96 * se_low), 
              alpha = 0.2, fill = 'blue') +
  geom_line(aes(y = beta_high), col='red') + 
  geom_ribbon(aes(ymin = beta_high - 1.96 * se_high, 
                  ymax = beta_high + 1.96 * se_high), 
              alpha = 0.2, fill = 'red') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 22,face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 16),#hjust = 0.5, 
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) + 
  xlab('Threshold\n(perons per squared kilometer)') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_continuous(limit = c(40, 120),
                     breaks= seq(40, 120, 10)) +
  scale_y_continuous(limit = c(-2, 0), 
                     breaks = seq(-2, 0, 1)) +
  ggtitle("\nAverage density in MSA") +
  coord_fixed(30.3) #10*5*80/(66*2)

library(egg)
p <- ggarrange(p1, p2, draw = F, ncol = 2)
ggsave("plot/hetero_sensi_us.png", plot = p, height = 12, width = 40,
       units = "cm", dpi = 300, limitsize = F)


# old ----
summary(msa$old)
hetero_sensi <- data.frame(threshold = seq(0.112, 0.148, 0.003), 
                           beta_low = 0, se_low = 0, 
                           beta_high = 0, se_high = 0)
for (k in 1 : nrow(hetero_sensi)){
  print(k)
  threshold <- seq(0.112, 0.148, 0.003)[k]
  m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close +
              win7_entertainment_close + win7_cultural_close +
              win7_worship_close + win7_sports_indoor_close + 
              #win7_sports_outdoor_close + 
              win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over |
              city_eng_name + as.factor(date) | 0 | city_eng_name, 
            filter(input, old > threshold))
  t <- get_max(m)
  hetero_sensi$beta_high[k] <- t$coefficient[1]
  hetero_sensi$se_high[k] <- t$se[1]
  
  m <- felm(log_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close +
              win7_entertainment_close + win7_cultural_close +
              win7_worship_close + win7_sports_indoor_close + 
              #win7_sports_outdoor_close + 
              win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over |
              city_eng_name + as.factor(date) | 0 | city_eng_name, 
            filter(input, old  < threshold))
  t <- get_max(m)
  hetero_sensi$beta_low[k] <- t$coefficient[1]
  hetero_sensi$se_low[k] <- t$se[1]
}
#p2 <- 
  ggplot(hetero_sensi, aes(x = threshold)) + 
  geom_line(aes(y = beta_low), col = 'blue', size = 0.5) + 
  geom_ribbon(aes(ymin = beta_low - 1.96 * se_low, 
                  ymax = beta_low + 1.96 * se_low), 
              alpha = 0.2, fill = 'blue') +
  geom_line(aes(y = beta_high), col='red') + 
  geom_ribbon(aes(ymin = beta_high - 1.96 * se_high, 
                  ymax = beta_high + 1.96 * se_high), 
              alpha = 0.2, fill = 'red') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 22,face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 16),#hjust = 0.5, 
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) + 
  xlab('Threshold\n(perons per squared kilometer)') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_continuous(limit = c(0.112, 0.148),#seq(0.112, 0.148, 0.003)
                     breaks= seq(0.112, 0.148, 0.01)) +
  scale_y_continuous(limit = c(-2, 0), 
                     breaks = seq(-2, 0, 1)) +
  ggtitle("\nAverage density in MSA") +
  coord_fixed(0.0136) #10*5*0.036/(66*2)

library(egg)
p <- ggarrange(p1, p2, draw = F, ncol = 2)
ggsave("plot/hetero_sensi_us.png", plot = p, height = 12, width = 40,
       units = "cm", dpi = 300, limitsize = F)

