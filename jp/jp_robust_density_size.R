library(dplyr)
library(lfe)
library(ggplot2)
library(scales)

input <- read.csv("jp/jp_input.csv", stringsAsFactors = F)
pref <- read.csv('jp/pref_moreinfo.csv', stringsAsFactors = F)
pref <- pref %>% mutate(old = y65to74 + y75, den = largest_density / 1000, pop = mupop_dense / 1000000,
                        gdppc = gdppc * 1000, income = income / 1000)  #gdppc thousand usd, income thousand yen
input <- merge(input, pref, by = 'division_eng_name')
var <- c('win7_school_close',
         'win7_childcare_close',
         'win7_shop_close',
         'win7_restaurant_close',
         'win7_bar_close',
         'win7_entertainment_close',
         'win7_cultural_close',
         'win7_worship_close',
         'win7_sports_indoor_close',
         'win7_sports_outdoor_close')
n <- length(var)
l <- rep(list(0:1), n)
l <- expand.grid(l)
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
    top_n(-1, coefficient)
}

# mupop ----
summary(pref$mupop)
hetero_sensi <- data.frame(threshold = seq(320000, 970000, 20000), 
                               beta_low = 0, se_low = 0, 
                               beta_high = 0, se_high = 0)
for (k in 1 : nrow(hetero_sensi)){
  print(k)
  threshold <- seq(320000, 970000, 20000)[k]
  m <- felm(log_win7_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close + 
              win7_entertainment_close + win7_cultural_close + 
              win7_worship_close + win7_sports_indoor_close +
              win7_sports_outdoor_close + win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over
            | division_eng_name + as.factor(date) | 0 | division_eng_name, 
            filter(input, mupop > threshold))
  t <- get_max(m)
  hetero_sensi$beta_high[k] <- t$coefficient[1]
  hetero_sensi$se_high[k] <- t$se[1]
  
  m <- felm(log_win7_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close + 
              win7_entertainment_close + win7_cultural_close + 
              win7_worship_close + win7_sports_indoor_close +
              win7_sports_outdoor_close + win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over
            | division_eng_name + as.factor(date) | 0 | division_eng_name, 
            filter(input, mupop < threshold))
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
  scale_x_continuous(limit = c(320000, 1000000),
                     breaks= c(400000, 600000, 800000, 1000000),
                     labels = scientific) +
  scale_y_continuous(limit = c(-5.3, 0), 
                     breaks = seq(-5, 0, 1)) +
  ggtitle("Total population\nof the largest municipality") +
  coord_fixed(91480) #10*5*640000/(66*5.3)

# sum_mupop_dense ----
summary(pref$sum_mupop_dense)
hetero_sensi <- data.frame(threshold = seq(510000, 1830000, 50000), 
                           beta_low = 0, se_low = 0, 
                           beta_high = 0, se_high = 0)
for (k in 1 : nrow(hetero_sensi)){
  print(k)
  threshold <- seq(510000, 1830000, 50000)[k]
  m <- felm(log_win7_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close + 
              win7_entertainment_close + win7_cultural_close + 
              win7_worship_close + win7_sports_indoor_close +
              win7_sports_outdoor_close + win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over
            | division_eng_name + as.factor(date) | 0 | division_eng_name, 
            filter(input, sum_mupop_dense > threshold))
  t <- get_max(m)
  hetero_sensi$beta_high[k] <- t$coefficient[1]
  hetero_sensi$se_high[k] <- t$se[1]
  
  m <- felm(log_win7_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close + 
              win7_entertainment_close + win7_cultural_close + 
              win7_worship_close + win7_sports_indoor_close +
              win7_sports_outdoor_close + win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over
            | division_eng_name + as.factor(date) | 0 | division_eng_name, 
            filter(input, sum_mupop_dense < threshold))
  t <- get_max(m)
  hetero_sensi$beta_low[k] <- t$coefficient[1]
  hetero_sensi$se_low[k] <- t$se[1]
}
# plot
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
  xlab('Threshold\n(perons)') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_continuous(limit = c(510000, 2000000),
                     breaks= c(500000, 1000000, 1500000, 2000000),
                     labels = scientific) +
  scale_y_continuous(limit = c(-5.7, 0.3), 
                     breaks = seq(-5, 0, 1)) +
  ggtitle("Total population\nof densely populated areas") +
  coord_fixed(188131) #10*5*1490000/(66*6)

# mean_density ----
summary(pref$mean_density)
hetero_sensi <- data.frame(threshold = seq(4400, 6200, 100), 
                           beta_low = 0, se_low = 0, 
                           beta_high = 0, se_high = 0)
for (k in 1 : nrow(hetero_sensi)){
  print(k)
  threshold <- seq(4400, 6200, 100)[k]
  m <- felm(log_win7_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close + 
              win7_entertainment_close + win7_cultural_close + 
              win7_worship_close + win7_sports_indoor_close +
              win7_sports_outdoor_close + win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over
            | division_eng_name + as.factor(date) | 0 | division_eng_name, 
            filter(input, mean_density > threshold))
  t <- get_max(m)
  hetero_sensi$beta_high[k] <- t$coefficient[1]
  hetero_sensi$se_high[k] <- t$se[1]
  
  m <- felm(log_win7_Rt_estimate ~ win7_school_close + win7_childcare_close +
              win7_shop_close + win7_restaurant_close + win7_bar_close + 
              win7_entertainment_close + win7_cultural_close + 
              win7_worship_close + win7_sports_indoor_close +
              win7_sports_outdoor_close + win7_stay_at_home +
              win7_gathering_outside_10lower + win7_gathering_outside_10over
            | division_eng_name + as.factor(date) | 0 | division_eng_name, 
            filter(input, mean_density < threshold))
  t <- get_max(m)
  hetero_sensi$beta_low[k] <- t$coefficient[1]
  hetero_sensi$se_low[k] <- t$se[1]
}

# plot
p3 <- ggplot(hetero_sensi, aes(x = threshold)) + 
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
  scale_x_continuous(limit = c(4400, 6200),
                     breaks= seq(4500, 6000, 500)) +
  scale_y_continuous(limit = c(-5, 0), 
                     breaks = seq(-5, 0, 1)) +
  ggtitle("Average density\nof densely populated areas") +
  coord_fixed(273) #10*5*1800/(66*5)


library(egg)
p <- ggarrange(p1, p2, p3, draw = F, ncol = 3)
ggsave("plot/hetero_sensi_jp.png", plot = p, height = 12, width = 40,
       units = "cm", dpi = 300, limitsize = F)

