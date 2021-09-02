setwd("~/Documents/!Work/2020.7 Covid19/July")

library(dplyr)
library(ggplot2)
library(scales)


# jp den ----
jp_den <- read.csv('plot/diff_den_jp.csv')
p_jp_den <- ggplot(jp_den, aes(x = threshold)) + 
  geom_line(aes(y = beta_denlow), col = 'blue', size = 0.5) + 
  geom_ribbon(aes(ymin = beta_denlow - 1.96 * se_denlow, 
                  ymax = beta_denlow + 1.96 * se_denlow), 
              alpha = 0.2, fill = 'blue') +
  geom_line(aes(y = beta_denhigh), col='red') + 
  geom_ribbon(aes(ymin = beta_denhigh - 1.96 * se_denhigh, 
                  ymax = beta_denhigh + 1.96 * se_denhigh), 
              alpha = 0.2, fill = 'red') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        #plot.title = element_text(size = 24,face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 16),#hjust = 0.5, 
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) + 
  xlab('Threshold\n(perons per squared kilometer)') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_continuous(limit = c(5000, 7300),
                     breaks= seq(5000, 7300, 500)) +
  scale_y_continuous(limit = c(-5, 0), 
                     breaks = seq(-5, 0, 1)) +
  #ggtitle("Japan") +
  coord_fixed(350) #10*5*2300/(66*5)

# jp pop ----
jp_pop <- read.csv('plot/diff_pop_jp.csv')
p_jp_pop <- ggplot(jp_pop, aes(x = threshold)) + 
  geom_line(aes(y = beta_poplow), col = 'blue', size = 0.5) + 
  geom_ribbon(aes(ymin = beta_poplow - 1.96 * se_poplow, 
                  ymax = beta_poplow + 1.96 * se_poplow), 
              alpha = 0.2, fill = 'blue') +
  geom_line(aes(y = beta_pophigh), col='red') + 
  geom_ribbon(aes(ymin = beta_pophigh - 1.96 * se_pophigh, 
                  ymax = beta_pophigh + 1.96 * se_pophigh), 
              alpha = 0.2, fill = 'red') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 24,face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 16),#hjust = 0.5, 
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) + 
  xlab('Threshold\n(persons)') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_continuous(limit = c(220000, 880000),
                     breaks= c(250000, 500000, 750000),
                     #breaks= seq(300000, 800000, 100000),
                     labels = scientific) +
  scale_y_continuous(limit = c(-5, 0), 
                     breaks = seq(-5, 0, 1)) +
  ggtitle("Japan") +
  coord_fixed(100000)

# uk den ----
uk_den <- read.csv('plot/diff_den_uk.csv')
uk_den <- uk_den[-1, ]
p_uk_den <- ggplot(uk_den, aes(x = threshold)) + 
  geom_line(aes(y = beta_denlow), col = 'blue', size = 0.5) + 
  geom_ribbon(aes(ymin = beta_denlow - 1.96 * se_denlow, 
                  ymax = beta_denlow + 1.96 * se_denlow), 
              alpha = 0.2, fill = 'blue') +
  geom_line(aes(y = beta_denhigh), col='red') + 
  geom_ribbon(aes(ymin = beta_denhigh - 1.96 * se_denhigh, 
                  ymax = beta_denhigh + 1.96 * se_denhigh), 
              alpha = 0.2, fill = 'red') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 16),#hjust = 0.5, 
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) + 
  xlab('Threshold\n(perons per squared kilometer)') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_continuous(limit = c(300, 1700),
                     breaks=c(500, 1000, 1500)) +
  scale_y_continuous(limit = c(-3.6, 0), 
                     breaks = seq(-3, 0, 1)) +
  #ggtitle("Japan") +
  coord_fixed(295)#10*5*1400/(66*3.6)

# uk pop ----
uk_pop <- read.csv('plot/diff_pop_uk.csv')
uk_pop <- uk_pop[3 : 11, ]
p_uk_pop <- ggplot(uk_pop, aes(x = threshold)) + 
  geom_line(aes(y = beta_poplow), col = 'blue', size = 0.5) + 
  geom_ribbon(aes(ymin = beta_poplow - 1.96 * se_poplow, 
                  ymax = beta_poplow + 1.96 * se_poplow), 
              alpha = 0.2, fill = 'blue') +
  geom_line(aes(y = beta_pophigh), col='red') + 
  geom_ribbon(aes(ymin = beta_pophigh - 1.96 * se_pophigh, 
                  ymax = beta_pophigh + 1.96 * se_pophigh), 
              alpha = 0.2, fill = 'red') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 24,face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 16),#hjust = 0.5, 
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) + 
  xlab('Threshold\n(persons)') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_continuous(limit = c(140000, 220000),
                     #breaks = c(150000, 200000),
                     breaks = c(150000, 200000),
                     labels = scientific) +
  scale_y_continuous(limit = c(-3.6, 0), 
                     breaks = seq(-4, 0, 1)) +
  ggtitle("United Kingdom") +
  coord_fixed(16835) #10*5*80000/(66*3.6)

# us den ----
us_den <- read.csv('plot/diff_den_us.csv')
p_us_den <- ggplot(us_den, aes(x = threshold)) + 
  geom_line(aes(y = beta_denlow), col = 'blue', size = 0.5) + 
  geom_ribbon(aes(ymin = beta_denlow - 1.96 * se_denlow, 
                  ymax = beta_denlow + 1.96 * se_denlow), 
              alpha = 0.2, fill = 'blue') +
  geom_line(aes(y = beta_denhigh), col='red') + 
  geom_ribbon(aes(ymin = beta_denhigh - 1.96 * se_denhigh, 
                  ymax = beta_denhigh + 1.96 * se_denhigh), 
              alpha = 0.2, fill = 'red') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 16),#hjust = 0.5, 
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) + 
  xlab('Threshold\n(perons per squared kilometer)') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_continuous(limit = c(600, 1000),
                     breaks = seq(600, 1000, 100)) +
  scale_y_continuous(limit = c(-2, 0.3), 
                     breaks = seq(-2, 0, 1)) +
  coord_fixed(132) #10*5*400/(66*2.3)

# us pop ----
us_pop <- read.csv('plot/diff_pop_us.csv')
p_us_pop <- ggplot(us_pop, aes(x = threshold)) + 
  geom_line(aes(y = beta_poplow), col = 'blue', size = 0.5) + 
  geom_ribbon(aes(ymin = beta_poplow - 1.96 * se_poplow, 
                  ymax = beta_poplow + 1.96 * se_poplow), 
              alpha = 0.2, fill = 'blue') +
  geom_line(aes(y = beta_pophigh), col='red') + 
  geom_ribbon(aes(ymin = beta_pophigh - 1.96 * se_pophigh, 
                  ymax = beta_pophigh + 1.96 * se_pophigh), 
              alpha = 0.2, fill = 'red') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 24,face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 16),#hjust = 0.5, 
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) + 
  xlab('Threshold\n(persons)') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_continuous(limit = c(90000, 470000),
    #limit = c(160000, 600000),
                     breaks = c(100000, 200000, 300000, 400000),
                     labels = scientific) +
  scale_y_continuous(limit = c(-2, 0.3), 
                     breaks = seq(-2, 0, 1)) +
  ggtitle("United States") +
  coord_fixed(125165) #10*5*380000/(66*2.3)

# br den ----
br_den <- read.csv('plot/diff_den_br.csv')
p_br_den <- ggplot(br_den, aes(x = threshold)) + 
  geom_line(aes(y = beta_denlow), col = 'blue', size = 0.5) + 
  geom_ribbon(aes(ymin = beta_denlow - 1.96 * se_denlow, 
                  ymax = beta_denlow + 1.96 * se_denlow), 
              alpha = 0.2, fill = 'blue') +
  geom_line(aes(y = beta_denhigh), col='red') + 
  geom_ribbon(aes(ymin = beta_denhigh - 1.96 * se_denhigh, 
                  ymax = beta_denhigh + 1.96 * se_denhigh), 
              alpha = 0.2, fill = 'red') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 16),#hjust = 0.5, 
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) + 
  xlab('Threshold\n(perons per squared kilometer)') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_continuous(limit = c(100, 1300),
                     breaks = seq(250, 1250, 250)) +
  scale_y_continuous(limit = c(-2, 0.2), 
                     breaks = seq(-2, 0, 1)) +
  #ggtitle("Japan") +
  coord_fixed(413) #10*5*1200/(66*2.2)

# bra pop ----
br_pop <- read.csv('plot/diff_pop_br.csv')
p_br_pop <- ggplot(br_pop, aes(x = threshold)) + 
  geom_line(aes(y = beta_poplow), col = 'blue', size = 0.5) + 
  geom_ribbon(aes(ymin = beta_poplow - 1.96 * se_poplow, 
                  ymax = beta_poplow + 1.96 * se_poplow), 
              alpha = 0.2, fill = 'blue') +
  geom_line(aes(y = beta_pophigh), col='red') + 
  geom_ribbon(aes(ymin = beta_pophigh - 1.96 * se_pophigh, 
                  ymax = beta_pophigh + 1.96 * se_pophigh), 
              alpha = 0.2, fill = 'red') +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 24,face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 16),#hjust = 0.5, 
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) + 
  xlab('Threshold\n(persons)') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_continuous(limit = c(130000, 330000),
                     breaks=c(150000, 200000, 250000, 300000),
                     labels = scientific) +
  scale_y_continuous(limit = c(-2, 0.1), 
                     breaks = seq(-2, 0, 1)) +
  ggtitle("Brazil") +
  coord_fixed(72150) #10*5*200000/(66*2.1)


# plot all ----
library(egg)
p <- ggarrange(p_jp_pop, p_uk_pop, p_us_pop, p_br_pop, p_jp_den, p_uk_den, p_us_den, p_br_den, draw = F, nrow = 2)
ggsave("plot/hetero.png", plot = p, height = 20, width = 70,
       units = "cm", dpi = 300, limitsize = F)
