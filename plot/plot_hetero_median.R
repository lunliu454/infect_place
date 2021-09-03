library(ggplot2)

jp <- read.csv('plot/median_jp.csv')
jp$id <- 'jp'
uk <- read.csv('plot/median_uk.csv')
uk$id <- 'uk'
us <- read.csv('plot/median_us.csv')
us$id <- 'us'
br <- read.csv('plot/median_bra.csv')
br$id <- 'br'
r <- rbind(jp, uk, us, br)
r$id <- factor(r$id, levels = c('jp', 'uk', 'us', 'br'))
r <- r %>% 
  mutate(lci = coefficient - 1.96 * se, uci = coefficient + 1.96 * se) %>%
  mutate(uci = ifelse(uci > 0, 0, uci), lci = ifelse(lci < -4, -4, lci)) %>%
  mutate(larrow = ifelse(lci == -4, 2, NA), uarrow = ifelse(uci == 0, 2, NA))

# by pop ----
p1 <- ggplot(filter(r, var == 'pop'), aes(fill=group, y=coefficient, x = id)) + 
  geom_bar(position="dodge", stat="identity", alpha = 0.5) +
  scale_fill_manual(labels = c("High", "Low"), values = c(high = 'red', low = 'blue')) +
  labs(title = "Impact of activity space closure\nby population size", x = "", y = expression(Delta*~"log R"[t]), 
       fill = "Size") +
  geom_errorbar(aes(ymin = coefficient-se*1.96, ymax = coefficient+se*1.96), 
                color = 'black', size = 0.5, width=0,
                position=position_dodge(.9)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 18)) + 
  #xlab('') + 
  #ylab(expression(Delta*~"log R"[t])) +
  scale_x_discrete(breaks=c("jp", "uk", "us", "br"),
                   labels=c("Japan", "United\nKingdom", "United\nStates", "Brazil")) +
  scale_y_continuous(limits = c(-5.2, 0.2), breaks = seq(-5, 0, 1)) +
  #ggtitle("Impact of activity space closure by density") +
  coord_fixed(0.7)

# by density ----
p2 <- ggplot(filter(r, var == 'den'), aes(fill=group, y=coefficient, x = id)) + 
  geom_bar(position="dodge", stat="identity", alpha = 0.5) +
  scale_fill_manual(labels = c("High", "Low"), values = c(high = 'red', low = 'blue')) +
  labs(title = "Impact of activity space closure\nby density", x = "", y = expression(Delta*~"log R"[t]), 
       fill = "Density") +
  geom_errorbar(aes(ymin=coefficient-se*1.96, ymax=coefficient+se*1.96), 
                color = 'black', size = 0.5, width=0,
                position=position_dodge(.9)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 24, hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 18)) + 
  #xlab('') + 
  #ylab(expression(Delta*~"log R"[t])) +
  scale_x_discrete(breaks=c("jp", "uk", "us", "br"),
                   labels=c("Japan", "United\nKingdom", "United\nStates", "Brazil")) +
  scale_y_continuous(limits = c(-5.2, 0.2), breaks = seq(-5, 0, 1)) +
  #ggtitle("Impact of activity space closure\n by density") +
  coord_fixed(0.7)

library(egg)
p <- ggarrange(p1, p2, draw = F, ncol = 2)
ggsave("plot/hetero_median.png", plot = p, height = 20, width =45,
       units = "cm", dpi = 300, limitsize = F)
