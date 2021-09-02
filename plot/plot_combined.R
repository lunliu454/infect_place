
# jp -----
jp <- read.csv("plot/combined_num_jp.csv")
jp <- jp[1 : 6, ]
jp <- jp %>% 
  mutate(number = as.factor(number + 1)) %>%
  mutate(uci = coefficient + 1.96 * se, lci = coefficient - 1.96 * se) %>%
  mutate(uci = ifelse(uci > 1.5, 1.5, uci), lci = ifelse(lci < -1.5, -1.5, lci)) %>%
  mutate(larrow = ifelse(lci == -1.5, 2, NA))   
p_jp <- 
  ggplot(jp, aes(x=number, y=coefficient)) + 
  geom_col(fill = '#d0886b', width = 0.5) +
  geom_errorbar(aes(ymin=lci, ymax=uci), 
                color = '#912e0b', size = 1, width=0) +
  geom_segment(aes(x = number, xend = number, y = coefficient, yend = lci), 
               arrow = arrow(length = unit(jp$larrow, "mm"), type = "closed")) +
  geom_hline(yintercept = 0, size = 0.6) +
  theme(panel.grid.major.x = element_blank(), #no gridlines
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line.y = element_line(color="black", size = 1),
        plot.title = element_text(size = 30,face = "bold", hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5, 
                                   size = c(rep(15, 6), rep(16, 4)), 
                                   #size = 16,
                                   face = "bold"),
        axis.text.y = element_text(size = 21),
        axis.title.y = element_text(size = 24)) + 
  xlab('') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                   labels=c("Entertainment", 
                            "Childcare\nEntertainment", 
                            "Childcare\nEntertainment\nReligion", 
                            "School\nChildcare\nEntertainment\nReligion", 
                            "School\nChildcare\nEntertainment\nReligion\nSports outdoor", 
                            "School\nChildcare\nEntertainment\nReligion\nSports indoor\nSports outdoor", "7 types", "8 types", "9 types", "10 types")) +
  scale_y_continuous(limit = c(-1.5, 1.5), 
                     breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  ggtitle("Japan") +
  coord_fixed(1.2)

# us ----
us <- read.csv("plot/combined_num_us.csv")
us$number <- us$number + 1
us$number <- as.factor(us$number)
us <- us[1: 4, ]
p_us <- 
  ggplot(us, aes(x=number, y=coefficient)) + 
  geom_col(fill = '#d0886b', width = 0.5) +
  geom_errorbar(aes(ymin=coefficient-se*1.96, ymax=coefficient+se*1.96), 
                color = '#912e0b', size = 1, width=0) +
  geom_hline(yintercept = 0, size = 0.6) +
  theme(panel.grid.major.x = element_blank(), #no gridlines
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line.y = element_line(color="black", size = 1),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.text.y = element_text(size = 21),
        axis.title.y = element_text(size = 24)) + 
  xlab('') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8"),
                   labels=c("Entertainment", 
                            "Entertainment\nSports indoor", 
                            "Restaurant\nEntertainment\nSports indoor", 
                            "School\nRestaurant\nEntertainment\nSports indoor", "5 types", "6 types", 
                            "7 types", "8 types")) +
  scale_y_continuous(limit = c(-1.5, 1.5), 
                     breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  ggtitle("United States") +
  coord_fixed(1.2)

# uk ----
uk <- read.csv("plot/combined_num_uk.csv")
uk <- uk %>% 
  mutate(number = as.factor(number)) %>%
  mutate(uci = coefficient + 1.96 * se, lci = coefficient - 1.96 * se) %>%
  mutate(uci = ifelse(uci > 1.5, 1.5, uci), lci = ifelse(lci < -1.5, -1.5, lci)) %>%
  mutate(larrow = ifelse(lci == -1.5, 2, NA))   
p_uk <- ggplot(uk, aes(x=number, y=coefficient)) + 
  geom_col(fill = '#d0886b', width = 0.5) +
  geom_errorbar(aes(ymin=lci, ymax=uci), 
                color = '#912e0b', size = 1, width=0) +
  geom_segment(aes(x = number, xend = number, y = coefficient, yend = lci), 
               arrow = arrow(length = unit(uk$larrow, "mm"), type = "closed")) +
  geom_hline(yintercept = 0, size = 0.3) +
  theme(panel.grid.major.x = element_blank(), #no gridlines
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line.y = element_line(color="black", size = 1),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.text.y = element_text(size = 21),
        axis.title.y = element_text(size = 24)) + 
  xlab('') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_discrete(breaks=c("1", "2", "3"),
                   labels=c("Sports indoor", 
                            "Shop\nSports indoor\nLarge group in", 
                            "Shop\nRestaurant\nCulture\nSports indoor\nSmall group in\nLarge group in")) +
  scale_y_continuous(limit = c(-1.5, 1.5), 
                     breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  ggtitle("United Kingdom") +
  coord_fixed(1.2)

# br ----
br <- read.csv("plot/combined_num_bra.csv")
br$number <- as.factor(br$number)
br <- br[1 : 2, ]
p_br <- ggplot(br, aes(x=number, y=coefficient)) + 
  geom_col(fill = '#d0886b', width = 0.5) +
  geom_errorbar(aes(ymin=coefficient-se*1.96, ymax=coefficient+se*1.96), 
                color = '#912e0b', size = 1, width=0) +
  geom_hline(yintercept = 0, size = 0.6) +
  theme(panel.grid.major.x = element_blank(), #no gridlines
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line.y = element_line(color="black", size = 1),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text.y = element_text(size = 21),
        axis.title.y = element_text(size = 24)) + 
  xlab('') + 
  ylab(expression(Delta*~"log R"[t])) +
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8"),
                   labels=c("Retail", 
                            "Retail\nSports outdoor", 
                            "3 types", "4 types", "5 types", "6 types", 
                            "7 types", "8 types")) +
  scale_y_continuous(limit = c(-1.5, 1.5), 
                     breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  ggtitle("Brazil") +
  coord_fixed(1.2)

library(egg)
p <- ggarrange(p_jp, p_uk, p_us, p_br, draw = F, widths = c(6, 3, 4, 2), col = 2)
ggsave("plot/combine.png", plot = p, height = 20, width = 70,
       units = "cm", dpi = 300, limitsize = F)
