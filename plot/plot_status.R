library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

intervention_plot <- function(intervention, cols, titles, 
                              city_eng_name, palette){
  for (i in 1:length(intervention)){
    int <- intervention[i]
    t <- input %>% select(date, city_eng_name, int) %>%
      spread(date, int) %>%
      mutate(intervention = do.call(paste, c(.[cols], sep = "-"))) 
    t2 <- as.data.frame(table(t$intervention))
    t <- t %>% select(-city_eng_name) %>% 
      distinct() %>%
      merge(t2, by.x = "intervention", by.y = "Var1") %>%
      gather("date", int, 2:length(cols))
    t$interventionid <- LETTERS[1:nrow(t2)]
    t$date <- as.Date(t$date)
    fig <- ggplot(t) + 
      geom_line(aes(x = date, y = int, group = intervention,
                    lwd = Freq), color = palette[i], alpha = 0.5) +
      scale_size(range = c(0.5, 5), guide = FALSE) +
      theme(panel.background = element_rect(fill = "white",colour = "black",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5,size = 38,margin = margin(0, 0, 10, 0)),
            axis.text = element_text(size = 20),#20 for uk 23for jp us br
            axis.text.y = element_text(size = 25),
            axis.title.y = element_text(size = 35, margin = margin(0, 10, 0, 0)),
            axis.title.x = element_text(size = 35, margin = margin(10, 0, 0, 0)),
            plot.margin = unit(c(1, 1, 1, 1), "cm"))+
      labs(title = titles[i], x = "Time", y = "Status") +
      scale_x_date(date_breaks = "1 month",
                   date_labels = '%b-%d')+
      scale_y_continuous(breaks = c(0, 0.5, 1))
    assign(paste0("fig", i), fig, env = .GlobalEnv)
  }
}
intervention <- c("win7_school_close", "win7_childcare_close", 
                  'win7_office_close', 'win7_shop_close', 'win7_restaurant_close', 
                  'win7_bar_close', "win7_entertainment_close", "win7_cultural_close", 
                  "win7_worship_close", "win7_sports_indoor_close", "win7_sports_outdoor_close")
titles <- c("School closure", "Childcare closure", 
            "Office closure", "Non-essential retail closure", "Restaurant closure", 
            "Bar closure", "Entertainment venue closure", 
            "Cultural venue closure", "Religious venue closure", 
            "Indoor sports closure", "Outdoor sports closure")
palette <- brewer.pal(11, 'Set3')
palette[2] <- palette[10] 
palette[9] <- palette[6] 

# japan ----
input <- read.csv("jp/jp_input_45counties.csv", stringsAsFactors = F)
cols <- seq(as.Date("2020/4/1"), as.Date("2020/8/15"), "days") %>% as.character()
intervention_plot(setdiff(intervention, 'win7_office_close'), 
                  cols, setdiff(titles, 'Office closure'), 'division_eng_name', palette)
fig <- ggarrange(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, 
                 fig9, fig10, 
                 #labels = c("A", rep("", 9)), 
                 #font.label = list(size=50), 
                 ncol = 4, nrow = 3)
ggsave("plot/policy_jp.png", fig, width = 80, height = 45, units = "cm", 
       dpi = 100, limitsize = F)


# uk ----
input <- read.csv("uk/uk_input_234lda_final.csv", stringsAsFactors = F)
input <- input %>% mutate(date = as.Date(date)) %>% 
  filter(date > as.Date('2020-02-20'))
cols <- seq(as.Date("2020/2/21"), as.Date("2020/8/15"), "days") %>% as.character()
intervention_plot(intervention, cols, titles, 'areaName', palette)
fig <- ggarrange(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, 
                 fig9, fig10, fig11, 
                 #labels = c("B", rep("", 10)), 
                 #font.label = list(size=50), 
                 ncol = 4, nrow = 3)
ggsave("plot/policy_uk.png", fig, width = 80, height = 45, units = "cm", 
       dpi = 100, limitsize = F)

# us ----
input <- read.csv("us/us_input_msa2.csv", stringsAsFactors = F)
input <- filter(input, !city_eng_name %in% c('Lawton, OK', 'Oklahoma City, OK', 'Tulsa, OK'))
cols <- seq(as.Date("2020/3/13"), as.Date("2020/8/15"), "days") %>% as.character()
intervention_plot(setdiff(intervention, c('win7_office_close', 'win7_sports_outdoor_close')), 
                  cols, setdiff(titles, c('Office closure', "Outdoor sports closure")), 
                  'city_eng_name', palette)
fig <- ggarrange(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, 
                 fig9,
                 #labels = c("C", rep("", 8)), 
                 #font.label = list(size=50), 
                 ncol = 4, nrow = 3)
ggsave("plot/policy_us.png", fig, width = 80, height = 45, units = "cm", 
       dpi = 100, limitsize = F)


# brazil ----
input <- read.csv("bra/brazil_input_319cities_final.csv", stringsAsFactors = F)
cols <- seq(as.Date("2020/4/3"), as.Date("2020/8/15"), "days") %>% as.character()
intervention_plot(intervention, cols, titles, 'code', palette)
fig <- ggarrange(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, 
                 fig9, fig10, fig11,
                 #labels = c("D", rep("", 10)), 
                 #font.label = list(size=50), 
                 ncol = 4, nrow = 3)
ggsave("plot/policy_br.png", fig, width = 80, height = 45, units = "cm", 
       dpi = 100, limitsize = F)

