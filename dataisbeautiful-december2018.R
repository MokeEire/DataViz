library(tidyverse)
library(lubridate)
library(scales)
library(extrafont)
df = read_csv("History of Ice on Lake Mendota - http___www.aos.wisc.edu__sco_lakes_Mendota-ice.html - Sheet1.csv")

df_dt = df %>% mutate(closedate = dmy(paste(CLOSED, ifelse(str_detect(CLOSED, "Jan|Feb|Mar"), `END YEAR`,`START YEAR`))), 
              opendate = dmy(paste(OPENED, ifelse(str_detect(OPENED, "Nov|Dec"),`START YEAR`, `END YEAR`))), 
              frozen = interval(start = closedate, end = opendate), 
              thawed = interval(start = opendate, end = lead(closedate))) %>% select(closedate:thawed)
frozen_list = as.list(df_dt$frozen)


df2 = data_frame(month_year = seq(ymd('1855-01-01'),ymd('2018-12-31'), by = 'days'))
#df2 = df2 %>% mutate(diff = make_difftime(days = days), month_interval = as.interval(diff, month_year))

df2 = df2 %>% mutate(state = if_else(month_year %within% frozen_list, "Frozen", "Thawed"), 
               year = year(month_year), 
               date_scaled = dmy(paste(day(month_year), month(month_year),"2016", sep="-"))) %>% 
  filter(month_year > dmy("01-06-1855"))

df2 %>% 
  ggplot(.,aes(y = year, x=date_scaled, colour = state))+
  geom_jitter(size = 2.5)+
  scale_x_date(breaks = seq(as.Date("1855-07-15"), 
                            as.Date("2018-12-15"), by = "1 month"), 
               date_labels = "%B", 
               expand = expand_scale(mult = c(0.025,0)))+
  scale_y_continuous(breaks = c(1855, 1900,1950, 2000,2018), 
                     minor_breaks = c(1855, 1875, 1900, 1925,1950,1975, 2000,2018), 
                     expand = expand_scale(mult = c(0.04,0.01)))+
  scale_colour_manual(values = c("Frozen" = "#a6cee3", "Thawed" = "#68B063"), 
                      guide = guide_legend(title = NULL, override.aes = list(size = 6)))+
  labs(title="Freezing and Thawing cycle of Lake Mendota, Wisconsin (1855-2018)",
       subtitle="",
       caption = "Source: Wisconsin State Climatology Office") +
  theme_minimal()+
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = rel(1.05)),
        legend.key.size = unit(.5, "cm"),
        legend.margin = margin(.025,.025,.025,.025, "cm"),
        plot.margin = unit(c(.5,.75,.5,.5), "cm"),
        axis.text.y = element_text(size = rel(1.35), face = "bold"),
        axis.text.x = element_text(size = rel(1.35)),
        plot.caption = element_text(colour = "gray"),
        plot.title = element_text(size = rel(1.5)),
        text = element_text(family = "Constantia"))
