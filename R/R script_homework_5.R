library(tidyverse)
library(magrittr)
library(ggthemes)
library(scales)
library(lubridate)
library(ggthemes)


homocides <- read_csv(file = "data-raw/homicide-data.csv")%>% 
unite(col = city_state, c(city, state), sep = ", ") %>% 
filter(city_state == "Baltimore, MD") %>%
  mutate(reported_date = ymd(reported_date)) %>% 
    mutate(year = year(reported_date), month = month(reported_date)) %>% 
  group_by(year, month) %>% count() %>%  
 mutate(date = make_date(year, month))
Arrest_of_Freddie <- homocides %>% filter(date == "2015-04-01")


ggplot(homocides, aes(x = date, y = n)) +
 geom_bar(stat = "identity") +
  geom_vline(xintercept = as.numeric(as.Date("2015-04-01")), 
                        color = "red", size = 0.7, linetype = "dashed" )+
  geom_text(data = Arrest_of_Freddie, aes(label = "Arrest of Freddie Gray"),
            color = "red",  hjust = 1.05, vjust = -8) + 
  geom_smooth( method = 'loess', se = FALSE, span = 0.1) +
  labs(title = "Homicides in Baltimore, MD", x = "Date", y = "Monthly homicides") +
  theme_dark()


   
  


