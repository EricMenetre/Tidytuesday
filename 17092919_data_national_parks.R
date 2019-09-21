##################################################################
##                           TRAINING                           ##
##################################################################

park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")

library(dplyr)
library(ggplot2)
library(ggpubr)
glimpse(gas_price)
glimpse(park_visits)
glimpse(state_pop)

View(gas_price)
View(park_visits)

# How does the number of visitors in a year increased over time?

# Dark custom version

years <- c(1906)
for (i in 1:11){
  years[i+1] <- years[i] + 10
  
}
years
park_visits$year <- as.numeric(park_visits$year)

dark.plot <- park_visits%>%
  group_by(year)%>%
  summarise(mean_visitors = mean(visitors, na.rm = TRUE))%>%
  filter(year != "Total")%>%
  ggplot(aes(x = year, y = mean_visitors))+
           geom_bar(stat = "identity", width = 0.5, color = "gray", fill = "gray")+ 
            geom_smooth(color = "red", se=F, method = "auto")+
            theme_minimal()+
            theme(axis.text.x = element_text(angle = 45), 
                  panel.background = element_rect(color = "black", fill = "black"), 
                  plot.background = element_rect(fill = "black"),
                  axis.text = element_text(color = "white"),
                  panel.grid = element_blank())+
            labs(title = "Number of visitors over time") + 
            theme(title = element_text(color = "white", size = 20),
                  axis.title = element_blank())+
            scale_x_continuous(breaks = years)

sunny.plot <- park_visits%>%
  filter(year != "Total")%>%
  group_by(year)%>%
  summarise(mean_visitors = mean(visitors, na.rm = TRUE))%>%
  ggplot(aes(x = year, y = mean_visitors))+
  geom_bar(stat = "identity", color = "gray50", width = 0.5)+ 
  geom_smooth(se=F, col = "yellow4")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45), 
        plot.background = element_rect("lightblue1"), 
        plot.title = element_text(size = 20))+
  labs(title = "Number of visitors over time")+
  scale_x_continuous(breaks = years)
  


# Does the number of visitors in a year is depending on the price of gas ? 

park_visits_mini <- park_visits %>%
  group_by(year, visitors)%>%
  filter(year != "Total")%>%
  summarise()
View(park_visits_mini)

    # Now let's add the gas data
park_visits_mini$gas_price <- NA
for (i in 1:nrow(gas_price)){
  for(j in 1:nrow(park_visits_mini)){
    if (gas_price$year[i] == park_visits_mini$year[j]) {
      park_visits_mini$gas_price[j] <- gas_price$gas_current[i]
    } else{
      next
    }
  }
}

park_visits_mini$gas_price_constant <- NA
for (i in 1:nrow(gas_price)){
  for(j in 1:nrow(park_visits_mini)){
    if (gas_price$year[i] == park_visits_mini$year[j]) {
      park_visits_mini$gas_price_constant[j] <- gas_price$gas_constant[i]
    } else{
      next
    }
  }
}

glimpse(park_visits_mini)

plot.uncorrected <- park_visits_mini %>%
  ggplot(aes(x = log(gas_price), y = log(visitors)))+
  geom_point(stat = "identity", alpha = 0.1, col = "gray39") + 
  geom_smooth(method = "lm", color = "gray25")+
  theme_minimal()+
  labs(x = "log values of non adjusted gas prices", 
       y = "log values of the number of visitors")

plot.corrected <- park_visits_mini %>%
  ggplot(aes(x = log(gas_price_constant), y = log(visitors)))+
  geom_point(stat = "identity", alpha = 0.1, col = "gray39") + 
  geom_smooth(method = "lm", color = "gray25")+
  theme_minimal()+
  labs(x = "log values of adjusted gas prices")+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())

plot.merge <- ggarrange(plot.uncorrected, plot.corrected)
plot.confound <- annotate_figure(plot.merge, 
                top = "Relationship between gas prices and number of visitors")



