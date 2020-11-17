#1a
install.packages("coronavirus")
library(coronavirus)
data("coronavirus")
#1b
head(coronavirus,100)
#1c 
#Date represents the day of the summary 
#Province represents the province or state from where this information was garnered
#Country represents what country the data is representing 
#Lat is the latitude point 
#Long is the longitude point
#Type is the type of case that the data is confirming 
#Cases represents the number of daily cases that corresponds to the type of case


#2a
library(dplyr)
summary_df = coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)
summary_df %>% head(20)

#2b
library(ggplot2)
Country = summary_df$country[1:5]
Total_Cases = summary_df$total_cases[1:5]
plot = ggplot(datat = coronavirus, 
              mapping = aes(x = Country , y = Total_Cases))+geom_bar(stat = "identity",
                                                                     fill = "dodgerblue", #the color of the bars has been changed to dodgerblue
                                                                     color= "grey40", #the color of the outline of the bars has been changed to grey40
                                                                     alpha =.5) + theme_bw()+ #alpha has been changed to .5, making the bars more translucent
                                                                                              #theme has been set to a black and white theme
                                                                     xlab("Countries")+ylab("Total Cases")+ #the x and y axes have been labeled accordingly
                                                                     geom_label(aes(label = Total_Cases), # each bar is labeled with the amount of cases it represents
                                                                                label.size = .20)+ #this sets the size of the label above
                                                                      labs(title = "Top 5 countries by total cases") #creates title for vertical graph
plot

#2c and d
plot_horizontal = plot +coord_flip()+ labs(title = "Top 5 countries by total cases")+ theme_light() #changed theme to  light from b/w
plot_horizontal

#3a
library(tidyr)
recent_cases = coronavirus %>% 
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(date) %>%
  mutate(active = confirmed - death - recovered) %>%
  mutate(active_total = cumsum(active),
         recovered_total = cumsum(recovered),
         death_total = cumsum(death))
recent_cases

#3b


library(plotly)

coronavirus %>% 
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(date) %>%
  mutate(active = confirmed - death - recovered) %>%
  mutate(active_total = cumsum(active),
         recovered_total = cumsum(recovered),
         death_total = cumsum(death)) %>%
  plot_ly(x = ~ date,
          y = ~ active_total,
          name = 'Active', 
          type = 'scatter',
          mode = 'lines')%>%
    layout(title = "Recent Cases Of COVID", xaxis = list(title = "Date"), yaxis = list(title= "Active Total Cases"))
  
