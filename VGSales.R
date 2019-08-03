library(tidyverse) #mainly for dyplr
library(plotly) #to make prettier graphs
library(ggplot2) #to occasionally base plots off of

#Let's make some graphs to understand video game sales!
vg2016 <- vgsales %>% filter(Year == 2016)
vge <- vgsales %>% filter(Rating == "E")
vgt <- vgsales %>% filter(Rating == "T")
vgm <- vgsales %>% filter(Rating == "M")

#plot 1: Bar plot of genre frequency
genre_table <- vgsales %>% count(Genre)
genre_bar <- genre_table %>% 
  plot_ly(x = ~Genre, y = ~n) %>% 
  layout(title = 'Frequency of Video Games by Genre',
         xaxis = list(title = "Genre"),
         yaxis = list(title = "Number of Games")) %>%
  add_bars()

#plot 2: Bar plot of console frequency
console_table <- vgsales %>% count(Platform) 
console_bar <- console_table %>% 
  plot_ly(x = ~Platform, y = ~n, color = "Orange") %>% 
  layout(title = 'Frequency of Video Games by Platform',
         xaxis = list(title = "Platforms"),
         yaxis = list(title = "Number of Games")) %>%
  add_bars()

#plot 3: Color Scale scatter of Critic by User Score (working but ugly)
color_critic <- vg2016 %>% plot_ly(
  type = 'scatter',
  mode = 'markers',
  x = ~User_Score,
  y = ~Critic_Score,
  marker = list (
    size = seq(0, 30),
    color = seq(20, 50),
    colorbar = list(title = "Colorbar"),
    colorscale = 'viridis',
    reverescale = TRUE
  )
) %>% layout(
  xaxis = list(title = "User Scores", 
               showgrid = FALSE, zeroline = FALSE),
  yaxis = list(title = "Critic Scores", 
               showgrid = FALSE, zeroline = FALSE)
) %>% add_markers()

#plot 4: Color Scale Scatter of NA Sales by EU Sales (working but ugly)
color_sales <- vg2016 %>% plot_ly (
  x = ~NA_Sales,
  y = ~EU_Sales,
  type = 'scatter',
  mode = 'markers',
  marker = list(
    size = seq(0, 20),
    color = seq(0, 30),
    colorbar = list(title = "Colorbar"),
    colorscale = 'viridis',
    reverescale = TRUE
  ) 
  )%>% layout(
    xaxis = list(title = "North American Sales",
                 showgrid = FALSE, zeroline = FALSE),
    yaxis = list(title = "European Sales",
                 showgrid = FALSE, zeroline = FALSE)
) %>% add_markers()

#plot 5: Normal scatterplot of NA sales by EU Sales, colored by rating
usEU <- vgsales %>% 
  plot_ly(x = ~NA_Sales, y = ~EU_Sales, color = ~Rating) %>%
  add_markers()

#plot 6: normal scatterplot of critic by user score, colored by genre
criticUser <- vg2016 %>% 
  plot_ly(x = ~Critic_Score, y = ~User_Score, color = ~Genre) %>%
  add_markers()

#plot 7: The Sales of every Genre, split by rating
barStack <- vg2016 %>%
  plot_ly(x = ~Genre, y = ~NA_Sales, color = ~Rating) %>%
  layout(barmode = 'stack')

#plot 8: scatter of NA sales by user score, colored by rating
usCrit <- vg2016 %>%
  plot_ly(x = ~User_Score, y = ~NA_Sales, color = ~Rating) %>%
  add_markers()

#plot 9: Density graph of Ratings vs. Critic Score
d.e <- density(vge$Critic_Score, na.rm = TRUE)
d.t <- density(vgt$Critic_Score, na.rm = TRUE)
d.m <- density(vgm$Critic_Score, na.rm = TRUE)

densityCritic <- plot_ly() %>%
  add_lines(x = ~d.e$x, y = ~d.e$y, 
            name = "E for Everyone", fill = 'tozerroy') %>%
  add_lines(x = ~d.t$x, y = ~d.t$y, 
            name = "T for Tean", fill = 'tozerroy') %>%
  add_lines(x = ~d.m$x, y = ~d.m$y, 
            name = "M for Mature", fill = 'tozerroy') %>%
  layout(
    title = "Density graph of Critic Score, Seperated by Rating",
    xaxis = list(title = "Critic Score"),
    yaxis = list(title = "Density")
  )
