## 6A PM25

library(plotly)
library(dplyr)

load("PM25_DF.RData")

min_date <- min(PM_25_DF$date)
max_date <- max(PM_25_DF$date)

PM25_Graph <- plot_ly(PM_25_DF, x = ~date) %>%
  add_lines(y = ~count, name = "Actual PM2.5 Level", mode = "lines", line = list(color = 'blue')) %>%
  add_lines(y = ~expected, name = "Expected PM2.5 Level", mode = "lines", line = list(color = 'red')) %>%
  layout(title = paste("PM2.5 Graph for", format(min_date, "%Y-%m-%d"), "-", format(max_date, "%Y-%m-%d")),
         xaxis = list(title = "Date"),
         yaxis = list(title = "PM2.5 Level"))



save(PM25_Graph, file = "PM25_Graph.RData")