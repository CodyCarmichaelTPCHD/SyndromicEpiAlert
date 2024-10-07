### Time Series Count Graphs

## V1 is just counts


library(dplyr)
library(plotly)

load("MainAPI_Results.RData")
load("IterativeData.RData")


TS_DF <- select(Main_df, Date)
TS_DF$Date <- as.Date(TS_DF$Date, format = "%m/%d/%Y")


TS_DF <- TS_DF %>%
 group_by(Date) %>%
 dplyr::summarize(total=n())



TS_Graph_Main_Count <- plot_ly(data = TS_DF, x = ~Date, y = ~total, type = 'scatter', mode = 'lines') %>%
  layout(title = paste('Time Series Plot for' , Main_df$SyndromeName[1], "for", DF_Iter$StartDate, "-", DF_Iter$EndDate),
         xaxis = list(title = 'Date'),
         yaxis = list(title = 'Visits'))


## v2 - make into section and break down into previously selected sections for age group, sex, race, etc. 







save(TS_DF, file = "TS_Data.RData")
save(TS_Graph_Main_Count, file = "TS_Count_Graph.RData")

