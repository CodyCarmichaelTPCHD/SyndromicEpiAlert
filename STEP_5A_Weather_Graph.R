# Step 5A, Weather Graphics
library(plotly)


load("Main_Weather_DF.RData")

Weather_TS <- plot_ly(data = Weather_DF, x = ~Date) %>%
  add_trace(y = ~MaxTemp, name = 'High Temp', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~AvgTemp, name = 'Avg Temp', type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = 'rgba(255, 0, 0, 0.3)') %>%
  add_trace(y = ~MinTemp, name = 'Low Temp', type = 'scatter', mode = 'lines', fill = 'tonexty', fillcolor = 'rgba(0, 0, 255, 0.3)') %>%
  layout(title = paste('Temperature Trends for', min(Weather_DF$Date), "to", max(Weather_DF$Date) ), xaxis = list(title = 'Date'), yaxis = list(title = 'Temperature'))



save(Weather_TS, file = "Weather_TS.RData")
write.csv(Weather_DF, file = "Temperatures.csv")
save(Weather_DF, file = "Weather_Data.RData")