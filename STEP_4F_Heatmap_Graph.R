## Time of Day Analysis


library(dplyr)
library(plotly)
library(lubridate)

load("MainAPI_Results.RData")
load("IterativeData.RData")

df <- Main_df

# Combine Date and Time into POSIXct datetime
df$Datetime <- as.POSIXct(paste(df$Date, df$Time), format="%m/%d/%Y %I:%M %p")

# Extract Day of Week
df$DayOfWeek <- weekdays(as.Date(df$Datetime))

# Convert DayOfWeek into an ordered factor with Sunday as the first day
df$DayOfWeek <- factor(df$DayOfWeek, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))



# Define 2-hour blocks
df$TimeBlock <- cut(as.numeric(format(df$Datetime, "%H")), breaks=seq(0, 24, by=2), include.lowest=TRUE, labels=FALSE)


time_labels <- c("00:00-02:00", "02:00-04:00", "04:00-06:00", "06:00-08:00", "08:00-10:00", 
                 "10:00-12:00", "12:00-14:00", "14:00-16:00", "16:00-18:00", "18:00-20:00", 
                 "20:00-22:00", "22:00-00:00")
df$TimeBlock <- factor(df$TimeBlock, levels=1:12, labels=time_labels)



# Prepare data for heatmap
heatmap_data <- df %>%
  group_by(DayOfWeek, TimeBlock) %>%
  dplyr::summarise(Count = n(), .groups = 'drop')


# Generate heatmap
HeatMap <- plot_ly(data = heatmap_data, x = ~DayOfWeek, y = ~TimeBlock, z = ~Count, type = "heatmap") %>%
  layout(title = paste("Heatmap of Day of Week vs. 2-Hour Blocks for", df$SyndromeName[1], DF_Iter$StartDate[1], "-", DF_Iter$EndDate[1]),
         xaxis = list(title = "Day of the Week"),
         yaxis = list(title = "2-Hour Block", tickmode = "array", tickvals = time_labels, ticktext = time_labels))

save(heatmap_data, file = "HeatMapData.RData")
save(HeatMap, file = "HeatmapGraph.RData")

