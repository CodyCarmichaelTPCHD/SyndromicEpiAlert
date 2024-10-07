## Hospital Groups

library(dplyr)
library(plotly)

load("MainAPI_Results.RData")
load("IterativeData.RData")



Main_df <- select(Main_df, SyndromeName, HospitalName)




Hosp_Only_Table <- Main_df %>%
  group_by(HospitalName, SyndromeName) %>%
  dplyr::summarize(total=n())

Hosp_Only_Table$HospitalName <- gsub("WA-H_", "", Hosp_Only_Table$HospitalName)

Hosp_Only_Graph <- plot_ly(x = Hosp_Only_Table$HospitalName, y = Hosp_Only_Table$total) %>%
  layout(title = paste("Hospital Groups for ", Hosp_Only_Table$SyndromeName[1], "for", DF_Iter$StartDate, "-", DF_Iter$EndDate), yaxis = list(title = "Frequency of Visits"), xaxis = list(title = "Hospital Group", tickangle = 45))



save(Hosp_Only_Table, file = "HospOnlyData.RData")
save(Hosp_Only_Graph, file = "HospOnlyGraph.RData")

