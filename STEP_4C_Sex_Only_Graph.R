##4C Gender Only

library(dplyr)
library(plotly)

load("MainAPI_Results.RData")
load("IterativeData.RData")
## Age and Sex, SyndromeName included.

Sex_Only <- select(Main_df, Sex, SyndromeName)


Sex_Only[!(Sex_Only$Sex %in% c('F', 'M')), 'Sex'] <- 'Unknown'
Sex_Only[Sex_Only$Sex=='F', 'Sex'] <- 'Female'
Sex_Only[Sex_Only$Sex=='M', 'Sex'] <- 'Male'



Sex_Only_Table <- Sex_Only %>%
  group_by(Sex, SyndromeName) %>%
  dplyr::summarize(total=n())


Sex_Only_Graph <- plot_ly(x = Sex_Only_Table$Sex, y = Sex_Only_Table$total) %>%
  layout(title = paste("Sex for ", Sex_Only_Table$SyndromeName[1], "for", DF_Iter$StartDate, "-", DF_Iter$EndDate), yaxis = list(title = "Frequency of Visits"), xaxis = list(title = "Sex"))


save(Sex_Only_Table, file = "SexOnlyData.RData")
save(Sex_Only_Graph, file = "SexOnlyGraph.RData")

