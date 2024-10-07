## 4B AgeOnly Graphs

library(dplyr)
library(plotly)

load("MainAPI_Results.RData")
load("IterativeData.RData")
## Age and Sex, SyndromeName included.

Age_Only <- select(Main_df, Age, SyndromeName)


## Need to chop up age groups into Normal, PIT, and PEDs

Age_Only$Age <- as.numeric(Age_Only$Age)

Age_Only$AgeGroup <- NA

if(DF_Iter$AgeGroup == "Standard"){
  #Age Grouping
  Age_Only[Age_Only$Age <= 18, 'AgeGroup'] <- '<18'
  Age_Only[Age_Only$Age > 18 & Age_Only$Age <= 44, 'AgeGroup'] <- '18-44'
  Age_Only[Age_Only$Age > 44 & Age_Only$Age <=64, 'AgeGroup'] <- '45-64'
  Age_Only[Age_Only$Age >= 65, 'AgeGroup'] <- '65+'
}



if(DF_Iter$AgeGroup == "Peds"){
  #Age Grouping
  Age_Only[Age_Only$Age <= 2, 'AgeGroup'] <- '<2'
  Age_Only[Age_Only$Age > 2 & Age_Only$Age <= 5, 'AgeGroup'] <- '3-5'
  Age_Only[Age_Only$Age > 5 & Age_Only$Age <=8, 'AgeGroup'] <- '6-8'
  Age_Only[Age_Only$Age > 8 & Age_Only$Age <=12, 'AgeGroup'] <- '9-12'
  Age_Only[Age_Only$Age > 12 & Age_Only$Age <= 16, 'AgeGroup'] <- '13-16'
  Age_Only[Age_Only$Age > 16 & Age_Only$Age <=18, 'AgeGroup'] <- '16-18'
  Age_Only[Age_Only$Age >= 18, 'AgeGroup'] <- '18+'
}




if(DF_Iter$AgeGroup == "PIT"){
  #Age Grouping
  Age_Only[Age_Only$Age < 18, 'AgeGroup'] <- '<18'
  Age_Only[Age_Only$Age > 18 & Age_Only$Age <= 24, 'AgeGroup'] <- '18-24'
  Age_Only[Age_Only$Age > 24 & Age_Only$Age <= 34, 'AgeGroup'] <- '25-34'
  Age_Only[Age_Only$Age > 34 & Age_Only$Age <=44, 'AgeGroup'] <- '35-44'
  Age_Only[Age_Only$Age > 44 & Age_Only$Age <= 54, 'AgeGroup'] <- '45-54'
  Age_Only[Age_Only$Age > 54 & Age_Only$Age <=61, 'AgeGroup'] <- '55-61'
  Age_Only[Age_Only$Age >= 62, 'AgeGroup'] <- '62+'
}





Age_Only_Table <- Age_Only %>%
  group_by(AgeGroup, SyndromeName) %>%
  dplyr::summarize(total=n())


Age_Only_Graph <- plot_ly(x = Age_Only_Table$AgeGroup, y = Age_Only_Table$total) %>%
  layout(title = paste("Age Groups for ", Age_Only_Table$SyndromeName[1], "for", DF_Iter$StartDate, "-", DF_Iter$EndDate), yaxis = list(title = "Frequency of Visits"), xaxis = list(title = "Age Group"))


save(Age_Only_Table, file = "AgeOnlyData.RData")
save(Age_Only_Graph, file = "AgeOnlyGraph.RData")