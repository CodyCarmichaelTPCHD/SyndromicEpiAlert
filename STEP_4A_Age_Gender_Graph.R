## 4A: AgeGenderGraphs
library(dplyr)
library(plotly)

load("MainAPI_Results.RData")
load("IterativeData.RData")
## Age and Sex, SyndromeName included.

Age_Sex <- select(Main_df, Age, Sex, SyndromeName)


## Need to chop up age groups into Normal, PIT, and PEDs

Age_Sex$Age <- as.numeric(Age_Sex$Age)

Age_Sex$AgeGroup <- NA

if(DF_Iter$AgeGroup == "Standard"){
  #Age Grouping
  Age_Sex[Age_Sex$Age <= 18, 'AgeGroup'] <- '<18'
  Age_Sex[Age_Sex$Age > 18 & Age_Sex$Age <= 44, 'AgeGroup'] <- '18-44'
  Age_Sex[Age_Sex$Age > 44 & Age_Sex$Age <=64, 'AgeGroup'] <- '45-64'
  Age_Sex[Age_Sex$Age >= 65, 'AgeGroup'] <- '65+'
}



if(DF_Iter$AgeGroup == "Peds"){
  #Age Grouping
  Age_Sex[Age_Sex$Age <= 2, 'AgeGroup'] <- '<2'
  Age_Sex[Age_Sex$Age > 2 & Age_Sex$Age <= 5, 'AgeGroup'] <- '3-5'
  Age_Sex[Age_Sex$Age > 5 & Age_Sex$Age <=8, 'AgeGroup'] <- '6-8'
  Age_Sex[Age_Sex$Age > 8 & Age_Sex$Age <=12, 'AgeGroup'] <- '9-12'
  Age_Sex[Age_Sex$Age > 12 & Age_Sex$Age <= 16, 'AgeGroup'] <- '13-16'
  Age_Sex[Age_Sex$Age > 16 & Age_Sex$Age <=18, 'AgeGroup'] <- '16-18'
  Age_Sex[Age_Sex$Age >= 18, 'AgeGroup'] <- '18+'
}




if(DF_Iter$AgeGroup == "PIT"){
  #Age Grouping
  Age_Sex[Age_Sex$Age < 18, 'AgeGroup'] <- '<18'
  Age_Sex[Age_Sex$Age > 18 & Age_Sex$Age <= 24, 'AgeGroup'] <- '18-24'
  Age_Sex[Age_Sex$Age > 24 & Age_Sex$Age <= 34, 'AgeGroup'] <- '25-34'
  Age_Sex[Age_Sex$Age > 34 & Age_Sex$Age <=44, 'AgeGroup'] <- '35-44'
  Age_Sex[Age_Sex$Age > 44 & Age_Sex$Age <= 54, 'AgeGroup'] <- '45-54'
  Age_Sex[Age_Sex$Age > 54 & Age_Sex$Age <=61, 'AgeGroup'] <- '55-61'
  Age_Sex[Age_Sex$Age >= 62, 'AgeGroup'] <- '62+'
}



Age_Sex[!(Age_Sex$Sex %in% c('F', 'M')), 'Sex'] <- 'Unknown'
Age_Sex[Age_Sex$Sex=='F', 'Sex'] <- 'Female'
Age_Sex[Age_Sex$Sex=='M', 'Sex'] <- 'Male'


Age_Sex$AgeGroupSex <- paste(Age_Sex$Sex, ",", Age_Sex$AgeGroup)
Age_Sex <- select(Age_Sex,AgeGroupSex, SyndromeName)


Age_Sex_Table <- Age_Sex %>%
  group_by(AgeGroupSex, SyndromeName) %>%
  dplyr::summarize(total=n())


Age_Sex_Graph <- plot_ly(x = Age_Sex_Table$AgeGroupSex, y = Age_Sex_Table$total) %>%
  layout(title = paste("Age/Sex for ", Age_Sex_Table$SyndromeName[1], "for", DF_Iter$StartDate, "-", DF_Iter$EndDate), yaxis = list(title = "Frequency of Visits"), xaxis = list(title = "Sex/Age"))


save(Age_Sex_Table, file = "AgeSexData.RData")
save(Age_Sex_Graph, file = "AgeSexGraph.RData")

