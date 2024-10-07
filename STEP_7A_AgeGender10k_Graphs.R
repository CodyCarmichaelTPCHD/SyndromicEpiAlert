## Step 7A AgeGender 10k Graphs

## 4A: AgeGenderGraphs

library(plotly)
library(dplyr)

load("MainAPI_Results.RData")
load("IterativeData.RData")
load("DF_10k.RData")


Age_Sex <- select(Main_df, Age, Sex, SyndromeName)
Age_Sex_10k <- select(Alldf, Age, Sex)

## Need to chop up age groups into Normal, PIT, and PEDs

Age_Sex$Age <- as.numeric(Age_Sex$Age)
Age_Sex_10k$Age <- as.numeric(Age_Sex_10k$Age)




Age_Sex$AgeGroup <- NA
Age_Sex_10k$AgeGroup <- NA



if(DF_Iter$AgeGroup == "Standard"){
  #Age Grouping
  Age_Sex[Age_Sex$Age <= 18, 'AgeGroup'] <- '<18'
  Age_Sex[Age_Sex$Age > 18 & Age_Sex$Age <= 44, 'AgeGroup'] <- '18-44'
  Age_Sex[Age_Sex$Age > 44 & Age_Sex$Age <=64, 'AgeGroup'] <- '45-64'
  Age_Sex[Age_Sex$Age >= 65, 'AgeGroup'] <- '65+'
  
  
  Age_Sex_10k[Age_Sex_10k$Age <= 18, 'AgeGroup'] <- '<18'
  Age_Sex_10k[Age_Sex_10k$Age > 18 & Age_Sex_10k$Age <= 44, 'AgeGroup'] <- '18-44'
  Age_Sex_10k[Age_Sex_10k$Age > 44 & Age_Sex_10k$Age <=64, 'AgeGroup'] <- '45-64'
  Age_Sex_10k[Age_Sex_10k$Age >= 65, 'AgeGroup'] <- '65+'
  
  
  
  
  
  
  
  
  
  
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
  
  Age_Sex_10k[Age_Sex_10k$Age <= 2, 'AgeGroup'] <- '<2'
  Age_Sex_10k[Age_Sex_10k$Age > 2 & Age_Sex_10k$Age <= 5, 'AgeGroup'] <- '3-5'
  Age_Sex_10k[Age_Sex_10k$Age > 5 & Age_Sex_10k$Age <=8, 'AgeGroup'] <- '6-8'
  Age_Sex_10k[Age_Sex_10k$Age > 8 & Age_Sex_10k$Age <=12, 'AgeGroup'] <- '9-12'
  Age_Sex_10k[Age_Sex_10k$Age > 12 & Age_Sex_10k$Age <= 16, 'AgeGroup'] <- '13-16'
  Age_Sex_10k[Age_Sex_10k$Age > 16 & Age_Sex_10k$Age <=18, 'AgeGroup'] <- '16-18'
  Age_Sex_10k[Age_Sex_10k$Age >= 18, 'AgeGroup'] <- '18+'
  
  
  
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
  
  
  Age_Sex_10k[Age_Sex_10k$Age < 18, 'AgeGroup'] <- '<18'
  Age_Sex_10k[Age_Sex_10k$Age > 18 & Age_Sex_10k$Age <= 24, 'AgeGroup'] <- '18-24'
  Age_Sex_10k[Age_Sex_10k$Age > 24 & Age_Sex_10k$Age <= 34, 'AgeGroup'] <- '25-34'
  Age_Sex_10k[Age_Sex_10k$Age > 34 & Age_Sex_10k$Age <=44, 'AgeGroup'] <- '35-44'
  Age_Sex_10k[Age_Sex_10k$Age > 44 & Age_Sex_10k$Age <= 54, 'AgeGroup'] <- '45-54'
  Age_Sex_10k[Age_Sex_10k$Age > 54 & Age_Sex_10k$Age <=61, 'AgeGroup'] <- '55-61'
  Age_Sex_10k[Age_Sex_10k$Age >= 62, 'AgeGroup'] <- '62+'
  
  
  
  
}



Age_Sex[!(Age_Sex$Sex %in% c('F', 'M')), 'Sex'] <- 'Unknown'
Age_Sex[Age_Sex$Sex=='F', 'Sex'] <- 'Female'
Age_Sex[Age_Sex$Sex=='M', 'Sex'] <- 'Male'


Age_Sex_10k[!(Age_Sex_10k$Sex %in% c('F', 'M')), 'Sex'] <- 'Unknown'
Age_Sex_10k[Age_Sex_10k$Sex=='F', 'Sex'] <- 'Female'
Age_Sex_10k[Age_Sex_10k$Sex=='M', 'Sex'] <- 'Male'



Age_Sex$AgeGroupSex <- paste(Age_Sex$Sex, ",", Age_Sex$AgeGroup)
Age_Sex <- select(Age_Sex,AgeGroupSex, SyndromeName)

Age_Sex_10k$AgeGroupSex <- paste(Age_Sex_10k$Sex, ",", Age_Sex_10k$AgeGroup)
Age_Sex_10k <- select(Age_Sex_10k,AgeGroupSex)






# Step 3: Calculate the counts
count_Age_Sex <- Age_Sex %>% dplyr::group_by(AgeGroupSex) %>% dplyr::summarise(Count = n())
count_Age_Sex_10k <- Age_Sex_10k %>% dplyr::group_by(AgeGroupSex) %>% dplyr::summarise(Count = n())

# Step 4: Merge the dataframes
merged_df <- merge(count_Age_Sex, count_Age_Sex_10k, by = "AgeGroupSex")

# Step 5: Calculate the ratio
merged_df$Ratio <- (merged_df$Count.x / merged_df$Count.y) * 10000



# Step 6: Create the plot
AS_10K_plot <- plot_ly(merged_df, x = ~AgeGroupSex, y = ~Ratio, type = 'bar', name = 'Visits per 10,000 by Age/Sex') %>%
  layout(title = 'Visits per 10,000 of the Same Age Group and Sex',
         xaxis = list(title = 'Age Group and Sex'),
         yaxis = list(title = 'Visits by Sex/Age Group, per 10,000'))







AS_10k_Table <- merged_df

colnames(AS_10k_Table) <- c("Age Group and Sex", "Syndrome Count", "All Visits", "Per 10k")


save(AS_10k_Table, file = "Per10kAgeSexTable.RData")
save(AS_10K_plot, file  = "Per10kAgeSexPlot.RData")