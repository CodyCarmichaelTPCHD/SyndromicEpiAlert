# 7B Age only

library(dplyr)
library(plotly)

load("MainAPI_Results.RData")
load("IterativeData.RData")
load("DF_10k.RData")


Age <- select(Main_df, Age, SyndromeName)
Age_10k <- select(Alldf, Age)

## Need to chop up age groups into Normal, PIT, and PEDs

Age$Age <- as.numeric(Age$Age)
Age_10k$Age <- as.numeric(Age_10k$Age)




Age$AgeGroup <- NA
Age_10k$AgeGroup <- NA



if(DF_Iter$AgeGroup == "Standard"){
  #Age Grouping
  Age[Age$Age <= 18, 'AgeGroup'] <- '<18'
  Age[Age$Age > 18 & Age$Age <= 44, 'AgeGroup'] <- '18-44'
  Age[Age$Age > 44 & Age$Age <=64, 'AgeGroup'] <- '45-64'
  Age[Age$Age >= 65, 'AgeGroup'] <- '65+'
  
  
  Age_10k[Age_10k$Age <= 18, 'AgeGroup'] <- '<18'
  Age_10k[Age_10k$Age > 18 & Age_10k$Age <= 44, 'AgeGroup'] <- '18-44'
  Age_10k[Age_10k$Age > 44 & Age_10k$Age <=64, 'AgeGroup'] <- '45-64'
  Age_10k[Age_10k$Age >= 65, 'AgeGroup'] <- '65+'
  
  
  
  
  
  
  
  
  
  
}



if(DF_Iter$AgeGroup == "Peds"){
  #Age Grouping
  Age[Age$Age <= 2, 'AgeGroup'] <- '<2'
  Age[Age$Age > 2 & Age$Age <= 5, 'AgeGroup'] <- '3-5'
  Age[Age$Age > 5 & Age$Age <=8, 'AgeGroup'] <- '6-8'
  Age[Age$Age > 8 & Age$Age <=12, 'AgeGroup'] <- '9-12'
  Age[Age$Age > 12 & Age$Age <= 16, 'AgeGroup'] <- '13-16'
  Age[Age$Age > 16 & Age$Age <=18, 'AgeGroup'] <- '16-18'
  Age[Age$Age >= 18, 'AgeGroup'] <- '18+'
  
  Age_10k[Age_10k$Age <= 2, 'AgeGroup'] <- '<2'
  Age_10k[Age_10k$Age > 2 & Age_10k$Age <= 5, 'AgeGroup'] <- '3-5'
  Age_10k[Age_10k$Age > 5 & Age_10k$Age <=8, 'AgeGroup'] <- '6-8'
  Age_10k[Age_10k$Age > 8 & Age_10k$Age <=12, 'AgeGroup'] <- '9-12'
  Age_10k[Age_10k$Age > 12 & Age_10k$Age <= 16, 'AgeGroup'] <- '13-16'
  Age_10k[Age_10k$Age > 16 & Age_10k$Age <=18, 'AgeGroup'] <- '16-18'
  Age_10k[Age_10k$Age >= 18, 'AgeGroup'] <- '18+'
  
  
  
}




if(DF_Iter$AgeGroup == "PIT"){
  #Age Grouping
  Age[Age$Age < 18, 'AgeGroup'] <- '<18'
  Age[Age$Age > 18 & Age$Age <= 24, 'AgeGroup'] <- '18-24'
  Age[Age$Age > 24 & Age$Age <= 34, 'AgeGroup'] <- '25-34'
  Age[Age$Age > 34 & Age$Age <=44, 'AgeGroup'] <- '35-44'
  Age[Age$Age > 44 & Age$Age <= 54, 'AgeGroup'] <- '45-54'
  Age[Age$Age > 54 & Age$Age <=61, 'AgeGroup'] <- '55-61'
  Age[Age$Age >= 62, 'AgeGroup'] <- '62+'
  
  
  Age_10k[Age_10k$Age < 18, 'AgeGroup'] <- '<18'
  Age_10k[Age_10k$Age > 18 & Age_10k$Age <= 24, 'AgeGroup'] <- '18-24'
  Age_10k[Age_10k$Age > 24 & Age_10k$Age <= 34, 'AgeGroup'] <- '25-34'
  Age_10k[Age_10k$Age > 34 & Age_10k$Age <=44, 'AgeGroup'] <- '35-44'
  Age_10k[Age_10k$Age > 44 & Age_10k$Age <= 54, 'AgeGroup'] <- '45-54'
  Age_10k[Age_10k$Age > 54 & Age_10k$Age <=61, 'AgeGroup'] <- '55-61'
  Age_10k[Age_10k$Age >= 62, 'AgeGroup'] <- '62+'
  
  
  
  
}






# Step 3: Calculate the counts
count_Age <- Age %>% dplyr::group_by(AgeGroup) %>% dplyr::summarise(Count = n())
count_Age_10k <- Age_10k %>% dplyr::group_by(AgeGroup) %>% dplyr::summarise(Count = n())

# Step 4: Merge the dataframes
merged_df <- merge(count_Age, count_Age_10k, by = "AgeGroup")

# Step 5: Calculate the ratio
merged_df$Ratio <- (merged_df$Count.x / merged_df$Count.y) * 10000



# Step 6: Create the plot
Age_10K_plot <- plot_ly(merged_df, x = ~AgeGroup, y = ~Ratio, type = 'bar', name = 'Visits per 10,000 by Age Group')  %>%
  layout(title = 'Visits per 10,000 of the Same Age Group',
         xaxis = list(title = 'Age Group'),
         yaxis = list(title = 'Visits by Age Group, per 10,000'))






Age_10k_Table <- merged_df

colnames(Age_10k_Table) <- c("Age Group", "Syndrome Count", "All Visits", "Per 10k")




save(Age_10k_Table, file = "Per10kAgeTable.RData")
save(Age_10K_plot, file  = "Per10kAgePlot.RData")