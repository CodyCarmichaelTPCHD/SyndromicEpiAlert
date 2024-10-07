# Step 7C Sex Only 


library(dplyr)
library(plotly)

load("MainAPI_Results.RData")
load("IterativeData.RData")
load("DF_10k.RData")


Sex <- select(Main_df, Sex, SyndromeName)
Sex_10k <- select(Alldf, Sex)



Sex[!(Sex$Sex %in% c('F', 'M')), 'Sex'] <- 'Unknown'
Sex[Sex$Sex=='F', 'Sex'] <- 'Female'
Sex[Sex$Sex=='M', 'Sex'] <- 'Male'


Sex_10k[!(Sex_10k$Sex %in% c('F', 'M')), 'Sex'] <- 'Unknown'
Sex_10k[Sex_10k$Sex=='F', 'Sex'] <- 'Female'
Sex_10k[Sex_10k$Sex=='M', 'Sex'] <- 'Male'



# Step 3: Calculate the counts
count_Sex <- Sex %>% dplyr::group_by(Sex) %>% dplyr::summarise(Count = n())
count_Sex_10k <- Sex_10k %>% dplyr::group_by(Sex) %>% dplyr::summarise(Count = n())


# Step 4: Merge the dataframes
merged_df <- merge(count_Sex, count_Sex_10k, by = "Sex")

# Step 5: Calculate the ratio
merged_df$Ratio <- (merged_df$Count.x / merged_df$Count.y) * 10000





# Step 6: Create the plot
Sex_10K_plot <- plot_ly(merged_df, x = ~Sex, y = ~Ratio, type = 'bar', name = 'Visits per 10,000 by Age Group')  %>%
  layout(title = 'Visits per 10,000 of the Same Age Group',
         xaxis = list(title = 'Sex'),
         yaxis = list(title = 'Visits by Sex, per 10,000'))






Sex_10k_Table <- merged_df

colnames(Sex_10k_Table) <- c("Sex", "Syndrome Count", "All Visits", "Per 10k")




save(Sex_10k_Table, file = "Per10kSexTable.RData")
save(Sex_10K_plot, file  = "Per10kSexPlot.RData")