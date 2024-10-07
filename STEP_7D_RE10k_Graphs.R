## 7D R/E per 10k 



library(dplyr)
library(plotly)

load("MainAPI_Results.RData")
load("IterativeData.RData")
load("DF_10k.RData")




#RACE
Main_df$race <- 'Unknown'
#finding if multiple races
Main_df$count_race = stringr::str_count(Main_df$Race_flat, ";")
Main_df[Main_df$count_race>2, 'race'] <- 'Multiple Races'
Main_df[Main_df$Race_flat==';1002-5;', 'race'] <- 'American Indian/Alaska Native'
Main_df[Main_df$Race_flat==';2028-9;', 'race'] <- 'Asian'
Main_df[Main_df$Race_flat==';2054-5;', 'race'] <- 'Black or African American'
Main_df[Main_df$Race_flat==';2076-8;', 'race'] <- 'Native Hawaiian or Pacific Islander'
Main_df[Main_df$Race_flat==';2131-1;', 'race'] <- ' Race - Not Defined'
Main_df[Main_df$Race_flat==';2106-3;', 'race'] <- 'White'

#ethnicity
Main_df$ethnicity <- 'Unknown'
Main_df[grepl('2135-2', Main_df$Ethnicity_flat), 'ethnicity'] <- 'Hispanic'
Main_df[grepl('2186-5', Main_df$Ethnicity_flat), 'ethnicity'] <- 'Not Hispanic'

#race_ethnicity


Main_df[Main_df$ethnicity=='Hispanic' & Main_df$race == 'American Indian/Alaska Native', 'race_ethn'] <- 'AIAN and Hispanic'
Main_df[Main_df$ethnicity=='Hispanic' & Main_df$race == 'Asian', 'race_ethn'] <- 'Asian and Hispanic'
Main_df[Main_df$ethnicity=='Hispanic' & Main_df$race == 'Black or African American', 'race_ethn'] <- 'Black or African American and Hispanic'
Main_df[Main_df$ethnicity=='Hispanic' & Main_df$race == 'Native Hawaiian or Pacific Islander', 'race_ethn'] <- 'Native Hawaiian or Pacific Islander and Hispanic'
Main_df[Main_df$ethnicity=='Hispanic' & Main_df$race == 'Other Race', 'race_ethn'] <- ' Race Not Defined and Hispanic'
Main_df[Main_df$ethnicity=='Hispanic' & Main_df$race == 'White', 'race_ethn'] <- 'White and Hispanic'


Main_df[Main_df$ethnicity=='Not Hispanic' & Main_df$race == 'American Indian/Alaska Native', 'race_ethn'] <- 'AIAN'
Main_df[Main_df$ethnicity=='Not Hispanic' & Main_df$race == 'Asian', 'race_ethn'] <- 'Asian'
Main_df[Main_df$ethnicity=='Not Hispanic' & Main_df$race == 'Black or African American', 'race_ethn'] <- 'Black or African American'
Main_df[Main_df$ethnicity=='Not Hispanic' & Main_df$race == 'Native Hawaiian or Pacific Islander', 'race_ethn'] <- 'Native Hawaiian or Pacific Islander'
Main_df[Main_df$ethnicity=='Not Hispanic' & Main_df$race == 'Other Race', 'race_ethn'] <- ' Race Not Defined'
Main_df[Main_df$ethnicity=='Not Hispanic' & Main_df$race == 'White', 'race_ethn'] <- 'White'




#RACE
Alldf$race <- 'Unknown'
#finding if multiple races
Alldf$count_race = stringr::str_count(Alldf$Race_flat, ";")
Alldf[Alldf$count_race>2, 'race'] <- 'Multiple Races'
Alldf[Alldf$Race_flat==';1002-5;', 'race'] <- 'American Indian/Alaska Native'
Alldf[Alldf$Race_flat==';2028-9;', 'race'] <- 'Asian'
Alldf[Alldf$Race_flat==';2054-5;', 'race'] <- 'Black or African American'
Alldf[Alldf$Race_flat==';2076-8;', 'race'] <- 'Native Hawaiian or Pacific Islander'
Alldf[Alldf$Race_flat==';2131-1;', 'race'] <- ' Race - Not Defined'
Alldf[Alldf$Race_flat==';2106-3;', 'race'] <- 'White'

#ethnicity
Alldf$ethnicity <- 'Unknown'
Alldf[grepl('2135-2', Alldf$Ethnicity_flat), 'ethnicity'] <- 'Hispanic'
Alldf[grepl('2186-5', Alldf$Ethnicity_flat), 'ethnicity'] <- 'Not Hispanic'

#race_ethnicity


Alldf[Alldf$ethnicity=='Hispanic' & Alldf$race == 'American Indian/Alaska Native', 'race_ethn'] <- 'AIAN and Hispanic'
Alldf[Alldf$ethnicity=='Hispanic' & Alldf$race == 'Asian', 'race_ethn'] <- 'Asian and Hispanic'
Alldf[Alldf$ethnicity=='Hispanic' & Alldf$race == 'Black or African American', 'race_ethn'] <- 'Black or African American and Hispanic'
Alldf[Alldf$ethnicity=='Hispanic' & Alldf$race == 'Native Hawaiian or Pacific Islander', 'race_ethn'] <- 'Native Hawaiian or Pacific Islander and Hispanic'
Alldf[Alldf$ethnicity=='Hispanic' & Alldf$race == 'Other Race', 'race_ethn'] <- ' Race Not Defined and Hispanic'
Alldf[Alldf$ethnicity=='Hispanic' & Alldf$race == 'White', 'race_ethn'] <- 'White and Hispanic'

Alldf[Alldf$ethnicity=='Not Hispanic' & Alldf$race == 'American Indian/Alaska Native', 'race_ethn'] <- 'AIAN'
Alldf[Alldf$ethnicity=='Not Hispanic' & Alldf$race == 'Asian', 'race_ethn'] <- 'Asian'
Alldf[Alldf$ethnicity=='Not Hispanic' & Alldf$race == 'Black or African American', 'race_ethn'] <- 'Black or African American'
Alldf[Alldf$ethnicity=='Not Hispanic' & Alldf$race == 'Native Hawaiian or Pacific Islander', 'race_ethn'] <- 'Native Hawaiian or Pacific Islander'
Alldf[Alldf$ethnicity=='Not Hispanic' & Alldf$race == 'Other Race', 'race_ethn'] <- ' Race Not Defined'
Alldf[Alldf$ethnicity=='Not Hispanic' & Alldf$race == 'White', 'race_ethn'] <- 'White'






# Step 3: Calculate the counts
count_RE <- Main_df %>% dplyr::group_by(race_ethn) %>% dplyr::summarise(Count = n())
count_RE_10k <- Alldf %>% dplyr::group_by(race_ethn) %>% dplyr::summarise(Count = n())


# Step 4: Merge the dataframes
merged_df <- merge(count_RE, count_RE_10k, by = "race_ethn")

# Step 5: Calculate the ratio
merged_df$Ratio <- (merged_df$Count.x / merged_df$Count.y) * 10000





# Step 6: Create the plot
RE_10K_plot <- plot_ly(merged_df, x = ~race_ethn, y = ~Ratio, type = 'bar', name = 'Visits per 10,000 by Race/Ethnicity')  %>%
  layout(title = 'Visits per 10,000 of the Same Race/Ethnicity',
         xaxis = list(title = 'Race/Ethnicity'),
         yaxis = list(title = 'Visits by Race/Ethnicity, per 10,000'))






RE_10k_Table <- merged_df

colnames(RE_10k_Table) <- c("Race/Ethnicity", "Syndrome Count", "All Visits", "Per 10k")




save(RE_10k_Table, file = "Per10kRETable.RData")
save(RE_10K_plot, file  = "Per10kREPlot.RData")