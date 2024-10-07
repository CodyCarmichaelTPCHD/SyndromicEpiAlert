## Race Ethnicity Graphs
library(dplyr)
library(plotly)

load("MainAPI_Results.RData")
load("IterativeData.RData")



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








Main_dfNH <- filter(Main_df, ethnicity == "Not Hispanic")
Main_dfH <- filter(Main_df, ethnicity == "Hispanic")


RNH_table <- Main_dfNH %>%
  select(race,SyndromeName) %>%
  group_by(race,SyndromeName) %>%
  dplyr::summarize(total=n())

RH_table  <- Main_dfH %>% 
  select(race_ethn,SyndromeName) %>%
  group_by(race_ethn,SyndromeName) %>%
  dplyr::summarize(total=n())





RNH_Graph <- plot_ly(x = RNH_table$race, y = RNH_table$total) %>%
  layout(title = paste("Race for ", RNH_table$SyndromeName[1], "for", DF_Iter$StartDate, "-", DF_Iter$EndDate, ", Not Hispanic"), yaxis = list(title = "Frequency of Visits"), xaxis = list(title = "Race"))

RH_Graph <- plot_ly(x = RH_table$race_ethn, y = RH_table$total) %>%
  layout(title = paste("Race for ", RH_table$SyndromeName[1], "for", DF_Iter$StartDate, "-", DF_Iter$EndDate, ", Hispanic"), yaxis = list(title = "Frequency of Visits"), xaxis = list(title = "Race"))




save(RNH_table, RH_table, file = "RE_Data.RData")
save(RNH_Graph, RH_Graph, file = "RE_Graphs.RData")

