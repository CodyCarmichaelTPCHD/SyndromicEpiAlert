
## Demographics per 10k maps



library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(sf)
library(leaflet)



load("FormattedMapBase.RData")
load("MainAPI_Results.RData")
load("IterativeData.RData")
load("DF_10k.RData")










Demographics_DF <- select(Main_df, ZipCode, Sex, Age, Ethnicity_flat, Race_flat)

Demo_DF10k <- select(Alldf, ZipCode, Sex, Age, Ethnicity_flat, Race_flat)


### Make fields formatted correctly


########### Age Groups
Demographics_DF$Age <- as.numeric(Demographics_DF$Age)
Demographics_DF$AgeGroup <- NA

if(DF_Iter$AgeGroup == "Standard"){
  #Age Grouping
  Demographics_DF[Demographics_DF$Age <= 18, 'AgeGroup'] <- '<18'
  Demographics_DF[Demographics_DF$Age > 18 & Demographics_DF$Age <= 44, 'AgeGroup'] <- '18-44'
  Demographics_DF[Demographics_DF$Age > 44 & Demographics_DF$Age <=64, 'AgeGroup'] <- '45-64'
  Demographics_DF[Demographics_DF$Age >= 65, 'AgeGroup'] <- '65+'
}

if(DF_Iter$AgeGroup == "Peds"){
  #Age Grouping
  Demographics_DF[Demographics_DF$Age <= 2, 'AgeGroup'] <- '<2'
  Demographics_DF[Demographics_DF$Age > 2 & Demographics_DF$Age <= 5, 'AgeGroup'] <- '3-5'
  Demographics_DF[Demographics_DF$Age > 5 & Demographics_DF$Age <=8, 'AgeGroup'] <- '6-8'
  Demographics_DF[Demographics_DF$Age > 8 & Demographics_DF$Age <=12, 'AgeGroup'] <- '9-12'
  Demographics_DF[Demographics_DF$Age > 12 & Demographics_DF$Age <= 16, 'AgeGroup'] <- '13-16'
  Demographics_DF[Demographics_DF$Age > 16 & Demographics_DF$Age <=18, 'AgeGroup'] <- '16-18'
  Demographics_DF[Demographics_DF$Age >= 18, 'AgeGroup'] <- '18+'
}


if(DF_Iter$AgeGroup == "PIT"){
  #Age Grouping
  Demographics_DF[Demographics_DF$Age < 18, 'AgeGroup'] <- '<18'
  Demographics_DF[Demographics_DF$Age > 18 & Demographics_DF$Age <= 24, 'AgeGroup'] <- '18-24'
  Demographics_DF[Demographics_DF$Age > 24 & Demographics_DF$Age <= 34, 'AgeGroup'] <- '25-34'
  Demographics_DF[Demographics_DF$Age > 34 & Demographics_DF$Age <=44, 'AgeGroup'] <- '35-44'
  Demographics_DF[Demographics_DF$Age > 44 & Demographics_DF$Age <= 54, 'AgeGroup'] <- '45-54'
  Demographics_DF[Demographics_DF$Age > 54 & Demographics_DF$Age <=61, 'AgeGroup'] <- '55-61'
  Demographics_DF[Demographics_DF$Age >= 62, 'AgeGroup'] <- '62+'
}



############# Sex Groups


Demographics_DF[!(Demographics_DF$Sex %in% c('F', 'M')), 'Sex'] <- 'Unknown'
Demographics_DF[Demographics_DF$Sex=='F', 'Sex'] <- 'Female'
Demographics_DF[Demographics_DF$Sex=='M', 'Sex'] <- 'Male'




################ R/E Groups


#RACE
Demographics_DF$race <- 'Unknown'
#finding if multiple races
Demographics_DF$count_race = stringr::str_count(Demographics_DF$Race_flat, ";")
Demographics_DF[Demographics_DF$count_race>2, 'race'] <- 'Multiple Races'
Demographics_DF[Demographics_DF$Race_flat==';1002-5;', 'race'] <- 'American Indian/Alaska Native'
Demographics_DF[Demographics_DF$Race_flat==';2028-9;', 'race'] <- 'Asian'
Demographics_DF[Demographics_DF$Race_flat==';2054-5;', 'race'] <- 'Black or African American'
Demographics_DF[Demographics_DF$Race_flat==';2076-8;', 'race'] <- 'Native Hawaiian or Pacific Islander'
Demographics_DF[Demographics_DF$Race_flat==';2131-1;', 'race'] <- 'Race - Not Defined'
Demographics_DF[Demographics_DF$Race_flat==';2106-3;', 'race'] <- 'White'

#ethnicity
Demographics_DF$ethnicity <- 'Unknown'
Demographics_DF[grepl('2135-2', Demographics_DF$Ethnicity_flat), 'ethnicity'] <- 'Hispanic'
Demographics_DF[grepl('2186-5', Demographics_DF$Ethnicity_flat), 'ethnicity'] <- 'Not Hispanic'

#race_ethnicity

Demographics_DF[Demographics_DF$race == 'Multiple Races', 'race_ethn'] <- 'Multiracial'
Demographics_DF[Demographics_DF$race == 'Unknown', 'race_ethn'] <- 'Unknown'

Demographics_DF[Demographics_DF$ethnicity=='Hispanic' & Demographics_DF$race == 'American Indian/Alaska Native', 'race_ethn'] <- 'AIAN and Hispanic'
Demographics_DF[Demographics_DF$ethnicity=='Hispanic' & Demographics_DF$race == 'Asian', 'race_ethn'] <- 'Asian and Hispanic'
Demographics_DF[Demographics_DF$ethnicity=='Hispanic' & Demographics_DF$race == 'Black or African American', 'race_ethn'] <- 'Black or African American and Hispanic'
Demographics_DF[Demographics_DF$ethnicity=='Hispanic' & Demographics_DF$race == 'Native Hawaiian or Pacific Islander', 'race_ethn'] <- 'Native Hawaiian or Pacific Islander and Hispanic'
Demographics_DF[Demographics_DF$ethnicity=='Hispanic' & Demographics_DF$race == 'Race - Not Defined', 'race_ethn'] <- ' Race Not Defined and Hispanic'
Demographics_DF[Demographics_DF$ethnicity=='Hispanic' & Demographics_DF$race == 'White', 'race_ethn'] <- 'White and Hispanic'


Demographics_DF[Demographics_DF$ethnicity=='Not Hispanic' & Demographics_DF$race == 'American Indian/Alaska Native', 'race_ethn'] <- 'AIAN'
Demographics_DF[Demographics_DF$ethnicity=='Not Hispanic' & Demographics_DF$race == 'Asian', 'race_ethn'] <- 'Asian'
Demographics_DF[Demographics_DF$ethnicity=='Not Hispanic' & Demographics_DF$race == 'Black or African American', 'race_ethn'] <- 'Black or African American'
Demographics_DF[Demographics_DF$ethnicity=='Not Hispanic' & Demographics_DF$race == 'Native Hawaiian or Pacific Islander', 'race_ethn'] <- 'Native Hawaiian or Pacific Islander'
Demographics_DF[Demographics_DF$ethnicity=='Not Hispanic' & Demographics_DF$race == 'Race - Not Defined', 'race_ethn'] <- ' Race Not Defined'
Demographics_DF[Demographics_DF$ethnicity=='Not Hispanic' & Demographics_DF$race == 'White', 'race_ethn'] <- 'White'






Demographics_DF <- select(Demographics_DF, ZipCode, Sex, AgeGroup, race_ethn)

Demographics_DF$ZipCode <- as.numeric(Demographics_DF$ZipCode)

colnames(Demographics_DF) <- c("ZIP_CODE", "Sex", "Age Group", "Race/Ethnicity")






########### Age Groups
Demo_DF10k$Age <- as.numeric(Demo_DF10k$Age)
Demo_DF10k$AgeGroup <- NA

if(DF_Iter$AgeGroup == "Standard"){
  #Age Grouping
  Demo_DF10k[Demo_DF10k$Age <= 18, 'AgeGroup'] <- '<18'
  Demo_DF10k[Demo_DF10k$Age > 18 & Demo_DF10k$Age <= 44, 'AgeGroup'] <- '18-44'
  Demo_DF10k[Demo_DF10k$Age > 44 & Demo_DF10k$Age <=64, 'AgeGroup'] <- '45-64'
  Demo_DF10k[Demo_DF10k$Age >= 65, 'AgeGroup'] <- '65+'
}

if(DF_Iter$AgeGroup == "Peds"){
  #Age Grouping
  Demo_DF10k[Demo_DF10k$Age <= 2, 'AgeGroup'] <- '<2'
  Demo_DF10k[Demo_DF10k$Age > 2 & Demo_DF10k$Age <= 5, 'AgeGroup'] <- '3-5'
  Demo_DF10k[Demo_DF10k$Age > 5 & Demo_DF10k$Age <=8, 'AgeGroup'] <- '6-8'
  Demo_DF10k[Demo_DF10k$Age > 8 & Demo_DF10k$Age <=12, 'AgeGroup'] <- '9-12'
  Demo_DF10k[Demo_DF10k$Age > 12 & Demo_DF10k$Age <= 16, 'AgeGroup'] <- '13-16'
  Demo_DF10k[Demo_DF10k$Age > 16 & Demo_DF10k$Age <=18, 'AgeGroup'] <- '16-18'
  Demo_DF10k[Demo_DF10k$Age >= 18, 'AgeGroup'] <- '18+'
}


if(DF_Iter$AgeGroup == "PIT"){
  #Age Grouping
  Demo_DF10k[Demo_DF10k$Age < 18, 'AgeGroup'] <- '<18'
  Demo_DF10k[Demo_DF10k$Age > 18 & Demo_DF10k$Age <= 24, 'AgeGroup'] <- '18-24'
  Demo_DF10k[Demo_DF10k$Age > 24 & Demo_DF10k$Age <= 34, 'AgeGroup'] <- '25-34'
  Demo_DF10k[Demo_DF10k$Age > 34 & Demo_DF10k$Age <=44, 'AgeGroup'] <- '35-44'
  Demo_DF10k[Demo_DF10k$Age > 44 & Demo_DF10k$Age <= 54, 'AgeGroup'] <- '45-54'
  Demo_DF10k[Demo_DF10k$Age > 54 & Demo_DF10k$Age <=61, 'AgeGroup'] <- '55-61'
  Demo_DF10k[Demo_DF10k$Age >= 62, 'AgeGroup'] <- '62+'
}



############# Sex Groups


Demo_DF10k[!(Demo_DF10k$Sex %in% c('F', 'M')), 'Sex'] <- 'Unknown'
Demo_DF10k[Demo_DF10k$Sex=='F', 'Sex'] <- 'Female'
Demo_DF10k[Demo_DF10k$Sex=='M', 'Sex'] <- 'Male'




################ R/E Groups


#RACE
Demo_DF10k$race <- 'Unknown'
#finding if multiple races
Demo_DF10k$count_race = stringr::str_count(Demo_DF10k$Race_flat, ";")
Demo_DF10k[Demo_DF10k$count_race>2, 'race'] <- 'Multiple Races'
Demo_DF10k[Demo_DF10k$Race_flat==';1002-5;', 'race'] <- 'American Indian/Alaska Native'
Demo_DF10k[Demo_DF10k$Race_flat==';2028-9;', 'race'] <- 'Asian'
Demo_DF10k[Demo_DF10k$Race_flat==';2054-5;', 'race'] <- 'Black or African American'
Demo_DF10k[Demo_DF10k$Race_flat==';2076-8;', 'race'] <- 'Native Hawaiian or Pacific Islander'
Demo_DF10k[Demo_DF10k$Race_flat==';2131-1;', 'race'] <- 'Race - Not Defined'
Demo_DF10k[Demo_DF10k$Race_flat==';2106-3;', 'race'] <- 'White'

#ethnicity
Demo_DF10k$ethnicity <- 'Unknown'
Demo_DF10k[grepl('2135-2', Demo_DF10k$Ethnicity_flat), 'ethnicity'] <- 'Hispanic'
Demo_DF10k[grepl('2186-5', Demo_DF10k$Ethnicity_flat), 'ethnicity'] <- 'Not Hispanic'

#race_ethnicity

Demo_DF10k[Demo_DF10k$race == 'Multiple Races', 'race_ethn'] <- 'Multiracial'
Demo_DF10k[Demo_DF10k$race == 'Unknown', 'race_ethn'] <- 'Unknown'

Demo_DF10k[Demo_DF10k$ethnicity=='Hispanic' & Demo_DF10k$race == 'American Indian/Alaska Native', 'race_ethn'] <- 'AIAN and Hispanic'
Demo_DF10k[Demo_DF10k$ethnicity=='Hispanic' & Demo_DF10k$race == 'Asian', 'race_ethn'] <- 'Asian and Hispanic'
Demo_DF10k[Demo_DF10k$ethnicity=='Hispanic' & Demo_DF10k$race == 'Black or African American', 'race_ethn'] <- 'Black or African American and Hispanic'
Demo_DF10k[Demo_DF10k$ethnicity=='Hispanic' & Demo_DF10k$race == 'Native Hawaiian or Pacific Islander', 'race_ethn'] <- 'Native Hawaiian or Pacific Islander and Hispanic'
Demo_DF10k[Demo_DF10k$ethnicity=='Hispanic' & Demo_DF10k$race == 'Race - Not Defined', 'race_ethn'] <- ' Race Not Defined and Hispanic'
Demo_DF10k[Demo_DF10k$ethnicity=='Hispanic' & Demo_DF10k$race == 'White', 'race_ethn'] <- 'White and Hispanic'


Demo_DF10k[Demo_DF10k$ethnicity=='Not Hispanic' & Demo_DF10k$race == 'American Indian/Alaska Native', 'race_ethn'] <- 'AIAN'
Demo_DF10k[Demo_DF10k$ethnicity=='Not Hispanic' & Demo_DF10k$race == 'Asian', 'race_ethn'] <- 'Asian'
Demo_DF10k[Demo_DF10k$ethnicity=='Not Hispanic' & Demo_DF10k$race == 'Black or African American', 'race_ethn'] <- 'Black or African American'
Demo_DF10k[Demo_DF10k$ethnicity=='Not Hispanic' & Demo_DF10k$race == 'Native Hawaiian or Pacific Islander', 'race_ethn'] <- 'Native Hawaiian or Pacific Islander'
Demo_DF10k[Demo_DF10k$ethnicity=='Not Hispanic' & Demo_DF10k$race == 'Race - Not Defined', 'race_ethn'] <- ' Race Not Defined'
Demo_DF10k[Demo_DF10k$ethnicity=='Not Hispanic' & Demo_DF10k$race == 'White', 'race_ethn'] <- 'White'






Demo_DF10k <- select(Demo_DF10k, ZipCode, Sex, AgeGroup, race_ethn)

Demo_DF10k$ZipCode <- as.numeric(Demo_DF10k$ZipCode)

colnames(Demo_DF10k) <- c("ZIP_CODE", "Sex", "Age Group", "Race/Ethnicity")



## Now to combine the sets. 


# RE
count_RE <- Demographics_DF %>% dplyr::group_by(ZIP_CODE, `Race/Ethnicity`) %>% dplyr::summarise(Count = n())
count_RE_10k <- Demo_DF10k %>% dplyr::group_by(ZIP_CODE, `Race/Ethnicity`) %>% dplyr::summarise(Count = n())
# Step 4: Merge the dataframes
merged_RE <- merge(count_RE, count_RE_10k, by = c("ZIP_CODE","Race/Ethnicity"))
# Step 5: Calculate the ratio
merged_RE$Ratio <- (merged_RE$Count.x / merged_RE$Count.y) * 10000


# Age

count_Age <- Demographics_DF %>% dplyr::group_by(ZIP_CODE, `Age Group`) %>% dplyr::summarise(Count = n())
count_Age_10k <- Demo_DF10k %>% dplyr::group_by(ZIP_CODE, `Age Group`) %>% dplyr::summarise(Count = n())
# Step 4: Merge the dataframes
merged_Age <- merge(count_Age, count_Age_10k, by = c("ZIP_CODE","Age Group"))
# Step 5: Calculate the ratio
merged_Age$Ratio <- (merged_Age$Count.x / merged_Age$Count.y) * 10000





# Sex

count_Sex <- Demographics_DF %>% dplyr::group_by(ZIP_CODE, `Sex`) %>% dplyr::summarise(Count = n())
count_Sex_10k <- Demo_DF10k %>% dplyr::group_by(ZIP_CODE, `Sex`) %>% dplyr::summarise(Count = n())
# Step 4: Merge the dataframes
merged_Sex <- merge(count_Sex, count_Sex_10k, by = c("ZIP_CODE","Sex"))
# Step 5: Calculate the ratio
merged_Sex$Ratio <- (merged_Sex$Count.x / merged_Sex$Count.y) * 10000




# Initialize the leaflet map
Leaflet_Main <- leaflet( width = "100%")  %>%
  addTiles() %>% 
  setView(lng = -122.2726, lat = 47.0777, zoom = 7)

# Loop over each race/ethnicity and add layers
unique_races <- unique(merged_RE$`Race/Ethnicity`)

unique_sexes <- unique(merged_Sex$Sex)

unique_ages <- unique(merged_Age$`Age Group` )



Leaflet_RE <- Leaflet_Main
Leaflet_Sex <- Leaflet_Main
Leaflet_Age <- Leaflet_Main






for(race in unique_races) {
  
  # Filter and merge data for the current group
  int_shape <- shapeData
  int_race <-  merged_RE  %>%
    filter(`Race/Ethnicity` == race)
  
  int_shape@data <- int_shape@data %>%
    left_join(int_race, by = "ZIP_CODE")
  
  
  popup <- paste0(
    "<br><strong> Race/Ethnicity: </strong>",
    int_shape$`Race/Ethnicity` ,
    "<br><strong> Per 10k for visits for ", DF_Iter$Syndromes[1],": </strong>",
    int_shape$Ratio ,
    "<br><strong> ZIP Code: </strong>",
    int_shape$ZIP_CODE ,
    "<br><strong> Nearest City: </strong>",
    int_shape$NAME )
  
  
  # Visits / Cost Pals
  Vis_pal<- colorBin("viridis", domain = int_shape$Ratio ,  na.color = "#808080", bins = 9)
  
  Leaflet_RE  <- Leaflet_RE  %>%
    addPolygons(data = int_shape, fillColor = ~Vis_pal(int_shape$Ratio),
                fillOpacity = 0.8, 
                weight = 1, popup = popup , group = race ) %>%
    addLegend( group = race,
               position = "topright",
               pal = Vis_pal , 
               values =  int_shape$Ratio, 
               title = paste("R/E per10k for <br>",DF_Iter$Syndromes[1], " <br> by ZipCode.") ) %>%
    addLayersControl(
      overlayGroups = unique_races, options = layersControlOptions(collapsed = FALSE)) %>%
    clearControls()
  
  rm(int_shape)
  
  
}









for(Age in unique_ages) {
  
  # Filter and merge data for the current group
  int_shape <- shapeData
  int_age <-  merged_Age  %>%
    filter(`Age Group` == Age)
  
  int_shape@data <- int_shape@data %>%
    left_join(int_age, by = "ZIP_CODE")
  
  
  popup <- paste0(
    "<br><strong> Age Group: </strong>",
    int_shape$`Age Group` ,
    "<br><strong> Per 10k for visits for ", DF_Iter$Syndromes[1],": </strong>",
    int_shape$Ratio ,
    "<br><strong> ZIP Code: </strong>",
    int_shape$ZIP_CODE ,
    "<br><strong> Nearest City: </strong>",
    int_shape$NAME )
  
  
  # Visits / Cost Pals
  Vis_pal<- colorBin("viridis", domain = int_shape$Ratio ,  na.color = "#808080", bins = 9)
  
  Leaflet_Age  <- Leaflet_Age  %>%
    addPolygons(data = int_shape, fillColor = ~Vis_pal(int_shape$Ratio),
                fillOpacity = 0.8, 
                weight = 1, popup = popup , group = Age ) %>%
    addLegend( group = Age,
               position = "topright",
               pal = Vis_pal , 
               values =  int_shape$Ratio, 
               title = paste("Age per10k for <br>",DF_Iter$Syndromes[1], " <br> by ZipCode.") ) %>%
    addLayersControl(
      overlayGroups = unique_ages , options = layersControlOptions(collapsed = FALSE)) %>%
    clearControls()
  
  rm(int_shape)
  
}






for(sex in unique_sexes) {
  
  # Filter and merge data for the current group
  int_shape <- shapeData
  int_sex <-  merged_Sex  %>%
    filter(Sex == sex)
  
  int_shape@data <- int_shape@data %>%
    left_join(int_sex, by = "ZIP_CODE")
  
  
  popup <- paste0(
    "<br><strong> Sex: </strong>",
    int_shape$Sex ,
    "<br><strong> Per 10k for visits for ", DF_Iter$Syndromes[1],": </strong>",
    int_shape$Ratio ,
    "<br><strong> ZIP Code: </strong>",
    int_shape$ZIP_CODE ,
    "<br><strong> Nearest City: </strong>",
    int_shape$NAME )
  
  
  # Visits / Cost Pals
  Vis_pal<- colorBin("viridis", domain = int_shape$Ratio ,  na.color = "#808080", bins = 9)
  
  Leaflet_Sex  <- Leaflet_Sex  %>%
    addPolygons(data = int_shape, fillColor = ~Vis_pal(int_shape$Ratio),
                fillOpacity = 0.8, 
                weight = 1, popup = popup , group = sex ) %>%
    addLegend( group = sex,
               position = "topright",
               pal = Vis_pal , 
               values =  int_shape$Ratio, 
               title = paste("Sex per10k for <br>",DF_Iter$Syndromes[1], " <br> by ZipCode.") ) %>%
    addLayersControl(
      overlayGroups = unique_sexes , options = layersControlOptions(collapsed = FALSE)) %>%
    clearControls()
  
  rm(int_shape)
  
}


Leaflet_Age_10k <- Leaflet_Age
Leaflet_RE_10k <- Leaflet_RE
Leaflet_Sex_10k <- Leaflet_Sex

save(Leaflet_Age_10k, file = "Age10kMap.RData")
save(Leaflet_RE_10k, file = "RE10kMap.RData")
save(Leaflet_Sex_10k, file = "Sex10kMap.RData")

MapDataAge10k <- merged_Age
MapDataRE10k <- merged_RE
MapDataSex10k <- merged_Sex

save(MapDataAge10k, file = "Age10kMapData.RData")
save(MapDataRE10k, file = "RE10kMapData.RData")
save(MapDataSex10k, file = "Sex10kMapData.RData")
