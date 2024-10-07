

library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(sf)
library(leaflet)



load("FormattedMapBase.RData")
load("MainAPI_Results.RData")
load("IterativeData.RData")

Demographics_DF <- select(Main_df, ZipCode, Sex, Age, Ethnicity_flat, Race_flat)


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





# Aggregate Data by Categories
# For Sex
sex_counts <- Demographics_DF %>%
  dplyr::group_by(ZIP_CODE, Sex) %>%
  dplyr::summarize(Count = n()) %>%
  ungroup()





# For Age Group
age_group_counts <- Demographics_DF %>%
  dplyr::group_by(ZIP_CODE, `Age Group`) %>%
  dplyr::summarize(Count = n()) %>%
  ungroup()




# For Race/Ethnicity
race_ethnicity_counts <- Demographics_DF %>%
  dplyr::group_by(ZIP_CODE, `Race/Ethnicity`) %>%
  dplyr::summarize(Count = n()) %>%
  ungroup()





# Assuming race_ethnicity_counts and shapeData are already loaded

# Initialize the leaflet map
Leaflet_Main <- leaflet( width = "100%")  %>%
  addTiles() %>% 
  setView(lng = -122.2726, lat = 47.0777, zoom = 7)

# Loop over each race/ethnicity and add layers
unique_races <- unique(race_ethnicity_counts$`Race/Ethnicity`)

unique_sexes <- unique(sex_counts$Sex)

unique_ages <- unique(age_group_counts$`Age Group` )



Leaflet_RE <- Leaflet_Main
Leaflet_Sex <- Leaflet_Main
Leaflet_Age <- Leaflet_Main


for(race in unique_races) {

  # Filter and merge data for the current group
  int_shape <- shapeData
  int_race <-  race_ethnicity_counts  %>%
    filter(`Race/Ethnicity` == race)

  int_shape@data <- int_shape@data %>%
    left_join(int_race, by = "ZIP_CODE")
  
  
  popup <- paste0(
                   "<br><strong> Race/Ethnicity: </strong>",
                   int_shape$`Race/Ethnicity` ,
                   "<br><strong> Number of visits for ", DF_Iter$Syndromes[1],": </strong>",
                   int_shape$Count ,
                   "<br><strong> ZIP Code: </strong>",
                   int_shape$ZIP_CODE ,
                   "<br><strong> Nearest City: </strong>",
                   int_shape$NAME )
  
  
  # Visits / Cost Pals
  Vis_pal<- colorBin("viridis", domain = int_shape$Count ,  na.color = "#808080", bins = 9)
  
  Leaflet_RE  <- Leaflet_RE  %>%
    addPolygons(data = int_shape, fillColor = ~Vis_pal(int_shape$Count),
                fillOpacity = 0.8, 
                weight = 1, popup = popup , group = race ) %>%
    addLegend( group = race,
               position = "topright",
               pal = Vis_pal , 
               values =  int_shape$Count, 
               title = paste("R/E Count for <br>",DF_Iter$Syndromes[1], " <br> by ZipCode.") ) %>%
    addLayersControl(
      overlayGroups = unique_races, options = layersControlOptions(collapsed = FALSE)) %>%
    clearControls()
  
rm(int_shape)
  

}









for(Age in unique_ages) {
  
  # Filter and merge data for the current group
  int_shape <- shapeData
  int_age <-  age_group_counts  %>%
    filter(`Age Group` == Age)
  
  int_shape@data <- int_shape@data %>%
    left_join(int_age, by = "ZIP_CODE")
  
  
  popup <- paste0(
    "<br><strong> Age Group: </strong>",
    int_shape$`Age Group` ,
    "<br><strong> Number of visits for ", DF_Iter$Syndromes[1],": </strong>",
    int_shape$Count ,
    "<br><strong> ZIP Code: </strong>",
    int_shape$ZIP_CODE ,
    "<br><strong> Nearest City: </strong>",
    int_shape$NAME )
  
  
  # Visits / Cost Pals
  Vis_pal<- colorBin("viridis", domain = int_shape$Count ,  na.color = "#808080", bins = 9)
  
  Leaflet_Age  <- Leaflet_Age  %>%
    addPolygons(data = int_shape, fillColor = ~Vis_pal(int_shape$Count),
                fillOpacity = 0.8, 
                weight = 1, popup = popup , group = Age ) %>%
    addLegend( group = Age,
               position = "topright",
               pal = Vis_pal , 
               values =  int_shape$Count, 
               title = paste("Age Group Count for <br>",DF_Iter$Syndromes[1], " <br> by ZipCode.") ) %>%
    addLayersControl(
      overlayGroups = unique_ages, options = layersControlOptions(collapsed = FALSE)) %>%
    clearControls()
  
  rm(int_shape)
  
  
}






for(sex in unique_sexes) {
  
  # Filter and merge data for the current group
  int_shape <- shapeData
  int_sex <-  sex_counts  %>%
    filter(Sex == sex)
  
  int_shape@data <- int_shape@data %>%
    left_join(int_sex, by = "ZIP_CODE")
  
  
  popup <- paste0(
    "<br><strong> Sex: </strong>",
    int_shape$Sex ,
    "<br><strong> Number of visits for ", DF_Iter$Syndromes[1],": </strong>",
    int_shape$Count ,
    "<br><strong> ZIP Code: </strong>",
    int_shape$ZIP_CODE ,
    "<br><strong> Nearest City: </strong>",
    int_shape$NAME )
  
  
  # Visits / Cost Pals
  Vis_pal<- colorBin("viridis", domain = int_shape$Count ,  na.color = "#808080", bins = 9)
  
  Leaflet_Sex  <- Leaflet_Sex  %>%
    addPolygons(data = int_shape, fillColor = ~Vis_pal(int_shape$Count),
                fillOpacity = 0.8, 
                weight = 1, popup = popup , group = sex ) %>%
    addLegend( group = sex,
               position = "topright",
               pal = Vis_pal , 
               values =  int_shape$Count, 
               title = paste("Sex Count for <br>",DF_Iter$Syndromes[1], " <br> by ZipCode.") ) %>%
    addLayersControl(
      overlayGroups = unique_sexes, options = layersControlOptions(collapsed = FALSE)) %>%
    clearControls()
  
  rm(int_shape)
  
  
}




save(Leaflet_Age, file = "AgeMap.RData")
save(age_group_counts, file = "AgeMapData.RData")

save(Leaflet_RE , file = "REMap.RData")
save(race_ethnicity_counts, file = "REMapData.RData")


save(Leaflet_Sex, file = "SexMap.RData")
save(sex_counts, file = "SexMapData.RData")







