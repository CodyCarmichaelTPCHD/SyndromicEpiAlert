

library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(sf)
library(leaflet)



load("FormattedMapBase.RData")
load("MainAPI_Results.RData")
load("IterativeData.RData")


## Highest count date map


Zip_Dates_DF <- Main_df %>%
  select(Date, ZipCode) %>%
  dplyr::group_by(ZipCode, Date) %>% 
  dplyr::summarize(total = n()) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  arrange(ZipCode, desc(total)) %>%  # Arrange by ZipCode and descending total
  dplyr::group_by(ZipCode) %>%
  slice(1)  # Keep only the first row for each ZipCode


Zip_Dates_DF$ZipCode <- as.numeric(Zip_Dates_DF$ZipCode)


colnames(Zip_Dates_DF) <- c("ZIP_CODE","Date", "total")






shapeData@data <- left_join(shapeData@data, Zip_Dates_DF, by= "ZIP_CODE")



popup <- paste0( "<br><strong>", DF_Iter$Syndromes[1]," Visit Frequency: </strong>",
                 shapeData$total ,
                 "<br><strong> Date of Highest Visit Count: </strong>",
                 shapeData$Date ,
                 "<br><strong> ZIP Code: </strong>",
                 shapeData$ZIP_CODE ,
                 "<br><strong> Nearest City: </strong>",
                 shapeData$NAME )



# Visits / Cost Pals
Vis_pal<- colorBin("viridis", domain = shapeData$total,  na.color = "#808080", bins = 9)




LeafletMapSet <- leaflet( width = "100%")  %>%
  addTiles() %>% 
  setView(lng = -122.2726, lat = 47.0777, zoom = 7)







LeafletMapSet_Date <- LeafletMapSet  %>%
  addPolygons(data = shapeData, fillColor = ~Vis_pal(shapeData$total),
              fillOpacity = 0.8, 
              weight = 1, popup = popup , group = "Visits" ) %>%
  addLegend( group = "Visits",
             position = "topright",
             pal = Vis_pal , 
             values =  shapeData$total, 
             title = paste("Number of ED Visits for <br>",DF_Iter$Syndromes[1], " <br> by ZipCode, for date of most visits") ) %>%
  addLayersControl(
    overlayGroups = c("Visits"), options = layersControlOptions(collapsed = FALSE)) %>%
  clearControls()







DateMapData <- shapeData@data

save(DateMapData, file = "DateMapData.RData")

save(LeafletMapSet_Date, file = "DateMap.RData")


