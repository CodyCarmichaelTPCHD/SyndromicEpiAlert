## Step 8A, base count layer, base 10k layer. 
library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(sf)
library(leaflet)



load("FormattedMapBase.RData")
load("MainAPI_Results.RData")
load("IterativeData.RData")


Zips_DF <- select(Main_df, ZipCode) %>%
  group_by(ZipCode) %>% 
  dplyr::summarize(total=n())

Zips_DF$ZipCode <- as.numeric(Zips_DF$ZipCode)
colnames(Zips_DF) <- c("ZIP_CODE", "total")





shapeData@data <- left_join(shapeData@data, Zips_DF, by= "ZIP_CODE")



popup <- paste0( "<br><strong>", DF_Iter$Syndromes[1]," Visit Frequency: </strong>",
                 shapeData$total ,
                 "<br><strong> ZIP Code: </strong>",
                 shapeData$ZIP_CODE ,
                 "<br><strong> Nearest City: </strong>",
                 shapeData$NAME )



# Visits / Cost Pals
Vis_pal<- colorBin("viridis", domain = shapeData$total,  na.color = "#808080", bins = 9)




LeafletMapSet <- leaflet( width = "100%")  %>%
  addTiles() %>% 
  setView(lng = -122.2726, lat = 47.0777, zoom = 7)







LeafletMapSet_vis <- LeafletMapSet  %>%
  addPolygons(data = shapeData, fillColor = ~Vis_pal(shapeData$total),
              fillOpacity = 0.8, 
              weight = 1, popup = popup , group = "Visits" ) %>%
  addLegend( group = "Visits",
             position = "topright",
             pal = Vis_pal , 
             values =  shapeData$total, 
             title = paste("Number of ED Visits for <br>",DF_Iter$Syndromes[1], " <br> by ZipCode") ) %>%
  addLayersControl(
    overlayGroups = c("Visits"), options = layersControlOptions(collapsed = FALSE)) %>%
  clearControls()







CountMapData <- shapeData@data

save(CountMapData, file = "CountMapData.RData")

save(LeafletMapSet_vis, file = "CountMap.RData")






if(DF_Iter$TimeSeriesPer10kVisits == TRUE){
  ## Add in data, format it, add polygon layers and such? 
  load("DF_10k.RData")
  Alldf <- select(Alldf, ZipCode) %>%
    group_by(ZipCode) %>% 
    dplyr::summarize(total=n())
  
  
  
  Alldf$ZipCode <- as.numeric(Alldf$ZipCode)
  colnames(Alldf) <- c("ZIP_CODE", "total10k")
  
  
  
  
  
  shapeData@data <- left_join(shapeData@data, Alldf, by= "ZIP_CODE")
  
  
  ## Create 10k measure
  
  
  shapeData@data$p10k <- round((shapeData@data$total / shapeData@data$total10k)*10000, 2)
  
  
  popup10k <- paste0( "<br><strong>", DF_Iter$Syndromes[1]," per 10k visits: </strong>",
                   shapeData$p10k ,
                   "<br><strong> ZIP Code: </strong>",
                   shapeData$ZIP_CODE ,
                   "<br><strong> Nearest City: </strong>",
                   shapeData$NAME )
  
  
  
  # Visits / Cost Pals
  Vis_pal10k<- colorBin("viridis", domain = shapeData$p10k,  na.color = "#808080", bins = 12)
  
  
  
LeafletMapSet_vis_10k <- LeafletMapSet  %>%
  addPolygons(data = shapeData, fillColor = ~Vis_pal10k(shapeData$p10k),
              fillOpacity = 0.8, 
              weight = 1, popup = popup10k , group = "Per 10k" ) %>%
  addLegend( group = "Per 10k",
             position = "topright",
             pal = Vis_pal10k, 
             values =  shapeData$p10k, 
             title = paste("ED Visits per 10k Visits for <br>",DF_Iter$Syndromes[1], " <br> by ZipCode") ) %>%
  addLayersControl(
    overlayGroups = c("Per 10k"), options = layersControlOptions(collapsed = FALSE)) %>%
  clearControls()  
  ## Save Data
Per10kMapData <- shapeData@data

save(Per10kMapData, file = "Per10kMapData.RData")

save(LeafletMapSet_vis_10k, file = "Per10kMap.RData")


}






