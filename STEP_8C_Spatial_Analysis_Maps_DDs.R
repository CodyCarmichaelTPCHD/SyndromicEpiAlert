

library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(sf)
library(leaflet)



load("FormattedMapBase.RData")
load("MainAPI_Results.RData")
load("IterativeData.RData")

DD_Preprocessed <- select(Main_df, ZipCode, DischargeDiagnosis)

## Map top 5 DDs per ZipCode, with underlying count maybe being the number of unique DDs? 


processed_data <- DD_Preprocessed %>%
  dplyr::group_by(ZipCode) %>%
  dplyr::summarize(DischargeDiagnosis = paste(DischargeDiagnosis, collapse = ";")) %>%
  dplyr::mutate(DischargeDiagnosis = toupper(gsub("\\.", "", DischargeDiagnosis))) %>%
  separate_rows(DischargeDiagnosis, sep = ";") %>%
  filter(DischargeDiagnosis != "") %>%
  dplyr::group_by(ZipCode, DischargeDiagnosis) %>%
  dplyr::summarize(Count = n()) %>%
  ungroup()

# Step 3: Count unique DDs and find top 5 per Zip Code
top_diagnoses <- processed_data %>%
  dplyr::group_by(ZipCode) %>%
  dplyr::mutate(TotalUniqueDDs = n_distinct(DischargeDiagnosis)) %>%
  arrange(desc(Count)) %>%
  slice_max(order_by = Count, n = 5) %>%
  ungroup()



top_diagnoses_transposed <- top_diagnoses %>%
  dplyr::group_by(ZipCode) %>%
  dplyr::summarize(Top5DDs = paste(head(DischargeDiagnosis, 5), collapse = ", "),
            TotalUniqueDDs = first(TotalUniqueDDs)) %>%
  ungroup()




top_diagnoses_transposed$ZipCode <- as.numeric(top_diagnoses_transposed$ZipCode)




colnames(top_diagnoses_transposed) <- c("ZIP_CODE", "Top 5 DDs", "TotalUniqueDDs")




shapeData@data <- left_join(shapeData@data, top_diagnoses_transposed, by= "ZIP_CODE")



popup <- paste0( "<br><strong> Number of Unique Diagnosis Codes for ", DF_Iter$Syndromes[1],": </strong>",
                 shapeData$TotalUniqueDDs ,
                 "<br><strong> Top 5 Diagnosis Codes: </strong>",
                 shapeData$`Top 5 DDs` ,
                 "<br><strong> ZIP Code: </strong>",
                 shapeData$ZIP_CODE ,
                 "<br><strong> Nearest City: </strong>",
                 shapeData$NAME )


# Visits / Cost Pals
Vis_pal<- colorBin("viridis", domain = shapeData$TotalUniqueDDs ,  na.color = "#808080", bins = 9)





LeafletMapSet <- leaflet( width = "100%")  %>%
  addTiles() %>% 
  setView(lng = -122.2726, lat = 47.0777, zoom = 7)







LeafletMapSet_DD <- LeafletMapSet  %>%
  addPolygons(data = shapeData, fillColor = ~Vis_pal(shapeData$TotalUniqueDDs),
              fillOpacity = 0.8, 
              weight = 1, popup = popup , group = "Discharge Diagnoses" ) %>%
  addLegend( group = "Discharge Diagnoses",
             position = "topright",
             pal = Vis_pal , 
             values =  shapeData$total, 
             title = paste("Unique Discharge Diangoses for <br>",DF_Iter$Syndromes[1], " <br> by ZipCode.") ) %>%
  addLayersControl(
    overlayGroups = c("Discharge Diagnoses"), options = layersControlOptions(collapsed = FALSE)) %>%
  clearControls()







DDMapData <- shapeData@data

save(DDMapData, file = "DDMapData.RData")

save(LeafletMapSet_DD, file = "DDMap.RData")


