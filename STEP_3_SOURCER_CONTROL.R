

library(rmarkdown)


source("STEP_2_ExpandUserData.R")



### REQUEST SEPERATOR
load("InputFile.RData")




## For length, iterate down and source files

for (row in 1:nrow(final_df)) {

  DF_Iter <- final_df[row,]
  
  save(DF_Iter, file = "IterativeData.RData")
  
  filepath <- paste(DF_Iter$StartDate[1], "-", DF_Iter$EndDate[1], "_for_", DF_Iter$Syndromes[1])
  if(DF_Iter$ComboExamination == TRUE){
    filepath <- paste(DF_Iter$StartDate[1], "-", DF_Iter$EndDate[1], "_for_", DF_Iter$PairedSyndromeName[1])
  }
  dir.create(filepath)
  dir.create(paste0(filepath,"/Data"), recursive = TRUE)
  dir.create(paste0(filepath,"/Report"), recursive = TRUE)
  save(filepath, file = "FilePathName.RData")
  print(2)
  
  if (DF_Iter$Syndromes[1]!= "ComboExamination") {
    # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
    source("Step_4_API_CALL_MAIN.R")
  } else {
    # Source and run the script 'Step_4_0_API_CALL_COMBO.R' if Syndromes is 'ComboExamination'
    source("Step_4_0_API_CALL_CUSTOM.R")
  }
  print(3)
  
  load("MainAPI_Results.RData")
  print(4)

  if (nrow(Main_df) > 1) {
  ## Now to iterate down each of the settings. 
    
    if (DF_Iter$AgeGenderGraphs[1] == TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_4A_Age_Gender_Graph.R")
    }
    
    print(5)
    if (DF_Iter$AgeOnlyGraphs[1] == TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_4B_Age_Only_Graph.R")
    }
    print(6)
    
    if (DF_Iter$GenderOnlyGraphs[1] == TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_4C_Sex_Only_Graph.R")
    }
    print(7)
    if (DF_Iter$RaceEthnicityGraphs[1] == TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_4D_RE_Graphs.R")
    }
    print(8)
    
    if (DF_Iter$HospitalGroupings[1] == TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_4E_Hosp_Graph.R")
    }
    
    print(9)
    
    if (DF_Iter$TimeOfDayAnalysis[1] == TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_4F_Heatmap_Graph.R")
    }
    print(10)
    if (DF_Iter$TimeSeriesCountGraphs[1] == TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_4G_TS_Count_Graph.R")
    }
  print(11)
    
    if (DF_Iter$NLPAnalysis[1] == TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_4H_TS_NLP_Analysis.R")
    }
    print(12)
    
    
    ## Need to do 5 and 5A together
    if (DF_Iter$ClimateData[1] == "Yes") {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_5_API_CALL_CLIMATE.R")
      source("Step_5A_Weather_Graph.R")
      
    }
    print(13)
    
    ## Need to do 6 and 6A together
    if (DF_Iter$PM25Measures[1] == "Yes") {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_6_API_CALL_PM25.R")
      source("Step_6A_PM25Graphing.R")
      
    }
    print(14)
    ## 7 needs to be true + Above trues
    
    
    
    if (DF_Iter$TimeSeriesPer10kVisits[1]==TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_7_API_CALL_10K.R")
    }
    print("15a")
    
    
    if (DF_Iter$AgeGenderGraphs[1] == TRUE & DF_Iter$TimeSeriesPer10kVisits[1]==TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_7A_AgeGender10k_Graphs.R")
    }
    print("15b")
    
    if (DF_Iter$AgeOnlyGraphs[1] == TRUE & DF_Iter$TimeSeriesPer10kVisits[1]==TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_7B_Age10k_Graphs.R")
    }
    print(16)
    
    if (DF_Iter$GenderOnlyGraphs[1] == TRUE & DF_Iter$TimeSeriesPer10kVisits[1]==TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_7C_Sex10k_Graphs.R")
    }
    print(17)
    if (DF_Iter$RaceEthnicityGraphs[1] == TRUE & DF_Iter$TimeSeriesPer10kVisits[1]==TRUE) {
      # Source and run the script 'Step_4_API_CALL_MAIN.R' if Syndromes is not 'ComboExamination'
      source("Step_7D_RE10k_Graphs.R")
    }
    print(18)
    
    if (DF_Iter$Maps[1] == TRUE){
      source("Step_8_SpatialDataReadin.R")
    }
    
    ## 8B - 8D
    if (DF_Iter$Maps[1] == TRUE){
      source("Step_8B_Spatial_Analysis_Maps_Dates.R")
      source("Step_8C_Spatial_Analysis_Maps_DDs.R")
      source("Step_8D_Spatial_Analysis_Maps_Demographics.R")
    }
    print(19)
    
    ## 8A +E if 10k is also true for spatial analysis
    
    if (DF_Iter$SpatialAnalysis[1] == TRUE & DF_Iter$TimeSeriesPer10kVisits[1]==TRUE) {
      source("Step_8A_Count_per10k_Maps.R")
      source("Step_8E_Spatial_Analysis_Maps_Demographics10k.R")
    }
    print(20)
 
    
    
    
    
    
    
    rmarkdown::render(
      input = "Step_9A_Actual_Report.Rmd", 
      output_file = paste0( filepath, "/Report/" , "Report.html"))
    
    if (DF_Iter$NLPAnalysis[1] == TRUE){
    rmarkdown::render(
      input = "Step_9A_2_Actual_Report_NLP.Rmd", 
      output_file = paste0( filepath, "/Report/" , "NLPReport.html"))
    }
    
    
    
    if(DF_Iter$Maps[1] == TRUE){
      rmarkdown::render(
        input = "Step_9A_2_Actual_Report_CountMaps.Rmd", 
        output_file = paste0( filepath, "/Report/" , "CountBasedMapsReport.html"))
    }
    
    
    
    
    if(DF_Iter$Maps[1] == TRUE){
      rmarkdown::render(
        input = "Step_9A_3_Actual_Report_CountMaps_Demographics.Rmd", 
        output_file = paste0( filepath, "/Report/" , "CountBasedDemographicsMapsReport.html"))
    }
    
    if (DF_Iter$SpatialAnalysis[1] == TRUE & DF_Iter$TimeSeriesPer10kVisits[1]==TRUE){
      rmarkdown::render(
        input = "Step_9A_4_Actual_Report_p10kMaps.Rmd", 
        output_file = paste0( filepath, "/Report/" , "Per10kMapsReport.html"))
    }
    
    if (DF_Iter$SpatialAnalysis[1] == TRUE & DF_Iter$TimeSeriesPer10kVisits[1]==TRUE){
      rmarkdown::render(
        input = "Step_9A_4_Actual_Report_p10kMaps_Age_Gender.Rmd", 
        output_file = paste0( filepath, "/Report/" , "Per10kAgeGenderMapsReport.html"))
    }
    
    if (DF_Iter$SpatialAnalysis[1] == TRUE & DF_Iter$TimeSeriesPer10kVisits[1]==TRUE){
      rmarkdown::render(
        input = "Step_9A_4_Actual_Report_p10kMaps_Race_Ethn.Rmd", 
        output_file = paste0( filepath, "/Report/" , "Per10kRaceEthnicityMapsReport.html"))
    }
    
    
    print(21)
    
    
    
    ## Generate Datafiles
    
    source("Step_9B_Data_Provider.R")
    
    print(22)
     
 
    
  } else {
    # Code to execute if the number of rows is not greater than 1
    # Producing a string as an example
    rmarkdown::render(
      input = "Step_9_Null_Report.Rmd", 
      output_file = paste0( filepath, "/Report/" , "Report_Not_Created.html"))
    
    
    
    
  }
  
  
  # List all .RData files
  all_rdata_files <- list.files(pattern = "\\.RData$")
  
  # Specify the file you want to keep
  file_to_keep <- "InputFile.RData"
  
  # Exclude the file you want to keep from the list
  files_to_delete <- all_rdata_files[all_rdata_files != file_to_keep]
  
  # Delete the files
  file.remove(files_to_delete)
  
  
}


file.remove(file_to_keep)
