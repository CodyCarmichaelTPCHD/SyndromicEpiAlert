# PER 10K API
#required libraries
library(httr)
library(jsonlite)
library(dplyr)

load("IterativeData.RData")



USER <- DF_Iter$Username
PASS <- DF_Iter$Password

Start_Date <- format(DF_Iter$StartDate, "%d%b%Y") 
End_Date <- format(DF_Iter$EndDate, "%d%b%Y") 

## Pull in 10k data 

fields_for_syns <-"&field=PID&field=Date&field=Time&field=HospitalName&field=ZipCode&field=Sex&field=Age&field=Ethnicity_flat&field=Race_flat"



All_URL <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?datasource=va_er&startDate=",Start_Date,"&medicalGroupingSystem=essencesyndromes&userId=5629&endDate=",End_Date,"&percentParam=noPercent&aqtTarget=DataDetails&geographySystem=region&detector=probrepswitch&timeResolution=daily&refValues=true",fields_for_syns)





Alldf <- lapply(All_URL, function(u){
  api_response <- GET(u, authenticate(user = USER, password = PASS))
  api_response_json <- httr::content(api_response, as = "text")
  api_data <- fromJSON(api_response_json, simplifyDataFrame = TRUE)
  api_data$dataDetails
})


Alldf <- Reduce(rbind, Alldf)


Alldf <- Alldf[!duplicated(Alldf[c('PID', 'Time')]),]








save(Alldf, file = "DF_10k.RData")