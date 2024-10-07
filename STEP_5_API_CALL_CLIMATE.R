## API CLIMATE MAIN

library(httr)
library(jsonlite)
library(dplyr)

load("IterativeData.RData")

UserData <- DF_Iter

USER = UserData$Username
PASS = UserData$Password

Start_Date <- format(UserData$StartDate, "%d%b%Y") 
End_Date <- format(UserData$EndDate, "%d%b%Y")


weather_URL <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?percentParam=noPercent&endDate=",End_Date,"&userId=5629&datasource=va_weather_aggr&timeResolution=daily&aqtTarget=DataDetails&detector=probrepswitch&startDate=",Start_Date,"&stationID=sew-sea&refValues=true")

Weather_DF <- lapply(weather_URL, function(u){
  
  
  api_response <-  GET(u, authenticate(user = USER, password = PASS))
  api_response_json <- httr::content(api_response, as = "text")
  api_data <- fromJSON(api_response_json, simplifyDataFrame = TRUE)
  api_data$dataDetails
  
})

Weather_DF <-Reduce(rbind, Weather_DF)

Weather_DF <- select(Weather_DF, Date, MaxTemp, MinTemp, AvgTemp)

Weather_DF$MaxTemp <- as.numeric(Weather_DF$MaxTemp)
Weather_DF$AvgTemp <- as.numeric(Weather_DF$AvgTemp)
Weather_DF$MinTemp <- as.numeric(Weather_DF$MinTemp)
Weather_DF$Date <- as.Date(Weather_DF$Date, format = "%m/%d/%Y")

save(Weather_DF, file = "Main_Weather_DF.RData")