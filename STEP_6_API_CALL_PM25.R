## MAIN CALL PMI25
library(httr)
library(jsonlite)
library(dplyr)



load("IterativeData.RData")

UserData <- DF_Iter

USER = UserData$Username
PASS = UserData$Password

Start_Date <- format(UserData$StartDate, "%d%b%Y") 
End_Date <- format(UserData$EndDate, "%d%b%Y")



PM_25_URL <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?percentParam=noPercent&endDate=",End_Date,"&airQualityParameterName=pm2.5-24hr&County=pierce,%20wa&userId=5629&datasource=airquality&State=wa&stationAggregateFunc=max&timeResolution=daily&aqtTarget=TimeSeries&detector=probrepswitch&timeAggregateFunc=max&startDate=",Start_Date)


PM_25_DF <- lapply(PM_25_URL, function(u){
  
  
  api_response <-  GET(u, authenticate(user = USER, password = PASS))
  api_response_json <- httr::content(api_response, as = "text")
  api_data <- fromJSON(api_response_json, simplifyDataFrame = TRUE)
  api_data$timeSeriesData
  
})

PM_25_DF <-Reduce(rbind, PM_25_DF)

PM_25_DF <- select(PM_25_DF, date, count, expected, color)

PM_25_DF$date <- as.Date(PM_25_DF$date)
PM_25_DF$count <- as.numeric(PM_25_DF$count)
PM_25_DF$expected <- as.numeric(PM_25_DF$expected)



save(PM_25_DF, file = "PM25_DF.RData")



