#required libraries
library(httr)
library(jsonlite)
library(dplyr)

load("IterativeData.RData")


UserData <- DF_Iter 

Syn_API_Coding <- read.csv("Query_API_Codings.csv")


USER = UserData$Username
PASS = UserData$Password

Start_Date <- format(UserData$StartDate, "%d%b%Y") 
End_Date <- format(UserData$EndDate, "%d%b%Y") 
syndromes_for_link <- Syn_API_Coding$CCDD_HTML[Syn_API_Coding$Name %in% UserData$Syndromes]



### Have fields preset to limit memory use and allow for zero mix
fields_for_syns <-"&field=PID&field=Date&field=Time&field=HospitalName&field=ZipCode&field=ChiefComplaintOrig&field=DischargeDiagnosis&field=Sex&field=Age&field=Ethnicity_flat&field=Race_flat&field=Admit_Reason_Combo&field=Orig_Sex&field=ICD_Section_Desc_Flat"


URL <- paste0('https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?datasource=va_er&startDate=',Start_Date,'&medicalGroupingSystem=essencesyndromes&userId=5629&endDate=',End_Date,'&percentParam=noPercent&hospFacilityType=emergency%20care&aqtTarget=DataDetails&ccddCategory=', syndromes_for_link,'&geographySystem=region&detector=probrepswitch&timeResolution=daily&hasBeenE=1&refValues=true',fields_for_syns)




df <- lapply(URL, function(u){
  api_response <- GET(u, authenticate(user = USER, password = PASS))
  api_response_json <- httr::content(api_response, as = "text")
  api_data <- fromJSON(api_response_json, simplifyDataFrame = TRUE)
  api_data$dataDetails
})


Main_df <- Reduce(rbind, df)


Main_df <- Main_df[!duplicated(Main_df[c('PID', 'Time')]),]

Main_df$SyndromeName <- DF_Iter$Syndromes



save(Main_df, file = "MainAPI_Results.RData")