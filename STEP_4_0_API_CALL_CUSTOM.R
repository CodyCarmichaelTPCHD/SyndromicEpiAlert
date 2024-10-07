



load("InputFile.RData")


DF_Iter <- final_df[9,]

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

### Have fields preset to limit memory use and allow for zero mix
fields_for_syns <-"&field=PID&field=Date&field=Time&field=HospitalName&field=ZipCode&field=ChiefComplaintOrig&field=DischargeDiagnosis&field=Sex&field=Age&field=Ethnicity_flat&field=Race_flat&field=Admit_Reason_Combo&field=Orig_Sex&field=ICD_Section_Desc_Flat"


# Assuming DF_Iter has Syndrome1 and Syndrome2 columns
syndromes1_for_link <- Syn_API_Coding$CCDD_HTML[Syn_API_Coding$Name %in% UserData$Syndrome1]
syndromes2_for_link <- Syn_API_Coding$CCDD_HTML[Syn_API_Coding$Name %in% UserData$Syndrome2]

# Create two sets of URLs
URL1 <- paste0('https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?datasource=va_er&startDate=', Start_Date, '&medicalGroupingSystem=essencesyndromes&userId=5629&endDate=', End_Date, '&percentParam=noPercent&hospFacilityType=emergency%20care&aqtTarget=DataDetails&ccddCategory=', syndromes1_for_link, '&geographySystem=region&detector=probrepswitch&timeResolution=daily&hasBeenE=1&refValues=true', fields_for_syns)
URL2 <- paste0('https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?datasource=va_er&startDate=', Start_Date, '&medicalGroupingSystem=essencesyndromes&userId=5629&endDate=', End_Date, '&percentParam=noPercent&hospFacilityType=emergency%20care&aqtTarget=DataDetails&ccddCategory=', syndromes2_for_link, '&geographySystem=region&detector=probrepswitch&timeResolution=daily&hasBeenE=1&refValues=true', fields_for_syns)

# Function to fetch data
fetch_data <- function(urls) {
  lapply(urls, function(u){
    api_response <- GET(u, authenticate(user = USER, password = PASS))
    api_response_json <- httr::content(api_response, as = "text")
    api_data <- fromJSON(api_response_json, simplifyDataFrame = TRUE)
    api_data$dataDetails
  })
}

# Fetch data for both sets of URLs
df1 <- fetch_data(URL1)
df2 <- fetch_data(URL2)

# Combine the results
Main_df1 <- Reduce(rbind, df1)
Main_df2 <- Reduce(rbind, df2)
Combined_Main_df <- inner_join(Main_df1, Main_df2)

# Remove duplicates
Combined_Main_df <- Combined_Main_df[!duplicated(Combined_Main_df[c('PID', 'Time')]),]

# Assign Syndrome Names (if needed)
Combined_Main_df$SyndromeName1 <- DF_Iter$Syndrome1
Combined_Main_df$SyndromeName2 <- DF_Iter$Syndrome2

Main_df <- Combined_Main_df

Main_df$SyndromeName <- DF_Iter$PairedSyndromeName

save(Main_df, file = "MainAPI_Results.RData")