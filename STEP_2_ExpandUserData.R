library(dplyr)
library(tidyr)
library(lubridate)
print(0)
# Read the data
user_inputs <- read.csv("UserInputs.csv")
print(0.5)
# Convert 'UseHistoricalData' and 'ComboExamination' to logical
user_inputs$UseHistoricalData <- as.logical(user_inputs$UseHistoricalData)
user_inputs$ComboExamination <- as.logical(user_inputs$ComboExamination)
user_inputs$StartDate <- ymd(user_inputs$StartDate)
user_inputs$EndDate <- ymd(user_inputs$EndDate)

# Split 'Syndromes' into separate rows and set 'ComboExamination' to FALSE
split_rows <- user_inputs %>%
  mutate(Syndromes = strsplit(as.character(Syndromes), ";")) %>%
  unnest(Syndromes) %>%
  mutate(ComboExamination = FALSE)

# Add rows for 'ComboExamination' when TRUE in original data
if(user_inputs$ComboExamination == TRUE){
combo_rows <- user_inputs %>%
  filter(ComboExamination) %>%
  mutate(Syndromes = "ComboExamination")

# Combine the above two dataframes
combined_df <- bind_rows(split_rows, combo_rows)
}else{
  combined_df <- split_rows
}
# Initialize an empty dataframe for historical rows
historical_rows <- data.frame()

# Loop through the dataframe to generate historical rows
for (i in 1:nrow(combined_df)) {
  row <- combined_df[i, ]
  if (row$UseHistoricalData) {
    for (j in 1:row$LookbackYears) {
      new_row <- row
      new_row$StartDate <- new_row$StartDate - years(j)
      new_row$EndDate <- new_row$EndDate - years(j)
      historical_rows <- rbind(historical_rows, new_row)
    }
  }
}

# Combine all rows into the final dataframe
final_df <- bind_rows(combined_df, historical_rows)

# View the final dataframe
print(final_df)
print(1)

save(final_df, file = "InputFile.RData")