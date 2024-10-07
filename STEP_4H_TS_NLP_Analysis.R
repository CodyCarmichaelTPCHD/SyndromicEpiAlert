### NLP Analysis

library(plotly)
library(tidyr)
library(tidyverse)
library(tidytext)
library(purrr)
library(topicmodels)
library(irlba)
library(igraph)
library(ggraph)
library(networkD3)
library(tm)
library(dplyr)


load("MainAPI_Results.RData")
load("IterativeData.RData")


## Need to do stuff for Chief Complaint, Discharge Diagnosis, ICD_Section , Admit Reason

CC_Column <- select(Main_df, ChiefComplaintOrig)


DD_Column <- select(Main_df, DischargeDiagnosis)


AD_Column <- select(Main_df, Admit_Reason_Combo)

ICD_Column <- select(Main_df, ICD_Section_Desc_Flat)



## Clean the text

## Clean CC

CC_Column_Cleaned <- CC_Column %>%
  mutate(
    # Convert to uppercase
    Uppercase = toupper(ChiefComplaintOrig),
    # Tokenize on ";"
    Strings = strsplit(Uppercase, ";"),
    # Remove duplicates
    Strings = lapply(Strings, function(x) {
      x <- gsub("\\.", "", x)
      x <- unique(trimws(x))
      x <- x[!grepl("^[A-Z][0-9]{2}", x)]
      x <- gsub("[[:punct:]]", "", x)
      x[x != ""]  # Removes empty strings
    })
  )
## Now I can tokenize, then do frequency etc. 
CC_Column_Cleaned <- CC_Column_Cleaned %>%
  mutate(
    # Tokenize Strings on singular words and perform other operations
    ProcessedStrings = lapply(Strings, function(stringList) {
      # Flatten the list of strings into a single string, then tokenize
      unlistedStrings <- unlist(stringList)
      words <- unlist(strsplit(unlistedStrings, "\\s+"))
      # Remove basic stopwords and convert to uppercase
      stopwordsList <- stopwords("en")
      cleanedWords <- words[!tolower(words) %in% tolower(stopwordsList)]
      toupper(cleanedWords)
    })
  )

CC_Column_Cleaned$Strings <- CC_Column_Cleaned$ProcessedStrings











## Clean DD

DD_Column_Cleaned <- DD_Column %>%
  mutate(
    # Convert to uppercase
    Uppercase = toupper(DischargeDiagnosis),
    
    # Tokenize on ";"
    Tokens = strsplit(Uppercase, ";"),
    
    # Remove duplicates
    UniqueTokens = lapply(Tokens, function(x) {
      x <- gsub("\\.", "", x)
      x <- unique(trimws(x))
      x[x != ""]  # Removes empty strings
    }),
    
   Tokens = lapply(Tokens, function(x) {
      x <- gsub("\\.", "", x)
      x <- unique(trimws(x))
      x[x != ""]  # Removes empty strings
    })
    
  )
   





## Clean AD
## Capitalize all, then remove all ., then remove pattern of ICD codes

AD_Column_Cleaned <- AD_Column %>%
  mutate(
    # Convert to uppercase
    Uppercase = toupper(Admit_Reason_Combo),
    # Tokenize on ";"
    Strings = strsplit(Uppercase, ";"),
    # Remove duplicates
    Strings = lapply(Strings, function(x) {
      x <- gsub("\\.", "", x)
      x <- unique(trimws(x))
      x <- x[!grepl("^[A-Z][0-9]{2}", x)]
      x <- gsub("[[:punct:]]", "", x)
      x[x != ""]  # Removes empty strings
    })
)
## Now I can tokenize, then do frequency etc. 
AD_Column_Cleaned <- AD_Column_Cleaned %>%
  mutate(
    # Tokenize Strings on singular words and perform other operations
    ProcessedStrings = lapply(Strings, function(stringList) {
      # Flatten the list of strings into a single string, then tokenize
      unlistedStrings <- unlist(stringList)
      words <- unlist(strsplit(unlistedStrings, "\\s+"))
      # Remove basic stopwords and convert to uppercase
      stopwordsList <- stopwords("en")
      cleanedWords <- words[!tolower(words) %in% tolower(stopwordsList)]
      toupper(cleanedWords)
    })
)

AD_Column_Cleaned$Strings <- AD_Column_Cleaned$ProcessedStrings





## Clean ICD

###################################################
# Cleaning process
ICD_Column_Cleaned <- ICD_Column %>%
  mutate(
    # Convert to uppercase
    Uppercase = toupper(ICD_Section_Desc_Flat),
    
    # Tokenize on ";"
    Tokens = strsplit(Uppercase, ";"),
    
    # Remove duplicates
    UniqueTokens = lapply(Tokens, function(x) {
      x <- unique(trimws(x))
      x[x != ""]  # Removes empty strings
    })
  )

###################################################



## Frequency




### ICD FREQUENCIES
#####################################################################

# Updated function to create n-grams from a vector
create_ngrams <- function(vector, n) {
  ngrams <- map(seq_along(vector), ~ {
    if (.x <= (length(vector) - n + 1)) {
      paste(vector[.x:(.x + n - 1)], collapse = " / ")
    } else {
      NA_character_  # Ensure the function always returns a character vector
    }
  }) %>% unlist() %>% na.omit()
  return(ngrams)
}

# Prepare the data
ICD_tokens_df <- ICD_Column_Cleaned %>%
  unnest(UniqueTokens) %>%
  filter(UniqueTokens != "") %>%
  dplyr::rename(word = UniqueTokens)


# Generate Unigrams, Bigrams, and Trigrams
ICD_unigram_freq <- ICD_tokens_df %>%
  dplyr::count(word, sort = TRUE)


ICD_bigrams <- ICD_Column_Cleaned %>%
  mutate(Bigrams = map(UniqueTokens, ~ create_ngrams(.x, 2))) %>%
  unnest(Bigrams)

ICD_trigrams <- ICD_Column_Cleaned %>%
  mutate(Trigrams = map(UniqueTokens, ~ create_ngrams(.x, 3))) %>%
  unnest(Trigrams)

# Count frequencies of bigrams and trigrams
ICD_bigram_freq <- ICD_bigrams %>%
  dplyr::count(Bigrams, sort = TRUE)

ICD_trigram_freq <- ICD_trigrams %>%
  dplyr::count(Trigrams, sort = TRUE)


################################################

#DD Frequencies

########################################################
# Prepare the data
DD_tokens_df <- DD_Column_Cleaned %>%
  unnest(UniqueTokens) %>%
  filter(UniqueTokens != "") %>%
  dplyr::rename(word = UniqueTokens)

# Generate Unigrams, Bigrams, and Trigrams
DD_unigram_freq <- DD_tokens_df %>%
  dplyr::count(word, sort = TRUE)

DD_bigrams <- DD_Column_Cleaned %>%
  mutate(Bigrams = map(UniqueTokens, ~ create_ngrams(.x, 2))) %>%
  unnest(Bigrams)

DD_trigrams <- DD_Column_Cleaned %>%
  mutate(Trigrams = map(UniqueTokens, ~ create_ngrams(.x, 3))) %>%
  unnest(Trigrams)

# Count frequencies of bigrams and trigrams
DD_bigram_freq <- DD_bigrams %>%
  dplyr::count(Bigrams, sort = TRUE)

DD_trigram_freq <- DD_trigrams %>%
  dplyr::count(Trigrams, sort = TRUE)



#AD Frequencies

################################################

# Prepare the data
AD_tokens_df <- AD_Column_Cleaned %>%
  unnest(ProcessedStrings) %>%
  filter(ProcessedStrings != "") %>%
  dplyr::rename(word = ProcessedStrings)

# Generate Unigrams, Bigrams, and Trigrams
AD_unigram_freq <- AD_tokens_df %>%
  dplyr::count(word, sort = TRUE)

AD_bigrams <- AD_Column_Cleaned %>%
  mutate(Bigrams = map(ProcessedStrings, ~ create_ngrams(.x, 2))) %>%
  unnest(Bigrams)

AD_trigrams <- AD_Column_Cleaned %>%
  mutate(Trigrams = map(ProcessedStrings, ~ create_ngrams(.x, 3))) %>%
  unnest(Trigrams)

# Count frequencies of bigrams and trigrams
AD_bigram_freq <- AD_bigrams %>%
  dplyr::count(Bigrams, sort = TRUE)

AD_trigram_freq <- AD_trigrams %>%
  dplyr::count(Trigrams, sort = TRUE)



## CC Frequencies

# Prepare the data
CC_tokens_df <- CC_Column_Cleaned %>%
  unnest(ProcessedStrings) %>%
  filter(ProcessedStrings != "") %>%
  dplyr::rename(word = ProcessedStrings)

# Generate Unigrams, Bigrams, and Trigrams
CC_unigram_freq <- CC_tokens_df %>%
  dplyr::count(word, sort = TRUE)

CC_bigrams <- CC_Column_Cleaned %>%
  mutate(Bigrams = map(ProcessedStrings, ~ create_ngrams(.x, 2))) %>%
  unnest(Bigrams)

CC_trigrams <- CC_Column_Cleaned %>%
  mutate(Trigrams = map(ProcessedStrings, ~ create_ngrams(.x, 3))) %>%
  unnest(Trigrams)

# Count frequencies of bigrams and trigrams
CC_bigram_freq <- CC_bigrams %>%
  dplyr::count(Bigrams, sort = TRUE)

CC_trigram_freq <- CC_trigrams %>%
  dplyr::count(Trigrams, sort = TRUE)







## Topic Modeling

##############################################
### ICD TOPIC MODELING: 



# Convert your ICD_tokens_df to a document-term matrix
ICD_dtm <- ICD_tokens_df %>%
  dplyr::count(document = row_number(), word) %>%
  cast_dtm(document, word, n)

# Run LDA for topic modeling
ICD_lda_model <- LDA(ICD_dtm, k = 5)  # Assuming you want to find 5 topics

# Extract the terms associated with each topic
ICD_topics <- tidy(ICD_lda_model, matrix = "beta")

# For each topic, find the top terms
ICD_top_terms <- ICD_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

##NOTE: Beta values are relevance measures in how common a term shows up in the topic presented. 


##############################################



### DD Topic Modeling 

DD_dtm <- DD_tokens_df %>%
  dplyr::count(document = row_number(), word) %>%
  cast_dtm(document, word, n)

# Run LDA for topic modeling
DD_lda_model <- LDA(DD_dtm, k = 5)  # Assuming you want to find 5 topics

# Extract the terms associated with each topic
DD_topics <- tidy(DD_lda_model, matrix = "beta")

# For each topic, find the top terms
DD_top_terms <- DD_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)



### AD Topic Modeling 


AD_dtm <- AD_tokens_df %>%
  dplyr::count(document = row_number(), word) %>%
  cast_dtm(document, word, n)

# Run LDA for topic modeling
AD_lda_model <- LDA(AD_dtm, k = 5)  # Assuming you want to find 5 topics

# Extract the terms associated with each topic
AD_topics <- tidy(AD_lda_model, matrix = "beta")

# For each topic, find the top terms
AD_top_terms <- AD_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)





### CC Topic Modeling 


CC_dtm <- CC_tokens_df %>%
  dplyr::count(document = row_number(), word) %>%
  cast_dtm(document, word, n)

# Run LDA for topic modeling
CC_lda_model <- LDA(CC_dtm, k = 5)  # Assuming you want to find 5 topics

# Extract the terms associated with each topic
CC_topics <- tidy(CC_lda_model, matrix = "beta")

# For each topic, find the top terms
CC_top_terms <- CC_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)








## Social Network Analysis/ Bubble plot

############################################################################


# Calculate the threshold as 5% of the number of rows
threshold <- nrow(ICD_tokens_df) * 0.05

# Token frequency calculation remains the same
token_freq <- ICD_tokens_df %>%
  unnest(Tokens) %>%
  dplyr::count(Tokens, sort = TRUE)

# Selecting tokens that appear more than the threshold
frequent_tokens <- token_freq %>%
  filter(n > threshold)

# Create edges with filtered tokens
edges <- ICD_tokens_df %>%
  mutate(doc_id = row_number()) %>%
  unnest(Tokens) %>%
  filter(Tokens %in% frequent_tokens$Tokens) %>%
  group_by(doc_id) %>%
  filter(dplyr::n() > 1) %>%
  dplyr::summarise(pairs = list(combn(Tokens, 2, simplify = FALSE)), .groups = 'drop') %>%
  unnest(pairs) %>%
  mutate(pairs = map_chr(pairs, ~ paste(sort(.x), collapse = ","))) %>%
  separate(pairs, into = c("source", "target"), sep = ",") %>%
  group_by(source, target) %>%
  dplyr::summarise(weight = dplyr::n(), .groups = 'drop')

# Determine limits for filtering
top_percent_limit <- min(ceiling(nrow(token_freq) * 0.2), 20)
top_edges_limit <- min(ceiling(nrow(edges) * 0.2), 20)

# Apply filtering based on the calculated limits
# For nodes
top_nodes <- token_freq %>%
  top_n(top_percent_limit, n) %>%
  select(Tokens)

# For edges
top_edges <- edges %>%
  top_n(top_edges_limit, weight)

# Ensure that source and target in the top_edges are matched to the top_nodes
top_edges <- top_edges %>%
  filter(source %in% top_nodes$Tokens & target %in% top_nodes$Tokens)

# Create a data frame of unique terms (nodes) from top_edges
nodes <- data.frame(name = unique(c(top_edges$source, top_edges$target))) %>%
  mutate(id = row_number() - 1)

# Match source and target IDs
top_edges <- top_edges %>%
  left_join(nodes, by = c("source" = "name")) %>%
  rename(source_id = id) %>%
  left_join(nodes, by = c("target" = "name")) %>%
  rename(target_id = id) %>%
  select(source_id, target_id, weight)

# Create the network graph using networkD3 with filtered data
network <- forceNetwork(Links = top_edges, Nodes = nodes, Source = "source_id",
                        Target = "target_id", NodeID = "name",
                        Group = "name", opacity = 0.8, fontSize = 12)
#########################################################################
## DD Cluster Analysis


# Calculate the threshold as APPEARS AT LEAST 5 TIMES
DD_threshold <- 5

# Token frequency calculation
DD_token_freq <- DD_tokens_df %>%
  unnest(word) %>%
 dplyr::count(word, sort = TRUE)

# Selecting tokens that appear more than the threshold
DD_frequent_tokens <- DD_token_freq %>%
  filter(n > DD_threshold)

# Create edges with filtered tokens
DD_edges <- DD_tokens_df %>%
  mutate(doc_id = row_number()) %>%
  unnest(Tokens) %>%
  filter(Tokens %in% DD_frequent_tokens$word) %>%
  group_by(doc_id) %>%
  filter(dplyr::n() > 1)

if (nrow(DD_edges) == 0) {
  DD_network <- "Data insufficient to create dependable networks."
} else {
  DD_edges <- DD_edges %>%
    dplyr::summarise(pairs = list(combn(Tokens, 2, simplify = FALSE))) %>%
    unnest(pairs) %>%
    mutate(pairs = map_chr(pairs, ~ paste(sort(.x), collapse = ","))) %>%
    separate(pairs, into = c("source", "target"), sep = ",") %>%
    group_by(source, target) %>%
    dplyr::summarise(weight = dplyr::n(), .groups = 'drop')
  
  # Calculate median weight
  DD_median_weight <- median(DD_edges$weight)
  
  # Filter edges by weight if more than 5 edges
  if(nrow(DD_edges) > 5) {
    DD_edges <- DD_edges %>%
      filter(weight > DD_median_weight)
  }
  
  # Determine limits for filtering
  top_percent_limit <- min(ceiling(nrow(DD_token_freq) * 0.1), 20)
  top_edges_limit <- min(ceiling(nrow(DD_edges) * 0.1), 20)
  
  # Apply filtering based on the calculated limits
  # For nodes
  top_nodes <- DD_token_freq %>%
    top_n(top_percent_limit, n) %>%
    select(word)
  
  # For edges
  top_edges <- DD_edges %>%
    top_n(top_edges_limit, weight) %>%
    filter(source %in% top_nodes$word & target %in% top_nodes$word)
  
  # Create a data frame of unique terms (nodes) from top_edges
  DD_nodes <- data.frame(name = unique(c(top_edges$source, top_edges$target))) %>%
    mutate(id = row_number() - 1)
  
  # Match source and target IDs in the edges data frame to the node IDs
  top_edges <- top_edges %>%
    left_join(DD_nodes, by = c("source" = "name")) %>%
    rename(source_id = id) %>%
    left_join(DD_nodes, by = c("target" = "name")) %>%
    rename(target_id = id) %>%
    select(source_id, target_id, weight)
  
  # Create the network graph using networkD3 with filtered data
  DD_network <- forceNetwork(Links = top_edges, Nodes = DD_nodes, Source = "source_id",
                             Target = "target_id", NodeID = "name",
                             Group = "name", opacity = 0.8, fontSize = 12)
}







# Calculate the threshold as APPEARS AT LEAST 5 TIMES


# Token frequency calculation
AD_token_freq <- AD_tokens_df %>%
  unnest(word) %>%
  dplyr::count(word, sort = TRUE)


# Create edges with filtered tokens
AD_edges <- AD_tokens_df %>%
  dplyr::mutate(doc_id = row_number()) %>%
  unnest() %>%
  group_by(doc_id) %>%
  dplyr::filter(dplyr::n() > 1) 




if (nrow(AD_edges) == 0) {
  AD_network <- "Data insufficient to create dependable networks."
} else {

AD_edges <- AD_edges %>%
  dplyr::summarise(pairs = list(combn(Strings, 2, simplify = FALSE))) %>%
  unnest(pairs) %>%
  dplyr::mutate(pairs = map_chr(pairs, ~ paste(sort(.x), collapse = ","))) %>%
  separate(pairs, into = c("source", "target"), sep = ",") %>%
  group_by(source, target) %>%
  dplyr::summarise(weight = dplyr::n(), .groups = 'drop')

# Calculate median weight
AD_median_weight <- median(AD_edges$weight)

# Filter edges by weight (keep only heavier half)
if(nrow(AD_edges)>5){
AD_edges <- AD_edges %>%
  filter(weight > AD_median_weight)
}
# Create a data frame of unique terms (nodes)
AD_nodes <- data.frame(name = unique(c(AD_edges$source, AD_edges$target))) %>%
  dplyr::mutate(id = row_number() - 1)

# Ensure that source and target in the edges data frame are matched to the node IDs
AD_edges <- AD_edges %>%
  left_join(AD_nodes, by = c("source" = "name")) %>%
  dplyr::rename(source_id = id) %>%
  left_join(AD_nodes, by = c("target" = "name")) %>%
  dplyr::rename(target_id = id) %>%
  select(source_id, target_id, weight)

# Create the network graph using networkD3
AD_network <- forceNetwork(Links = AD_edges, Nodes = AD_nodes, Source = "source_id",
                           Target = "target_id", NodeID = "name",
                           Group = "name", opacity = 0.8, fontSize = 12)



}



## CC Network



# Token frequency calculation
CC_token_freq <- CC_tokens_df %>%
  unnest(word) %>%
  dplyr::count(word, sort = TRUE)


# Create edges with filtered tokens
CC_edges <- CC_tokens_df %>%
  dplyr::mutate(doc_id = row_number()) %>%
  unnest() %>%
  group_by(doc_id) %>%
  dplyr::filter(dplyr::n() > 1) 

if (nrow(CC_edges) == 0) {
  CC_network <- "Data insufficient to create dependable networks."
} else {
  CC_edges <- CC_edges %>%
    dplyr::summarise(pairs = list(combn(Strings, 2, simplify = FALSE))) %>%
    unnest(pairs) %>%
    dplyr::mutate(pairs = map_chr(pairs, ~ paste(sort(.x), collapse = ","))) %>%
    separate(pairs, into = c("source", "target"), sep = ",") %>%
    group_by(source, target) %>%
    dplyr::summarise(weight = dplyr::n(), .groups = 'drop')
  
# Calculate median weight
CC_median_weight <- median(CC_edges$weight)

# Filter edges by weight (keep only heavier half)
if(nrow(CC_edges)>5){
CC_edges <- CC_edges %>%
  filter(weight > CC_median_weight)
}
# Create a data frame of unique terms (nodes)
CC_nodes <- data.frame(name = unique(c(CC_edges$source, CC_edges$target))) %>%
  dplyr::mutate(id = row_number() - 1)

# Ensure that source and target in the edges data frame are matched to the node IDs
CC_edges <- CC_edges %>%
  left_join(CC_nodes, by = c("source" = "name")) %>%
  dplyr::rename(source_id = id) %>%
  left_join(CC_nodes, by = c("target" = "name")) %>%
  dplyr::rename(target_id = id) %>%
  select(source_id, target_id, weight)

# Create the network graph using networkD3
CC_network <- forceNetwork(Links = CC_edges, Nodes = CC_nodes, Source = "source_id",
                           Target = "target_id", NodeID = "name",
                           Group = "name", opacity = 0.8, fontSize = 12)




}

## Change of terms over time? Check for term drift? Future implementation. 

## Maybe next version do some Text Classification and check differences between Emergency Only and Inpatient visits if applicable? 




## First frequencies

## Save created docs, top 10 unigram, bigram, trigram
ADU_10 <- head(AD_unigram_freq, 10)
CCU_10 <- head(CC_unigram_freq, 10)
DDU_10 <- head(DD_unigram_freq, 10)
ICDU_10 <- head(ICD_unigram_freq, 10)

ADB_10 <- head(AD_bigram_freq, 10)
CCB_10 <- head(CC_bigram_freq, 10)
DDB_10 <- head(DD_bigram_freq, 10)
ICDB_10 <- head(ICD_bigram_freq, 10)

ADT_10 <- head(AD_trigram_freq, 10)
CCT_10 <- head(CC_trigram_freq, 10)
DDT_10 <- head(DD_trigram_freq, 10)
ICDT_10 <- head(ICD_trigram_freq, 10)




save(ADU_10, CCU_10, DDU_10, ICDU_10, file = "UnigramsNLP.RData")

save(ADB_10, CCB_10, DDB_10, ICDB_10, file = "BigramsNLP.RData")

save(ADT_10, CCT_10, DDT_10, ICDT_10, file = "TrigramsNLP.RData")

## Then TM
save(CC_top_terms, AD_top_terms, DD_top_terms, ICD_top_terms, file = "TopicModelingNLP.RData")

## Then Clustering
ICD_network <- network

save(AD_network, CC_network, DD_network, network, file = "SNA_NetworksNLP.RData")

