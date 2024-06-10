### Dataset Creation part II
### Language: R
### Author: Luisa Ripoll Alberola

### Import libraries

library(dplyr)
library(sentimentr)
library(tidyr)
library(syuzhet)
library(foreach)
library(doParallel)
library(progress)

### Reading data 

data <- read.csv('dataset1.csv')
head(data)

### Initial preprocessing

# Rename columns using rename function
data <- rename(data, Users = Usuarios, New_users = Usuarios.nuevos, Avg_session = Duración.media.de.la.sesión,
                Pages_session = Páginas.sesión, Sessions = Sesiones, P_rebound = Porcentaje.de.rebote)

# Ensure correct datatypes
data$New_users <- as.integer(data$New_users) 
data$Avg_session <- as.integer(data$Avg_session)
data$Sessions <- as.integer(data$Sessions)

# Ensure there aren't NaN values. In the conversion double -> integer, NaN values indicate existing decimals
(sum(is.na(data$New_users)))
(sum(is.na(data$Avg_session)))
(sum(is.na(data$Sessions)))

### Creation of the column 'Days_count'

# Convert the Date column to Date class
data$Date <- as.Date(data$Date)

# Extract information from the Date column
data$Year <- format(data$Date, "%Y")  # Extract year
data$Month <- format(data$Date, "%m")  # Extract month
data$Day <- format(data$Date, "%d")    # Extract day

# Calculate the days since creation
reference_date <- as.Date("2024-02-29")
data$Days_count <- as.numeric(difftime(reference_date, data$Date, units = "days"))
head(data$Days_count)

### Creation of the columns 'Title_length', 'Subtitle_length' and 'Content_length'

data$Title_length <- nchar(data$Title)
data$Subtitle_length <- nchar(data$Subtitle)

# Before calculating length in content, we should replace the character \n
data$Content <- gsub("\n\n", " ", data$Content)
data$Content <- gsub("\n", " ", data$Content)
data$Content_length <- nchar(data$Content)

### Sentiment analysis of titles
### 1st attempt: using sentimentr package

title_sentences <- get_sentences(data$Title)
title_emotion <- emotion(title_sentences)
head(title_emotion)

# Summarize emotion scores by element_id
emotion_summary <- title_emotion %>%
  group_by(element_id, emotion_type) %>%
  summarise(emotion_score = sum(emotion))

# Pivot the data to have emotions as columns
emotion_summary_pivot <- emotion_summary %>%
  pivot_wider(names_from = emotion_type, values_from = emotion_score, values_fill = 0) %>%
  rename_with(~ paste0(., "_title"), -element_id)

# Checking number of zeros per column
colSums(emotion_summary_pivot == 0)

### 2nd attempt: using syuzhet package

# Using parallelisation
# Number of cores to use for parallel processing
num_cores <- detectCores()

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Initialize an empty list to store sentiment scores for each title
sentiment_scores <- vector("list", length(data$Title))

# Perform parallel processing
sentiment_scores <- foreach(i = seq_along(data$Title), .combine = "c") %dopar% {
  title <- data$Title[i]
  # Split the title into sentences
  title_sentences <- syuzhet::get_sentences(title)
  
  # Extract emotions for each sentence
  sent_df <- syuzhet::get_nrc_sentiment(title_sentences, lang = "spanish")
  
  # Aggregate sentiment scores for the title
  agg_sentiment <- colSums(sent_df)
  
  # Return the aggregated sentiment scores
  agg_sentiment
}

# Stop parallel processing
stopCluster(cl)

# Reshape sentiment_scores into a matrix with 10 columns
num_columns <- 10
sentiment_matrix <- matrix(unlist(sentiment_scores), ncol = num_columns, byrow = TRUE)

# Set column names
colnames(sentiment_matrix) <- c("anger", "anticipation", "disgust", "fear", "joy", 
                                "sadness", "surprise", "trust", "negative", "positive")

# Comparing the number of zeros of the matrix with the one obtained through sentimentr
# Here, the number of zeros decreases, and we assume it is because it captures better the sentiments in Spanish
colSums(sentiment_matrix == 0)

### Merging emotion scores' matrix into our original dataset

# Convert the matrix to a data frame
sentiment_df <- as.data.frame(sentiment_matrix)

# Add row names as a column to the data frame
sentiment_df$row <- rownames(sentiment_df)

# Rename columns in sentiment_df
colnames(sentiment_df)[-11] <- paste0(colnames(sentiment_df)[-11], "_title")

# Reset indices in column X
data$X <- seq_len(nrow(data))

# Merge with your original dataset "data" based on row names
merged_data1 <- merge(data, sentiment_df, by.x = "X", by.y = "row", all.x = TRUE)

# Remove the row column
merged_data1$row <- NULL

### Sentiment analysis of subtitles

# Number of cores to use for parallel processing
num_cores <- detectCores()

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Initialize an empty list to store sentiment scores for each title
sentiment_scores <- vector("list", length(data$Subtitle))

# Perform parallel processing
sentiment_scores <- foreach(i = seq_along(data$Subtitle), .combine = "c") %dopar% {
  subtitle <- data$Subtitle[i]
  # Split the title into sentences
  subtitle_sentences <- syuzhet::get_sentences(subtitle)
  
  # Extract emotions for each sentence
  sent_df <- syuzhet::get_nrc_sentiment(subtitle_sentences, lang = "spanish")
  
  # Aggregate sentiment scores for the title
  agg_sentiment <- colSums(sent_df)
  
  # Return the aggregated sentiment scores
  agg_sentiment
}

# Stop parallel processing
stopCluster(cl)

# Reshape sentiment_scores into a matrix with 10 columns
num_columns <- 10
sentiment_matrix <- matrix(unlist(sentiment_scores), ncol = num_columns, byrow = TRUE)

# Set column names
colnames(sentiment_matrix) <- c("anger", "anticipation", "disgust", "fear", "joy", 
                                "sadness", "surprise", "trust", "negative", "positive")

# Convert the matrix to a data frame
sentiment_df <- as.data.frame(sentiment_matrix)

# Add row names as a column to the data frame
sentiment_df$row <- rownames(sentiment_df)

# Rename columns in sentiment_df
colnames(sentiment_df)[-11] <- paste0(colnames(sentiment_df)[-11], "_subtitle")

# Merge with your original dataset "data" based on row names
merged_data2 <- merge(merged_data1, sentiment_df, by.x = "X", by.y = "row", all.x = TRUE)

# Remove the row column
merged_data2$row <- NULL

### Sentiment analysis of the content

# Number of cores to use for parallel processing
num_cores <- detectCores()

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Initialize an empty list to store sentiment scores for each title
sentiment_scores <- vector("list", length(data$Content))

# Perform parallel processing
sentiment_scores <- foreach(i = seq_along(data$Content), .combine = "c") %dopar% {
  content <- data$Content[i]
  # Split the title into sentences
  content_sentences <- syuzhet::get_sentences(content)
  
  # Extract emotions for each sentence
  sent_df <- syuzhet::get_nrc_sentiment(content_sentences, lang = "spanish")
  
  # Aggregate sentiment scores for the title
  agg_sentiment <- colSums(sent_df)
  
  # Return the aggregated sentiment scores
  agg_sentiment
}

# Stop parallel processing
stopCluster(cl)

# Reshape sentiment_scores into a matrix with 10 columns
num_columns <- 10
sentiment_matrix <- matrix(unlist(sentiment_scores), ncol = num_columns, byrow = TRUE)

# Set column names
colnames(sentiment_matrix) <- c("anger", "anticipation", "disgust", "fear", "joy", 
                                "sadness", "surprise", "trust", "negative", "positive")

# Convert the matrix to a data frame
sentiment_df <- as.data.frame(sentiment_matrix)

# Add row names as a column to the data frame
sentiment_df$row <- rownames(sentiment_df)

# Rename columns in sentiment_df
colnames(sentiment_df)[-11] <- paste0(colnames(sentiment_df)[-11], "_content")

# Merge with your original dataset "data" based on row names
merged_data3 <- merge(merged_data2, sentiment_df, by.x = "X", by.y = "row", all.x = TRUE)

# Remove the row column
merged_data3$row <- NULL
merged_data3$X <- NULL

### Saving dataframe with sentiment analysis columns
write.csv(merged_data3, file = "dataset2.csv", row.names = FALSE)

