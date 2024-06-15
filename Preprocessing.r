### Preprocessing
### Language: R
### Author: Luisa Ripoll Alberola

### Import libraries

library(readr)
library(rvest)
library(dplyr)
library(tidyr)
library(tidyverse)
library(spacyr)
library(GGally)
library(genero)

### Read data

dataset2 <- read_csv("dataset2.csv")

### Correct data types

int_cols <- c(2:4, 6, 17:51)
factor_cols <- c(8, 10, 15:16)
dataset2[ , int_cols] <- lapply(dataset2[, int_cols], as.integer)
dataset2[ , factor_cols] <- lapply(dataset2[ , factor_cols], as.factor)

### Imputation of NaN values for Date and Content

colSums(is.na(dataset2), na.rm = TRUE)

# Step 1: Identify rows with NaN values in the "content" column
rows_with_nan <- which(is.nan(dataset2$Content) | is.na(dataset2$Content))

# Step 2: Extract URLs from these rows
urls_with_nan <- dataset2$URL[rows_with_nan]
head(urls_with_nan)
tail(urls_with_nan)

# Step 3: Drop rows with NaN values in the "content" column
dataset2 <- dataset2[-rows_with_nan, ]

# Check again remaining NaN values
colSums(is.na(dataset2), na.rm = TRUE)

# Check the 2 NaN dates
rows_with_nan <- which(is.nan(dataset2$Date) | is.na(dataset2$Date))

# Extract URLs from these rows
urls_with_nan <- dataset2$URL[rows_with_nan]
print(urls_with_nan)

# Manual imputation
dataset2$Date[rows_with_nan] <- as.Date("2024-02-16")
dataset2$Year[rows_with_nan] <- as.factor(2024)
dataset2$Month[rows_with_nan] <- as.factor("02")
dataset2$Day[rows_with_nan] <- as.integer(16)
reference_date <- as.Date("2024-02-29")
dataset2$Days_count[rows_with_nan] <- as.integer(difftime(reference_date, dataset2$Date[rows_with_nan], units = "days"))

### Imputation of NaN values for Title

# Check NaNs in the title
rows_with_nan <- which(is.nan(dataset2$Title) | is.na(dataset2$Title))
urls_with_nan <- dataset2$URL[rows_with_nan]
print(urls_with_nan)

# Function to extract title and subtitle from a URL
extract_title_subtitle <- function(url) {
  webpage <- read_html(url)
  
  # Extract title
  title <- webpage %>% 
    html_nodes(xpath = "//meta[@name='title']") %>% 
    html_attr("content") 
  
  # Extract subtitle
  subtitle <- webpage %>% 
    html_node(".article__summary") %>% 
    html_text() 
  
  return(list(title = title, subtitle = subtitle))
}

# Initialize empty lists to store titles and subtitles
titles <- c()
subtitles <- c()

# Loop through each URL and extract title and subtitle
for (url in urls_with_nan) {
  content <- extract_title_subtitle(url)
  titles <- c(titles, content$title)
  subtitles <- c(subtitles, content$subtitle)
}

# Add titles and subtitles to dataset2
dataset2$Title[rows_with_nan] <- titles
dataset2$Subtitle[rows_with_nan] <- subtitles

colSums(is.na(dataset2), na.rm = TRUE)

### Imputation of NaN values for Tags
### Web scraping

# Loop through each row of the dataset
for (i in 1:nrow(dataset2)) {
  # Check if the 'Tags' value is NaN
  if (is.na(dataset2$Tags[i])) {
    # Read the URL
    page <- read_html(dataset2$URL[i])
    
    # Extract meta tags with property="article:tag"
    tags <- page %>% 
      html_nodes(xpath = '//meta[@property="article:tag"]') %>% 
      html_attr("content")
    
    # Convert tags to a comma-separated string
    tags_string <- paste(tags, collapse = ",")
    
    # Update the 'Tags' column with the scraped tags
    dataset2$Tags[i] <- tags_string
  }
}

colSums(is.na(dataset2), na.rm = TRUE)

# Save the dataframe as a CSV file
write.csv(dataset2, file = "dataset3.csv", row.names = FALSE)
dataset3 <- read_csv("dataset3.csv")

### Preprocessing tags
### Reducing the number of levels in the factor

# Split tags and create a list
tag_list <- strsplit(as.character(dataset3$Tags), ",")

# Convert list to a single vector
all_tags <- unlist(tag_list)

# Get unique tags
unique_tags <- unique(all_tags)

head(sort(table(all_tags), decreasing = TRUE), n = 50L)

print(unique(dataset3$Section))
topics <- c("Fútbol", "Estados Unidos", "Rusia", "Alimentación", "Cine", "Real Madrid", 
            "Famosos", "LaLiga EA Sports", "Sexo", "papel", "Música", "Lenguaje", 
            "Ucrania", "Animales", "Psicología", "Enfermedades", "Pedro Sánchez", 
            "Guerra en Ucrania", "Instagram", "Santoral")

# Function to check if a topic exists in a tag list
topic_exists <- function(tag_list, topic) {
  any(sapply(tag_list, function(tags) topic %in% tags))
}

# Merging 'Ucrania' and 'Guerra en Ucrania'?
# Check which rows in tag_list have "Ucrania" as a tag
rows_with_ucrania <- unlist(lapply(tag_list, topic_exists, topic = "Ucrania"))
rows_with_war <- unlist(lapply(tag_list, topic_exists, topic = "Guerra en Ucrania"))

length(which(rows_with_ucrania))
length(which(rows_with_war))

(length(intersect(which(rows_with_ucrania), which(rows_with_war))))

# Merge 'RAE' and 'Lenguaje'?
rows_with_rae <- unlist(lapply(tag_list, topic_exists, topic = "RAE"))
rows_with_language <- unlist(lapply(tag_list, topic_exists, topic = "Lenguaje"))

length(which(rows_with_rae))
length(which(rows_with_language))

(length(intersect(which(rows_with_rae), which(rows_with_language))))

# Function to replace tags
replace_tags <- function(tag_list, old_tag, new_tag) {
  lapply(tag_list, function(tags) {
    if (old_tag %in% tags) {
      tags[tags == old_tag] <- new_tag
    }
    return(tags)
  })
}

# Replace "RAE" with "Lenguaje"
tag_list <- replace_tags(tag_list, "RAE", "Lenguaje")

# Replace "Guerra en Ucrania" with "Ucrania"
tag_list <- replace_tags(tag_list, "Guerra en Ucrania", "Ucrania")

# Function to create subsection vector
create_subsection <- function(tag_list, topics) {
  sapply(tag_list, function(tags) {
    intersection <- intersect(tags, topics)
    if (length(intersection) > 0) {
      return(intersection)
    } else {
      return(NA)
    }
  })
}

# Create subsection vector
subsection <- create_subsection(tag_list, topics)
subsection_single <- sapply(subsection, function(x) if(length(x) > 1) x[1] else x)
dataset3$Topics <- subsection_single

### Save the dataframe as a CSV file

write.csv(dataset3, file = "dataset3.csv", row.names = FALSE)
dataset3 <- read.csv("dataset3.csv")

### Author imputation

# Step 1: Identify rows with NaN values in the "Author" column
rows_with_nan <- which(is.nan(dataset3$Author) | is.na(dataset3$Author))

# Step 2: Extract URLs from these rows
urls_with_nan <- dataset3$URL[rows_with_nan]
head(urls_with_nan)

dataset3$Author <- as.character(dataset3$Author)

# Define a function to scrape author name from a given URL
scrape_author <- function(url) {
  webpage <- read_html(url)
  author <- webpage %>%
    html_nodes(".article-author__name span") %>%
    html_text() %>%
    trimws()
  
  return(author)
}

# Iterate over each row of the dataset
for (i in 1:nrow(dataset3)) {
  # Check if the 'Author' value is NaN
  if (is.na(dataset3$Author[i])) {
    # Scrape author name from the URL in the same row
    author <- scrape_author(dataset3$URL[i])
    
    # Check if author is retrieved
    if (length(author) > 0) {
      # Replace NaN value in 'Author' column with scraped author name
      dataset3$Author[i] <- author
    } else {
      # If author is not retrieved, set 'Author' value to NaN
      dataset3$Author[i] <- NA
    }
  }
}

colSums(is.na(dataset3), na.rm = TRUE)

# Iterative process until all NaNs are imputed

# Step 1: Identify rows with NaN values in the "Author" column
rows_with_nan <- which(is.nan(dataset3$Author) | is.na(dataset3$Author))

# Step 2: Extract URLs from these rows
urls_with_nan <- dataset3$URL[rows_with_nan]
head(urls_with_nan)
tail(urls_with_nan)

# Manual imputation
dataset3[dataset3$URL == "https://www.larazon.es/comunidad-valenciana/20221203/bxpotz257rcf7dgeyxsuxirozu.html", ]$Author = "Cintia Borja"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/20190617/kiu5jsxu25bizjdswmje3wikzi.html", ]$Author = "M. Hérnandez Sánchez-Barba"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/20200427/pg42r36hkfbtragkmd7wz7mlzi.html", ]$Author = "M. Hérnandez Sánchez-Barba"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/20210629/mlzr7bwn35btze4mqcm774oxcy.html", ]$Author = "Luis María Anson"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/20220317/fwivjkcc2vcafh6iqvigijerma.html", ]$Author = "Francisco Marhuenda"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/almodovar-tiene-razon_2024021365caaa8682085c0001655f36.html", ]$Author = "Luis María Anson"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/basura_2024012865b59280b8340700014399bf.html", ]$Author = "María José Navarro"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/columnistas/la-expansion-de-nueva-espana-YX9617652/", ]$Author = "M. Hérnandez Sánchez-Barba"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/columnistas/sturm-und-drang-GB8003118/", ]$Author = "M. Hérnandez Sánchez-Barba"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/ensenanza-matematicas-problema-pais_2024012865b59289327cdd0001e3695e.html", ]$Author = "Manuel de León"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/espana-estado-vasallo-cataluna_20240110659efd7967d53e0001dfe920.html?utm_source=redaccion&utm_medium=webpush&utm_campaign=notificacion", ]$Author = "Francisco Marhuenda"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/evitar-apocaliptica-tercera-guerra-mundial_202401036594f01867d53e0001c833f5.html", ]$Author = "Jorge Fernández Díaz"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/odio-israel_202310076521b31ee0d7620001e7d484.html", ]$Author = "Francisco Marhuenda"
dataset3[dataset3$URL == "https://www.larazon.es/opinion/tinduf_2024012465b04c7ee5843000011de99d.html", ]$Author = "Tomás Torres Peral"

# Step 1: Identify rows with NaN values in the "Author" column
rows_with_nan <- which(is.nan(dataset3$Author) | is.na(dataset3$Author))

# Step 2: Extract URLs from these rows
urls_with_nan <- dataset3$URL[rows_with_nan]
print(urls_with_nan)

# Eliminates the duplicated URLs with NA Topics
dataset3 <- dataset3 %>%
  group_by(URL) %>%
  filter(!(duplicated(URL) & is.na(Topics))) %>%
  ungroup()

# Eliminates duplicated URLs with same Topics
dataset3 <- dataset3[!duplicated(dataset3$URL), ]

(length(unique(dataset3$URL)))

# Save the dataframe as a CSV file
write.csv(dataset3, file = "dataset4.csv", row.names = FALSE)
dataset4 <- read.csv("dataset4.csv")

### Imputation of subtitles

# Step 1: Identify rows with NaN values in the "Subtitle" column
rows_with_nan <- which(is.nan(dataset3$Subtitle) | is.na(dataset3$Subtitle))

# Step 2: Extract URLs from these rows
urls_with_nan <- dataset3$URL[rows_with_nan]
head(urls_with_nan)

### Exploratory Data Analysis

# Drop rows with character type
# char_cols <- c("URL", "Tags", "Title", "Subtitle", "Date", "Content")
char_cols <- c(1, 9, 11:14)
dataset4 <- dataset4[ , -char_cols]

# Checking data types
dataset4$Users <- as.integer(dataset4$Users)
dataset4$New_users <- as.integer(dataset4$New_users)
dataset4$Avg_session <- as.integer(dataset4$Avg_session)
dataset4$Sessions <- as.integer(dataset4$Sessions)
dataset4$Author <- as.factor(dataset4$Author)
dataset4$Section <- as.factor(dataset4$Section)
dataset4$Year <- as.integer(dataset4$Year)
dataset4$Month <- as.integer(dataset4$Month)
# Convert columns 11 to 45 into integers
dataset4[, 11:45] <- lapply(dataset4[, 11:45], as.integer)
dataset4$Topics <- as.factor(dataset4$Topics)
head(dataset4)

## Chi-squared tests

dataset4_chi <- dataset4[ ,-6]

# Function to compute Chi-squared test p-values
chi_squared_pvals <- function(data) {
  var_names <- colnames(data)
  pval_matrix <- matrix(NA, ncol = ncol(data), nrow = ncol(data))
  colnames(pval_matrix) <- var_names
  rownames(pval_matrix) <- var_names
  
  for (i in 1:(ncol(data)-1)) {
    for (j in (i+1):ncol(data)) {
      tbl <- table(data[, i], data[, j])
      test <- chisq.test(tbl)
      pval_matrix[i, j] <- test$p.value
      pval_matrix[j, i] <- test$p.value
    }
  }
  
  return(pval_matrix)
}

# Calculate p-values
pval_matrix <- chi_squared_pvals(as.matrix(dataset4_chi))

# Convert the matrix to long format for ggplot2
pval_long <- melt(pval_matrix, na.rm = TRUE)

# Create the heatmap
ggplot(data = pval_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", na.value = NA) +
  labs(x = '', y = '', title = 'Chi-squared Test p-values') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Histograms

numeric_var_names <- names(dataset4)[sapply(dataset4, is.numeric)]
numeric_var_names <- setdiff(numeric_var_names, "label")

# Study the histograms of the numerical variables
metrics <- numeric_var_names[1:6]
date_vars <- numeric_var_names[7:9]
length_vars <- numeric_var_names[10:12]
title_vars <- numeric_var_names[13:22]
subtitle_vars <- numeric_var_names[23:32]
content_vars <- numeric_var_names[33:42]

# Loop through numeric variables and plot histograms
for (var in metrics) {
  min_val <- min(dataset4[[var]])
  max_val <- max(dataset4[[var]])
  hist(dataset4[[var]], main = var, xlab = var, xlim = c(min_val, max_val))
}

head(sort(dataset4$Users, decreasing = TRUE), n = 20)

# Studying if oversampling users close to 0 is feasible
less_users <- dataset4$Users[dataset4$Users < 1000]
hist(less_users, main = "Users", xlab = "Users")
length(less_users)

least_users <- dataset4$Users[dataset4$Users < 400]
hist(least_users, main = "Users", xlab = "Users")
length(least_users)

# Histogram of date
for (var in date_vars) {
  min_val <- min(dataset4[[var]])
  max_val <- max(dataset4[[var]])
  hist(dataset4[[var]], main = var, xlab = var, xlim = c(min_val, max_val))
}

# Histogram of length variables
for (var in length_vars) {
  min_val <- min(dataset4[[var]])
  max_val <- max(dataset4[[var]])
  hist(dataset4[[var]], main = var, xlab = var, xlim = c(min_val, max_val))
}

head(sort(dataset4$Content_length, decreasing = T), n = 15)

less_content <- dataset4$Content_length[dataset4$Content_length < 30000]
hist(less_content, main = "Content_length", xlab = "Content_length", xlim = c(0, 30000))

# Histograms of title variables
for (var in title_vars) {
  min_val <- min(dataset4[[var]])
  max_val <- max(dataset4[[var]])
  hist(dataset4[[var]], main = var, xlab = var, xlim = c(min_val, max_val))
}

# Histograms of subtitle variables
for (var in subtitle_vars) {
  min_val <- min(dataset4[[var]])
  max_val <- max(dataset4[[var]])
  hist(dataset4[[var]], main = var, xlab = var, xlim = c(min_val, max_val))
}

# Histograms of content variables 
for (var in content_vars) {
  min_val <- min(dataset4[[var]])
  max_val <- max(dataset4[[var]])
  hist(dataset4[[var]], main = var, xlab = var, xlim = c(min_val, max_val))
}

# Save the dataframe as a CSV file
# write.csv(dataset4, file = "dataset5.csv", row.names = FALSE)
dataset5 <- read.csv("dataset4.csv")

### Checking multinomial variables
### Extracting information of Author column, and fixing the quantity of levels

# Having 395 authors for only 5,000 rows won't add information.
length(unique(dataset4$Author))

## Manual modifications
dataset5$Author <- as.character(dataset5$Author)

# Merge "Álvaro Gómez-Chaparro" and "Álvaro Gómez - Chaparro"
dataset5[which(dataset5$Author == "Álvaro Gómez - Chaparro"), "Author"] <- "Álvaro Gómez-Chaparro"

# Replace "@Reborivera" for "Matías Rebolledo"
dataset5[which(dataset5$Author == "@Reborivera"), "Author"] <- "Matías Rebolledo"

# Merge "Silvia Jiiménez" and "Silvia Jiménez"
dataset5[which(dataset5$Author == "Silvia Jiiménez"), "Author"] <- "Silvia Jiménez"

# See article signed with R. C. and merge with "Rodrigo Carrasco" if it's him
dataset5[which(dataset5$Author == "R. C."), "Author"] <- "Rodrigo Carrasco"

# Merge "Patricia Contreras" and "Patricia Contreras Tejada"
dataset5[which(dataset5$Author == "Patricia Contreras Tejada"), "Author"] <- "Patricia Contreras"

# Merge "Laura C. Liébana" and "Laura Cano Liébana" 
dataset5[which(dataset5$Author == "Laura Cano Liébana"), "Author"] <- "Laura C. Liébana"

# Merge "Luis E. Togores" and "Luis Togores"
dataset5[which(dataset5$Author == "Luis Togores"), "Author"] <- "Luis E. Togores"

# Merge "L. Bustamante" with "Luis Bustamante"
dataset5[which(dataset5$Author == "L. Bustamante"), "Author"] <- "Luis Bustamante"

# Merge "José Luis Díaz Garde" and "José Luis Díez-Garde"
dataset5[which(dataset5$Author == "José Luis Díaz Garde"), "Author"] <- "José Luis Díez-Garde"

# Merge "José María Fernández-Rúa" and "José María Fernández Rúa"
dataset5[which(dataset5$Author == "José María Fernández Rúa"), "Author"] <- "José María Fernández-Rúa"

# Merge "Infodefensa", "Infodefensa.com", "Fernando Valente (Infodefensa.com)" and "Benjamín Carrasco (Infodefensa.com)"
dataset5[which(dataset5$Author == "Infodefensa"), "Author"] <- "Infodefensa.com"

# Merge "I. T.", "I. Trujillo" and "I.T"
dataset5[which(dataset5$Author == "I.T"), "Author"] <- "I. Trujillo"
dataset5[which(dataset5$Author == "I. T."), "Author"] <- "I. Trujillo"

# Check the author "Foto: Gonzalo Pérez"
# print(dataset3[which(dataset3$Author == "Foto: Gonzalo Pérez"), ]$URL)
dataset5[which(dataset5$Author == "Foto: Gonzalo Pérez"), "Author"] <- "C. S. Macías"

# Merge "Enrique Villar" and "Enrique Vlllar"
dataset5[which(dataset5$Author == "Enrique Vlllar"), "Author"] <- "Enrique Villar"

# Merge "Elric Ruiz", "Elric Ruiz - Elsotanoperdido" and "elsótanoperdido"
dataset5[which(dataset5$Author == "Elric Ruiz - Elsotanoperdido"), "Author"] <- "Elric Ruiz"
dataset5[which(dataset5$Author == "elsótanoperdido"), "Author"] <- "Elric Ruiz"

# Merge "E. S." and "E.S."
dataset5[which(dataset5$Author == "E. S."), "Author"] <- "E.S."

# Check "Editorial", "Ical" and "Atlas"
# print(dataset2[which(dataset2$Author == "Editorial"), ]$URL)
# print(dataset2[which(dataset2$Author == "Ical"), ]$URL)
# print(dataset2[which(dataset2$Author == "Atlas"), ]$URL)

# Merge "Álvaro G. Daza - What The Fav" and "Álvaro García Daza"
dataset5[which(dataset5$Author == "Álvaro G. Daza - What The Fav"), "Author"] <- "Álvaro García Daza"

# Merge "Álvaro Gómez-Chaparro" and "Álvaro Gómez - Chaparro"
dataset5[which(dataset5$Author == "Álvaro Gómez - Chaparro"), "Author"] <- "Álvaro Gómez-Chaparro"

# Merge "Daniel Gómez" and "Daniel Gómez Domínguez"
dataset5[which(dataset5$Author == "Daniel Gómez"), "Author"] <- "Daniel Gómez Domínguez"

# Merge "C. S. M." and "C. S. Macías"
dataset5[which(dataset5$Author == "C. S. M."), "Author"] <- "C. S. Macías"

# Merge "C. Laraña" and "Carmen Laraña"
dataset5[which(dataset5$Author == "C. Laraña"), "Author"] <- "Carmen Laraña"

# Calculate frequency of each unique value
author_freq <- table(dataset5$Author)

# Determine quartile boundaries
quartiles <- quantile(author_freq, probs = c(0, 0.25, 0.5, 0.75, 1))

# Assign quartile labels
author_quartiles <- cut(author_freq,
                        breaks = c(-Inf, quartiles[2:4], Inf),
                        labels = c("Q1", "Q2", "Q3", "Q4"),
                        include.lowest = TRUE)

df_qauthor <- data.frame(sort(unique(dataset5$Author)), author_quartiles)

merged_dataset <- merge(dataset5, df_qauthor, by.x = "Author", by.y = "sort.unique.dataset5.Author..", all.x = TRUE)
# merged_dataset <- merged_dataset[ , -47]
head(merged_dataset)

# New column "Gender"
merged_dataset$Author_gender <- genero(merged_dataset$Author)
head(merged_dataset, n = 20)
print(unique(merged_dataset[which(is.na(merged_dataset$Author_gender)), ]$Author))

## Correcting foreign names
# Identify names to be corrected
female_names <- c("Adrienne Mayor", "Amor Martínez", "Ángeles López", "Arantxa Herranz", "Concha García", "Dámaris Fernández", "Henar Soto", "Inma Bermejo", "Mamen Sala", "María José Navarro", "Maya Siminovich", "Mirentxu Arroqui", "Shelly Ramírez Pino", "Talya Kivanc")
male_names <- c("Alexander Mikaberidze", "Elric Ruiz", "Fran Cárceles", "Fran Castro", "Fran Fernández", "Fran Gómez", "France Philippart de Foy", "Goyo G. Maestro", "Josep Guijarro", "Lluis Fernández", "Matías Rebolledo", "Niklas Gustafson", "Raad Salam", "Raoul Higuera", "Rostyslav Averchuk", "Toni Montesinos", "Toni Ramos", "Vladislav Inozemtsev")

# Manually correct gender for the identified names
merged_dataset$Author_gender[merged_dataset$Author %in% female_names] <- "female"
merged_dataset$Author_gender[merged_dataset$Author %in% male_names] <- "male"
(unique(merged_dataset$Author_gender))
(unique(merged_dataset$author_quartiles))

# We obtained NA values in author_quartile when the articles doesn't have author. This values will be replaced by Q1. 
merged_dataset[which(is.na(merged_dataset$author_quartiles)), ]
merged_dataset[which(is.na(merged_dataset$author_quartiles)), "author_quartiles"] <- "Q1" 

merged_dataset$author_quartiles <- as.factor(merged_dataset$author_quartiles)
merged_dataset$Author_gender <- as.factor(merged_dataset$Author_gender)

# Drop Author column
merged_dataset <- merged_dataset[ , -1]
head(merged_dataset, n = 50)
(table(merged_dataset$Author_gender))

### Modifying the levels of "Section" variable

(table(merged_dataset$Section))
(length(unique(merged_dataset$Section)))

# Identify values with frequency less than 10
section_freq <- table(merged_dataset$Section)
values_to_replace <- names(section_freq[section_freq < 30])

# Replace values with NA where frequency is less than 10
merged_dataset$Section[merged_dataset$Section %in% values_to_replace] <- NA

(length(unique(merged_dataset$Section)))
(table(merged_dataset$Section))
(length(which(is.na(merged_dataset$Section))))

write.csv(merged_dataset, file = "dataset6.csv", row.names = FALSE)
dataset6 <- read.csv("dataset6.csv")

### EDA of sentiment analysis

### Chi-squared tests of the emotion variables

emotion_df <- dataset6[ , c(15:44)]
emotion_df <- lapply(emotion_df, as.integer)
emotion_df <- as.data.frame(emotion_df)
# head(emotion_df)

# Calculate p-values
pval_matrix <- chi_squared_pvals(as.matrix(emotion_df))

# Convert the matrix to long format for ggplot2
pval_long <- melt(pval_matrix, na.rm = TRUE)

# Create the heatmap
ggplot(data = pval_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", na.value = NA) +
  labs(x = '', y = '', title = 'Chi-squared Test p-values') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Barplots

barplot(
  colSums(prop.table(emotion_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.6,
  main = "Emotions in title",
  xlab="emotions", ylab = NULL)

barplot(
  colSums(prop.table(emotion_df[, 11:18])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.5,
  main = "Emotions in subtitle",
  xlab="emotions", ylab = NULL)

barplot(
  colSums(prop.table(emotion_df[, 21:28])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.4,
  main = "Emotions in content",
  xlab="emotions", ylab = NULL)

par(mfrow = c(1, 3))

barplot(
  colSums(prop.table(emotion_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.6,
  main = "Emotions in title",
  xlab="emotions", ylab = NULL)

barplot(
  colSums(prop.table(emotion_df[, 11:18])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.6,
  main = "Emotions in subtitle",
  xlab="emotions", ylab = NULL)

barplot(
  colSums(prop.table(emotion_df[, 21:28])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.6,
  main = "Emotions in content",
  xlab="emotions", ylab = NULL)


par(mfrow = c(1, 3))
barplot(
  colSums(prop.table(emotion_df[, 9:10])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.6,
  main = "Emotions in title",
  xlab="emotions", ylab = NULL)

barplot(
  colSums(prop.table(emotion_df[, 19:20])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.6,
  main = "Emotions in subtitle",
  xlab="emotions", ylab = NULL)

barplot(
  colSums(prop.table(emotion_df[, 29:30])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.6,
  main = "Emotions in content",
  xlab="emotions", ylab = NULL)

# Plots with ggplot
ggplot(dataset6, aes(x = Users)) + 
  geom_histogram(color = "white", breaks = seq(0, 160, by = 10))

ggplot(dataset6, aes(y = Users, x = Year)) + 
  geom_point()