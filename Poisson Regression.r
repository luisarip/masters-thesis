### Poisson Regression
### Language: R
### Author: Luisa Ripoll Alberola

### Import libraries

library(glmnet)
library(caret)
library(utils)
library(foreach)
library(doParallel)
library(car)
library(MASS)
library(glmmTMB)
library(BAS)
library(arules)
library(arulesViz)

### Read data

dataset6 <- read.csv("dataset6.csv")
head(dataset6)

### Check data types

dataset6$Users <- as.integer(dataset6$Users)
dataset6$New_users <- as.integer(dataset6$New_users)
dataset6$Avg_session <- as.integer(dataset6$Avg_session)
dataset6$Sessions <- as.integer(dataset6$Sessions)
dataset6$Section <- as.factor(dataset6$Section)
dataset6$Year <- as.integer(dataset6$Year)
# dataset6$Month <- as.factor(dataset6$Month)
dataset6$Month <- as.integer(dataset6$Month)
# Convert columns 10 to 44 into integers
dataset6[, 10:44] <- lapply(dataset6[, 10:44], as.integer)
dataset6$Topics <- as.factor(dataset6$Topics)
dataset6$author_quartiles <- as.factor(dataset6$author_quartiles)
dataset6$Author_gender <- as.factor(dataset6$Author_gender)
str(dataset6)

colSums(is.na(dataset6))

### Converting NA levels into "Other"

# First, we convert them to char
dataset6$Section <- as.character(dataset6$Section)
dataset6$Topics <- as.character(dataset6$Topics)
dataset6$Author_gender <- as.character(dataset6$Author_gender)

# Modifications
dataset6$Section[is.na(dataset6$Section)] <- "Other"
dataset6$Topics[is.na(dataset6$Topics)] <- "Other"
dataset6$Author_gender[is.na(dataset6$Author_gender)] <- "Other"

any(is.na(dataset6))

# Converting back to factor data type
dataset6$Section <- as.factor(dataset6$Section)
dataset6$Topics <- as.factor(dataset6$Topics)
dataset6$Author_gender <- as.factor(dataset6$Author_gender)

### Checking for NA values
sum(is.na(dataset6))

### Applying dummy variables

dummy_data <- dummyVars(~ ., data = dataset6)
dataset7 <- predict(dummy_data, newdata = dataset6)
colnames(dataset7)
# Saving dataset with dummy vars
write.csv(dataset7, "dataset7.csv")

### Feature selection: Association Rules Mining

# Convert the dataset to transactions
transactions <- as(dataset6, "transactions")

# Mine association rules
rules <- apriori(transactions, parameter = list(support = 0.1, confidence = 0.5))
inspect(head(sort(rules, by = "lift"), 10))

## Finding redundant rules
rules.sorted <- sort(rules, by="lift", decreasing = TRUE)
rules.sorted <- rules.sorted[1:25000]
subset.matrix <- is.subset(rules.sorted, rules.sorted)
# Convert the sparse matrix to a dense matrix
subset.matrix <- as.matrix(subset.matrix)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
# which(redundant)

# Remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(head(sort(rules.pruned, by = "lift"), 10))

## Studying the frequency of rules
length(rules.pruned)

# Extract antecedents and their items
antecedents_list <- LIST(lhs(rules.pruned))
all_antecedents <- unlist(antecedents_list)

# Count frequency of each variable in antecedents
variable_freq <- table(all_antecedents)

# Print the frequency of each variable
print(sort(variable_freq, ascending = FALSE))

# Select the most frequent and contextually important features
important_features <- names(variable_freq[variable_freq > 10])
print(important_features)

# Extract the base variable names
base_variables <- sub("=.*", "", names(variable_freq))

# Aggregate the counts based on the base variable names
aggregated_counts <- tapply(variable_freq, base_variables, sum)
print(sort(aggregated_counts, decreasing = TRUE))

## Filter rules related to the response variable 'Users'
# Step 1: Filter rules to include only those with the response variable on the RHS
response_rules <- subset(rules.pruned, rhs %pin% "Users")

# Step 2: Sort the response-specific rules by lift
response_rules_sorted <- sort(response_rules, by = "lift", decreasing = TRUE)

# Step 3: Select the top 20,000 rules
inspect(response_rules_sorted)

# Extract antecedents and their items
antecedents_list <- LIST(lhs(response_rules_sorted))
all_antecedents <- unlist(antecedents_list)

# Repeat the process to extract the variable names
variable_freq <- table(all_antecedents)
important_features <- names(variable_freq[variable_freq > 10])
base_variables <- sub("=.*", "", names(variable_freq))
aggregated_counts <- tapply(variable_freq, base_variables, sum)
print(sort(aggregated_counts, decreasing = TRUE))

## Plotting rules
plot(response_rules[1:20], method="graph", control=list(type="items"))
par(cex.axis = 0.5)  # Adjust the size of the axis labels
plot(response_rules, method="paracoord", control=list(reorder=TRUE, cex.axis = 0.25))
par(cex.axis = 0.5)  # Adjust the size of the axis labels
plot(response_rules, method="paracoord", control=list(reorder=TRUE, cex.axis = 0.25))
plot(response_rules)
plot(response_rules, method = "matrix", measure = "lift")

### Feature selection using GVIF values

model0 <- glm(Users ~ ., family = poisson, data = dataset6)
vif_values <- vif(model0)
print(vif_values)

# Iterative deselection of variables with higher GVIF
model01 <- update(model0, .~. - Year)
print(car::vif(model01))

model02 <- update(model01, .~. - negative_content)
print(car::vif(model02))

model03 <- update(model02, .~. - positive_content)
print(car::vif(model03))

model04 <- update(model03, .~. - fear_content)
print(car::vif(model04))

### 1st model: full model

model1 <- glm(Users ~., family = poisson, data = as.data.frame(dataset7))
summary(model1)

model11 <- update(model1, .~. - Year)
summary(model11)

# Negative binomial model
model12 <- glm.nb(Users ~., link = log, data = as.data.frame(dataset7))
summary(model12)

# Log likelihood
(logLik(model1))

plot(model11)

### 2nd model: 1st selection of features

feature1 <- dataset6[, names(dataset6) %in% c("Users", "Pages_session", "Sessions", 
                     "New_users", "Avg_session", "positive_title", "trust_subtitle",
                      "P_rebound", "negative_title")]

model2 <- glm(Users ~ ., family = poisson, data = as.data.frame(feature1))
summary(model2)
plot(model2)

# Check for overdispersion
residual_deviance <- residuals(model2, type = "deviance")
residual_df <- df.residual(model2)
overdispersion <- sum(residual_deviance^2) / residual_df
if (overdispersion > 1) {
  cat("Overdispersion present\n")
  print(overdispersion)
} else {
  cat("No overdispersion detected\n")
}

# Quasipoisson model
model21 <- glm(Users ~., family = quasipoisson, data = feature1)
summary(model21)

(sum(dpois(feature1$Users, lambda=exp(predict(model21)),log=TRUE)))

# Negative binomial model
model22 <- glm.nb(Users ~., link = log, data = feature1)
summary(model22)

# Scale predictors
vars_to_scale <- setdiff(names(feature1), "Users")
feature1_scaled <- scale(feature1[, vars_to_scale])

# Combine scaled predictors with the response variable
feature1_scaled <- cbind(feature1_scaled, Users = feature1$Users)
head(feature1)
head(feature1_scaled)

# Fit a Poisson regression model with scaled predictors
model23 <- glm(Users ~ ., family = poisson, data = as.data.frame(feature1_scaled))
summary(model23)

plot(model22)

## Try weighted Poisson regression: not good results
# Calculate Pearson residuals
pearson_residuals <- residuals(model22, type = "pearson")

# Compute weights 
weights <- 1 / abs(pearson_residuals)

# Refit the model with weights
model23 <- glm.nb(Users ~ ., link = log, data = as.data.frame(feature1), weights = weights)
summary(model23)
plot(model23)

### 3rd model: 2nd selection of variables

feature2 <- dataset6[, names(dataset6) %in% c("Users", "Sessions", 
                     "author_quartiles", "positive_subtitle", "negative_title", 
                     "negative_subtitle", "Pages_session", "positive_title",
                     "trust_subtitle")]
dummy_data_2 <- dummyVars(~ ., data = feature2)
feature2 <- predict(dummy_data_2, newdata = feature2)

model3 <- glm(Users ~., family = poisson, data = as.data.frame(feature2))
summary(model3)

# Check for overdispersion
residual_deviance <- residuals(model3, type = "deviance")
residual_df <- df.residual(model3)
overdispersion <- sum(residual_deviance^2) / residual_df
if (overdispersion > 1) {
  cat("Overdispersion present\n")
  print(overdispersion)
} else {
  cat("No overdispersion detected\n")
}

model32 <- glm.nb(Users ~., link = log, data = as.data.frame(feature2))
summary(model32)

pearson_residuals <- residuals(model32, type = "pearson")
weights <- 1 / abs(pearson_residuals)
model33 <- glm.nb(Users ~ ., link = log, data = as.data.frame(feature2), weights = weights)
summary(model33)

