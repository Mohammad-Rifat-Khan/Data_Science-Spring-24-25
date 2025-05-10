file_path <- "C:\\Users\\ashik\\OneDrive\\Desktop\\Dataset_MIdterm_sectoin(D).csv"
df <- read.csv(file_path,header=TRUE,sep = ",")



cat("Number of rows:", nrow(df),"\n")
cat("Number of columns:", ncol(df))


str(df)


unique_values_of_gender <- unique(df$Heart_Rate)
print(unique_values_of_gender)
sum(is.na(df))   

df[df==""]<- NA

num_of_missing_value_per_column <- sapply(df, function(x) sum(is.na(x)))
barplot(num_of_missing_value_per_column, 
        main = "Missing Values per Column", 
        col = "cyan", 
        las = 1, 
        cex.names = 0.8, 
        ylim = c(0, 5))

floor_or_ceil <- function(mean) {
  if (mean %% 1 == 0) {
    return(mean)
  } else if (mean %% 1 < 0.5) {
    return(floor(mean))
  } else {
    return(ceiling(mean))
  }
}
mean_age <- mean(df$Age, na.rm = TRUE)
df$Age[is.na(df$Age)] <- floor_or_ceil(mean_value)
print(paste("Total missing values:",sum(is.na(df[1]))))


most_frequent_gender <- as.numeric(names(sort(table(df$Gender), decreasing = TRUE)[1]))
df$Gender[is.na(df$Gender)] <- most_frequent_gender
print(paste("Total missing values:", sum(is.na(df[2]))))

str(df)

library(dplyr)
df <- df %>% mutate(BloodPressure = as.numeric(BloodPressure))
mean_blood_pressure <- mean(df$BloodPressure, na.rm = TRUE)
df$BloodPressure[is.na(df$BloodPressure)] <- floor_or_ceil(mean_blood_pressure)
print(paste("Total missing values:",sum(is.na(df[3]))))


str(df)

most_frequent_heart_rate <- names(sort(table(df$Heart_Rate), decreasing = TRUE)[1])
df$Heart_Rate[is.na(df$Heart_Rate)] <- most_frequent_heart_rate
print(paste("Total missing values:",sum(is.na(df[5]))))

str(df)

num_of_missing_value_per_column <- sapply(df, function(x) sum(is.na(x)))
missingVal<-colSums(is.na(df))
barplot(num_of_missing_value_per_column, 
        main = "Missing Values per Column", 
        col = "cyan", 
        las = 1, 
        cex.names = 0.8, 
        ylim = c(0, 5))



df$Heart_Rate <- ifelse(df$Heart_Rate == "Low",0,ifelse(df$Heart_Rate == "High", 1, NA))

str(df)


find_outliers <- function(column) {
  if (!is.numeric(column)) {
    stop("The input column must be numeric.")
  }
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  outlier_indices <- which(column < lower_bound | column > upper_bound)
  return(outlier_indices)
}
outlier_indices_age <- find_outliers(df$Age)
outlier_indices_blood_pressure <- find_outliers(df$BloodPressure)
outlier_indices_cholesterol <- find_outliers(df$Cholesterol)
outlier_indices_quantum_pattern_feature <- find_outliers(df$QuantumPatternFeature)
print(outlier_indices_age)
print(outlier_indices_blood_pressure)
print(outlier_indices_cholesterol)
print(outlier_indices_quantum_pattern_feature)

head(df)

all_outlier_indices <- unique(c(outlier_indices_age, 
                                outlier_indices_blood_pressure, 
                                outlier_indices_cholesterol, 
                                outlier_indices_quantum_pattern_feature))
df <- df[-all_outlier_indices, ]

head(df)



filtered_data <- df[df$Age >= 20 & df$Age <= 80 & df$BloodPressure >= 80 & df$BloodPressure <= 120, ]
View(head(filtered_data))

View(df)

normalize_dataset <- df
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
normalize_dataset$Age <- normalize(df$Age)
normalize_dataset$BloodPressure <- normalize(df$BloodPressure)
normalize_dataset$Cholesterol <- normalize(df$Cholesterol)
normalize_dataset$QuantumPatternFeature <- normalize(df$QuantumPatternFeature)
View(normalize_dataset)

idata <- normalize_dataset
class_dist <- table(idata$HeartDisease)
class_dist

barplot(table(idata$HeartDisease), main = "Original Class Distribution", col = "yellow")

if (class_dist[1] > class_dist[2]) {
  majority <- filter(idata, HeartDisease == 0)
  minority <- filter(idata, HeartDisease == 1)
} else {
  majority <- filter(idata, HeartDisease == 1)
  minority <- filter(idata, HeartDisease == 0)
}
set.seed(100)
os_minority <- minority %>% sample_n(nrow(majority), replace = TRUE)
os_balanced_data <- bind_rows(majority, os_minority)
table(os_balanced_data$HeartDisease)
barplot(table(os_balanced_data$HeartDisease), main = "Oversampled Distribution", col = "purple")

us_majority <- majority %>% sample_n(nrow(minority))
us_balanced_data <- bind_rows(minority, us_majority)
table(us_balanced_data$HeartDisease)

barplot(table(us_balanced_data$HeartDisease), main = "Undersampled Distribution", col = "magenta")

final_data <- os_balanced_data 


n <- nrow(final_data)
random_index <- sample(1:n, size = 0.8 * n)
train_data <- final_data[random_index, ]
test_data  <- final_data[-random_index, ]
View(train_data)
View(test_data)


aggregate(Age ~ Gender, data = final_data, 
          FUN = function(x) c(mean = mean(x), median = median(x), mode = as.numeric(names(which.max(table(x))))))

aggregate(Age ~ Heart_Rate, data = final_data, 
          FUN = function(x) c(mean = mean(x), median = median(x), mode = as.numeric(names(which.max(table(x))))))

final_data %>%
  group_by(Gender) %>%
  summarise(
    Range = max(Age, na.rm = TRUE) - min(Age, na.rm = TRUE),
    IQR = IQR(Age, na.rm = TRUE),
    Variance = var(Age, na.rm = TRUE),
    SD = sd(Age, na.rm = TRUE)
  )


age_summary <- final_data %>%
  group_by(Gender) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Mode_Age = as.numeric(names(which.max(table(final_data$Age)))),
    .groups = "drop"
  )

print(age_summary)


age_summary <- final_data %>%
  group_by(Heart_Rate) %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE),
    Mode_Age = as.numeric(names(which.max(table(final_data$Age)))),
    .groups = "drop"
  )

print(age_summary)

age_spread_summary <- final_data %>%
  group_by(Gender) %>%
  summarise(
    Range_Age = max(Age, na.rm = TRUE) - min(Age, na.rm = TRUE),
    IQR_Age = IQR(Age, na.rm = TRUE),
    Variance_Age = var(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE),
    .groups = "drop"
  )

print(age_spread_summary)

write.csv(final_data, file = "final_dataset.csv", row.names = FALSE)





