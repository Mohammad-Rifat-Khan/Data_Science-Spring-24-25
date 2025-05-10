library(ggplot2)


titanic <- read.csv("E:\\Academics\\DATA SCIENCE\\FINAL\\Lab assesment\\titanic.csv")

titanic_clean <- na.omit(titanic)

titanic_clean$Sex <- as.factor(titanic_clean$Sex)
titanic_clean$Pclass <- as.factor(titanic_clean$Pclass)
titanic_clean$Embarked <- as.factor(titanic_clean$Embarked)
titanic_clean$Survived <- as.factor(titanic_clean$Survived)

# 1. Pearson's correlation between Age and Fare
pearson_corr <- cor(titanic_clean$Age, titanic_clean$Fare, method = "pearson")
print(paste("Pearson's correlation between Age and Fare:", round(pearson_corr, 2)))

plot1 <- ggplot(titanic_clean, aes(x = Age, y = Fare)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Age vs Fare", x = "Age", y = "Fare")
print(plot1)


# 2. Spearman's rank correlation between SibSp and Parch
spearman_corr <- cor(titanic_clean$SibSp, titanic_clean$Parch, method = "spearman")
print(paste("Spearman's correlation between SibSp and Parch:", round(spearman_corr, 2)))

plot2 <- ggplot(titanic_clean, aes(x = SibSp, y = Parch)) +
  geom_jitter(width = 0.2, height = 0.2) +
  labs(title = "Jitter Plot of SibSp vs Parch", x = "SibSp", y = "Parch")
print(plot2)

# 3. Chi-squared test between Sex and Survived
chi_sq <- chisq.test(titanic_clean$Sex, titanic_clean$Survived)
print("Chi-squared test for Sex and Survived:")
print(chi_sq)

plot3 <- ggplot(titanic_clean, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Sex", x = "Sex", y = "Count")
print(plot3)

# 4. ANOVA for Fare across Pclass
anova_result <- aov(Fare ~ Pclass, data = titanic_clean)
print("ANOVA for Fare across Pclass:")
summary(anova_result)

plot4 <- ggplot(titanic_clean, aes(x = Pclass, y = Fare)) +
  geom_boxplot() +
  labs(title = "Fare by Pclass", x = "Pclass", y = "Fare")
print(plot4)