install.packages(c("ggplot2", "dplyr", "fmsb", "gridExtra", "gridBase","png"))

library(ggplot2)
library(dplyr)
library(fmsb)
library(gridExtra)
library(grid)
library(gridBase)  
library(png)

data <- read.csv("path\\titanic.csv")

data <- data %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  na.omit()

histogram_plot <- ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram: Age Distribution", x = "Age", y = "Count") +
  theme_minimal()
show(histogram_plot)

bar_plot <- ggplot(data, aes(x = Sex)) +
  geom_bar(fill = "coral") +
  labs(title = "Bar Plot: Gender Count", x = "Sex", y = "Count") +
  theme_minimal()
show(bar_plot)

box_plot <- ggplot(data, aes(y = SibSp)) +
  geom_boxplot(fill = "lightgreen", color = "red") +
  labs(title = "Box: Siblings/Spouses Aboard", y = "SibSp") +
  theme_minimal()
show(box_plot)

grid.arrange(
  histogram_plot, bar_plot, box_plot,
  ncol = 3,
  top = textGrob("Titanic Dataset Univariate Visualization",
                 gp = gpar(fontface = "bold", fontsize = 16))
)


scatter_plot <- ggplot(data, aes(x = Age, y = Fare, color = factor(Survived))) +
  geom_point() +
  labs(title = "Scatter: Age vs Fare", color = "Survived") +
  theme_minimal()
show(scatter_plot)

violin_plot <- ggplot(data, aes(x = factor(Pclass), y = Fare, fill = factor(Pclass))) +
  geom_violin() +
  labs(title = "Violin: Fare by Pclass") +
  theme_minimal()
show(violin_plot)

line_data <- data %>%
  group_by(Age = round(Age)) %>%
  summarise(mean_fare = mean(Fare))

line_plot <- ggplot(line_data, aes(x = Age, y = mean_fare)) +
  geom_line(color = "blue") +
  labs(title = "Line: Avg Fare by Age") +
  theme_minimal()
show(line_plot)

normalize <- function(x) (x - min(x)) / (max(x) - min(x))

radar_data <- data %>%
  summarise(
    Age = mean(normalize(Age)),
    SibSp = mean(normalize(SibSp)),
    Parch = mean(normalize(Parch)),
    Fare = mean(normalize(Fare))
  )

radar_df <- rbind(rep(1,4), rep(0,4), radar_data)
colnames(radar_df) <- c("Age", "SibSp", "Parch", "Fare")

png("path\\radar_chart.png", width = 500, height = 500)
fmsb::radarchart(radar_df,
                 axistype = 1,
                 pcol = "darkred",
                 pfcol = rgb(1, 0, 0, 0.3),
                 plwd = 2,
                 cglcol = "grey",
                 cglty = 1,
                 axislabcol = "orange",
                 title = "Radar: Normalized Features")
dev.off()

radar_img <- readPNG("path\\radar_chart.png")
radar_grob <- rasterGrob(radar_img, interpolate = TRUE)

grid.arrange(
  scatter_plot, violin_plot, line_plot, radar_grob,
  ncol = 2,
  top = textGrob("Titanic Dataset Multivariate Visualization",
                 gp = gpar(fontface = "bold", fontsize = 16))
)
