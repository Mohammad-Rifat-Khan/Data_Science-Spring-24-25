install.packages(c("ggplot2", "dplyr", "fmsb", "gridExtra", "gridBase","png"))

library(ggplot2)
library(dplyr)
library(fmsb)
library(gridExtra)
library(grid)
library(gridBase)  
library(png)

data <- read.csv("D:\\Academics\\DATA SCIENCE\\FINAL\\Lab assesment 2\\titanic.csv")

data <- data %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  na.omit()

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

png("D:\\Academics\\DATA SCIENCE\\FINAL\\Lab assesment 2\\radar_chart.png", width = 500, height = 500)
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

radar_img <- readPNG("D:\\Academics\\DATA SCIENCE\\FINAL\\Lab assesment 2\\radar_chart.png")
radar_grob <- rasterGrob(radar_img, interpolate = TRUE)

grid.arrange(
  scatter_plot, violin_plot,
  line_plot, radar_grob,
  ncol = 2,
  top = textGrob("Titanic Dataset Multivariate Visualization",
                 gp = gpar(fontface = "bold", fontsize = 16))
)