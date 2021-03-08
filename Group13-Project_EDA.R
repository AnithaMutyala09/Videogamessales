# Importing required libraries
library(ggplot2)

# Loading the database 
data <- read.csv("C:\\Documents\\Semester3\\OR 568\\Project\\vgsales Dataset.csv", stringsAsFactors = FALSE)

#removing the column Rank
data$Rank <- NULL

#removing the data with NAN values and filtering the data above 2016 
data <- data[data$Year != "N/A" & data$Year != "2017" & data$Year != "2020", ]
data$Year <- factor(data$Year)


years <- data.frame(cbind(Frequency = table(data$Year), Percent = prop.table(table(data$Year)) * 100))
years <- years[order(years$Frequency, ascending=FALSE), ]
years



# Top 10 frequent years 
df <- head(years, 10)
ggplot(data = df, mapping = aes(x = Frequency, y = row.names(df))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(df), color = row.names(df)), alpha = .7, size = 1.1) +
  ggtitle("The 10 most frequent years") +
  xlab(" Sales (in millions) ") +
  ylab("Years")  +
  coord_flip() +
  theme(legend.position = "none")

# Top 10 least frequent years 
df1 <- tail(years, 10)
ggplot(data = df1, mapping = aes(x = Frequency, y = row.names(df1))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(df1), color = row.names(df1))) +
  ggtitle("The 10 least frequent years") +
  xlab(" Sales (millions)") +
  ylab("Years") +
  coord_flip() +
  theme(legend.position = "none")


# Top frequent pplatform names
name <- data.frame(cbind(Frequency = table(data$Name), Percent = prop.table(table(data$Name)) * 100))
name <- head(name[order(name$Frequency, decreasing = TRUE), ], 5)
name

platform <- data.frame(cbind(Frequency = table(data$Platform), Percent = prop.table(table(data$Platform)) * 100))
platform <- head(platform[order(platform$Frequency, decreasing = T), ], 5)
platform

ggplot(data = name, mapping = aes(x = row.names(platform), y = Frequency)) +
  geom_bar(stat = "identity", aes(fill = row.names(platform)), color = "red") +
  ggtitle("Top 5 most frequent gaming platforms") +
  xlab("Gaming Platforms") +
  ylab("Percent Share in total sales") +
  theme(legend.position = "none")

# Top most genres

genre <- data.frame(cbind(Frequency = table(data$Genre), Percent = prop.table(table(data$Genre)) * 100))
genre <- genre[order(genre$Frequency, decreasing = TRUE), ]
genre


ggplot(data = genre, mapping = aes(x = Frequency, y = row.names(genre))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(genre), color = row.names(genre))) +
  ggtitle("Genre Frequency Distribution") +
  xlab("Number of games ") +
  ylab("Genre") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 20),
        legend.position = "none")

# Top 10 most publishers
published <- data.frame(cbind(Frequency = table(data$Publishe), Percent = prop.table(table(data$Publishe)) * 100))
published <- head(published[order(published$Frequency, decreasing = TRUE), ], 10)
published

ggplot(data = published, mapping = aes(x = Frequency, y = row.names(published))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(published), color = row.names(published))) +
  ggtitle("Top 10 most frequent publisher") +
  xlab("Number of games ") +
  ylab("Genre") +
  coord_flip() +
  theme(axis.text.x = element_text(angle =45, hjust=1),
        legend.position = "none")



df_regionalsales <- data.frame(Mean = c(mean(data$NA_Sales), mean(data$EU_Sales), mean(data$JP_Sales), mean(data$Other_Sales)))
row.names(df_regionalsales) <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
df_regionalsales

ggplot(data = df_regionalsales, mapping = aes(x = Mean, y = row.names(df_regionalsales))) +
  geom_line(group = 1, linetype = "dashed", color = "blue") +
  geom_point(size = 5, stroke = 1.5, shape = 21, mapping = aes(fill = row.names(df_regionalsales))) +
  theme_minimal() +
  ggtitle("Average sales in different parts of world") +
  ylab("Region") +
  xlab("Average Sales (millions)") +
  coord_flip() +
  theme(legend.position = "none")




