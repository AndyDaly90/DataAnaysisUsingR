install.packages("ggplot2")
install.packages("readr")
library(ggplot2)
library(readr)

UniData <- read.csv("timesData.csv")

#Of all the universities in the world, which are the best?
#https://www.kaggle.com/mylesoneill/world-university-rankings

# world_rank - world rank for the university.
# university_name - name of university.
# country - country of each university.
# teaching - university score for teaching (the learning environment).
# international - university score international outlook (staff, students, research).
# research - university score for research (volume, income and reputation).
# citations - university score for citations (research influence).
# income - university score for industry income (knowledge transfer).
# total_score - total score for university, used to determine rank.
# num_students - number of students at the university.
# student_staff_ratio - Number of students divided by number of staff.
# international_students - Percentage of students who are international.
# female_male_ratio - Female student to Male student ratio.
# year - year of the ranking (2011 to 2016 included).

Uni2011 <- UniData[UniData$year == 2011,]
Uni2012 <- UniData[UniData$year == 2012,]
Uni2013 <- UniData[UniData$year == 2013,]
Uni2014 <- UniData[UniData$year == 2014,]
Uni2015 <- UniData[UniData$year == 2015,]
Uni2016 <- UniData[UniData$year == 2016,]

#How many colleges in the data set.
unique(UniData$university_name, incomparables = FALSE)
#find the range of countries
unique(UniData$country)

# Plot top universities from around the world based on country 2016
top1002016 <- data.frame(Uni2016[1:100,])
countries <- as.data.frame(table(top1002016$country))
colnames(countries) <- c("Country", "Frequency")
countries <- countries[which(countries$Frequency >= 1),]


plot <- ggplot(countries, aes(x = countries$Country, y = countries$Frequency, fill = countries$Frequency))+xlab("Country") + ylab("Number of universities in each country")+geom_bar(stat = "identity")+ggtitle("Countries with Universities in top 100 (2016)")+geom_text(check_overlap = TRUE, aes(label = countries$Frequency)) 
plot + theme(
  text = element_text(size=16, colour = "RED"),
  axis.text = element_text(size = 8),
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8),
  legend.key = element_rect(fill = "white"),
  legend.background = element_rect(fill = "#e0ffff"),
  legend.position = "top",
  legend.text =  element_text(size = 8),
  #panel.grid.major = element_line(colour = "navy"),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#e0ffff")
)

print(plot)  
head(top1002016)
sapply(top1002016, class)

# Method for converting factor to numeric value
is.factor(top1002016$total_score)
top1002016$total_score <- as.numeric(as.character(top1002016$total_score))
names(top1002016)

# Cleaning data, removing = from world rank
help(gsub)
library(stringr)
str_replace_all(top1002016$world_rank, "[^[:alnum:]]", "")
typeof(world_rank)

top1002016$world_rank <- str_replace_all(top1002016$world_rank,
                                     "[^[:alnum:]]", "")

# Converting data back to factor for decision tree (Bad Idea)
top1002016$world_rank <- as.factor(top1002016$world_rank)
str(top100)

# Fit a tree model with top 100 universities 
# to find useful inputs
install.packages("rpart")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Remove non alphanumeric values
str(top1002016$international)
top1002016$international <- str_replace_all(top1002016$international,
                                     "[^[:alnum:]]", "")
top1002016$international <- as.factor(top1002016$international)

# Check score for research (volume, income and reputation), teaching, and citation scores and how they affect world ranking. 
tree <- rpart(world_rank ~ research + teaching + citations, data = top1002016, method = "class")
plot(tree)
text(tree)
prp(tree)

summary(tree)

top1002014 <- data.frame(Uni2014[1:100,])
# world rankings 2015 decision tree
tree <- rpart(world_rank ~ research + teaching + citations, data = top1002014, method = "class")
plot(tree)
text(tree)
prp(tree)

summary(tree)

install.packages("plotly")
library(plotly)
# Simple scatter plot
plot_ly(data = top1002016, x=research, y=teaching, mode = "markers", color = c(100:1))
plot_ly(data = top1002016, x=research, y=citations, mode = "markers")

# with colour scale
plot_ly(data = top1002016, x=research, y=teaching, mode = "markers", color = country)






