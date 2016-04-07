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
df <- data.frame(Uni2016[1:100,])
countries <- as.data.frame(table(df$country))
countries <- countries[which(countries$Frequency >= 1),]
colnames(countries) <- c("Country", "Frequency")

plot <- ggplot(countries, aes(x = countries$Country, y = countries$Frequency, fill = countries$Frequency))+xlab("Country") + ylab("Number of universities in each country")+geom_bar(stat = "identity")+ggtitle("Countries with Universities in top 100 (2016)")+geom_text(check_overlap = TRUE, aes(label = countries$Frequency)) 
plot + theme(
  text = element_text(size=16, colour = "GREEN"),
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
  
  

