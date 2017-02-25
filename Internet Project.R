#Loading the Data
datadb <- read.csv("C:\\Users\\pravinw\\Documents\\Fractal\\DATA SCIENTIST\\R\\Projects_for_R\\Internet\\internet.csv")
summary(datadb)
nation <- table(datadb$Continent)
barplot(nation, main = "Traffic Distribution Across Continents", xlab = "Continent", ylab = "Traffic", col = NULL)


# Data Summarization and Exploratory Analysis
unqpgview <- aov(datadb$Uniquepageviews ~ datadb$Visits, data = datadb)
summary(unqpgview)

plot(datadb$Uniquepageviews,datadb$Visits, main = "Unique Page Views Vs Visits",
     xlab = "Unique page View", ylab = "Visits", pch=19)
abline(lm(datadb$Uniquepageviews~datadb$Visits), col="red")
lines(lowess(datadb$Uniquepageviews,datadb$Visits), col="blue")

#Whether the unique page view value depends on visits

existTraffic <- aov(datadb$Exits ~ datadb$Timeinpage + datadb$Continent + datadb$Sourcegroup + datadb$Bounces
                    + datadb$Uniquepageviews + datadb$Visits, data = datadb)
summary(existTraffic)


#Factors Affecting the Exists from Website
timeonpage <- aov(datadb$Timeinpage~datadb$Exits+datadb$Continent+datadb$Sourcegroup+
                    datadb$Bounces+datadb$Uniquepageviews+datadb$Visits, data = datadb)
summary(timeonpage)

#Factors impacting the bounce 
bncefctr <- glm(datadb$BouncesNew ~ datadb$Timeinpage + datadb$Continent + datadb$Exits + 
                  datadb$Sourcegroup + datadb$Uniquepageviews + datadb$Visits, data = datadb, family = "binomial")
summary(bncefctr)
