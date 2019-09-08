library(readr)
library(tidyverse)
library(ggplot2)

setwd("~/Stat Programming/R Documents")


library(readxl)
World_Happiness_Data_2018 <- read_excel("World Happiness Data 2018.xlsx")
View(World_Happiness_Data_2018)
##visualizing the year variable
ggplot(World_Happiness_Data_2018, aes(year)) + geom_bar(fill="purple") + labs(title="Number of Observations Per Year", subtitle = "Source: World Happiness Dataset", x="Year", y="Number of Observations") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

##creating a dataset with just year 2017
attach(World_Happiness_Data_2018)
happy17 <- filter(World_Happiness_Data_2018, year==2017)
View(happy17)
head(happy17)
attach(happy17)

ggplot(happy17, aes(Happiness)) + geom_histogram(fill="red", bins=20) + labs(title="Distribution of Happiness Variable", x="Answer to Survey Question Imagining a Ladder of Happiness from 0 to 10", y="Count") + theme(plot.title = element_text(hjust = 0.5))

ggplot(happy17, aes(LogGDPPerCapita)) + geom_histogram(fill="dark green", bins=10) + labs(title="Distribution of Log GDP Per Capita", x="Log of GDP Per Capita", y="Count") + theme(plot.title = element_text(hjust = 0.5))

ggplot(happy17, aes(SocialSupport)) + geom_histogram(fill="dark blue", bins=20) + labs(title="Distribution of Social Support", x="Social Support", y="Count") + theme(plot.title = element_text(hjust = 0.5))

ggplot(happy17, aes(HealthyLifeExpectancyAtBirth)) + geom_histogram(fill="pink", bins=30) + labs(title="Distribution of Healthy Life Expectancy at Birth", x="Healthy Life Expectancy", y="Count") + theme(plot.title = element_text(hjust = 0.5))

ggplot(happy17, aes(FreedomToMakeLifeChoices)) + geom_histogram(fill="light blue", bins=30) + labs(title="Distribution of Freedom to Make Life Choices", x="Freedom to Make Life Choices, as Measured by Survey Question", y="Count") + theme(plot.title = element_text(hjust = 0.5))

ggplot(happy17, aes(Generosity)) + geom_histogram(fill="orange", bins=30) + labs(title="Distribution of Generosity Variable", x="Generosity, as Measured by Survey Question", y="Count") + theme(plot.title = element_text(hjust = 0.5))

ggplot(happy17, aes(PerceptionsOfCorruption)) + geom_histogram(fill="brown", bins=30) + labs(title="Distribution of Perceptions of Corruption Variable", x="Perceptions of Corruption", y="Count") + theme(plot.title = element_text(hjust = 0.5))

ggplot(happy17, aes(PositiveAffect)) + geom_histogram(fill="yellow", bins=30) + labs(title="Distribution of Positive Affect Variable", x="Positive Affect", y="Count") + theme(plot.title = element_text(hjust = 0.5))

ggplot(happy17, aes(NegativeAffect)) + geom_histogram(fill="gray", bins=30) + labs(title="Distribution of Negative Affect Variable", x="Negative Affect", y="Count") + theme(plot.title = element_text(hjust = 0.5))

ggplot(happy17, aes(ConfidenceInNationalGovernment)) + geom_histogram(fill="blue", bins=20) + labs(title="Confidence in National Government Variable", x="Confidence in National Government", y="Count") + theme(plot.title = element_text(hjust = 0.5))

ggplot(happy17, aes(GINIIndexAverage)) + geom_histogram(fill="green", bins=20) + labs(title="Distribution of Gini Index Average Variable", x="GINI Index", y="Count") + theme(plot.title = element_text(hjust = 0.5))

ggplot(happy17, aes(GiniIndexGallup)) + geom_histogram(fill="purple", bins=30) + labs(title="Distribution of Gini Index Gallup Variable", x="GINI Index Gallup", y="Count") + theme(plot.title = element_text(hjust = 0.5))

#Scatterplots
#Log GDP Per Capita
ggplot(happy17, aes(x=LogGDPPerCapita, y=Happiness)) + geom_point() + labs(title="Happiness Score vs. Log GDP Per Capita", subtitle="Red is Least Squares Line, Blue is Loess Fit", x="Log GDP Per Capita", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(color="blue") + geom_smooth(method="lm", color="red")
reg.gdp <-lm(Happiness ~ LogGDPPerCapita)
summary(reg.gdp)

#Social Support
ggplot(happy17, aes(x=SocialSupport, y=Happiness)) + geom_point() + labs(title="Happiness Score vs. Social Support", subtitle="Red is Least Squares Line, Blue is Loess Fit", x="Social Support", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) + geom_smooth(color="blue") + geom_smooth(method="lm", color="red")
reg.ss <-lm(Happiness ~ SocialSupport)
summary(reg.ss)

#HealthyLifeExp
ggplot(happy17, aes(x=HealthyLifeExpectancyAtBirth, y=Happiness)) + geom_point() + labs(title="Happiness Score vs. Healthy Life Expectancy at Birth", subtitle="Red is Least Squares Line, Blue is Loess Fit", x="Social Support", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) + geom_smooth(color="blue") + geom_smooth(method="lm", color="red")
reg.le <-lm(Happiness ~ HealthyLifeExpectancyAtBirth)
summary(reg.le)

#Freedom to make Life...
ggplot(happy17, aes(x=FreedomToMakeLifeChoices, y=Happiness)) + geom_point() + labs(title="Happiness Score vs. Freedom to Make Life Choices", subtitle="Red is Least Squares Line, Blue is Loess Fit", x="Freedom to Make Life Choices", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) + geom_smooth(color="blue") + geom_smooth(method="lm", color="red")
reg.freedom <-lm(Happiness ~ FreedomToMakeLifeChoices)
summary(reg.freedom)

#Generosity
ggplot(happy17, aes(x=Generosity, y=Happiness)) + geom_point() + labs(title="Happiness Score vs. Generosity", subtitle="Red is Least Squares Line, Blue is Loess Fit", x="Generosity", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) + geom_smooth(color="blue") + geom_smooth(method="lm", color="red")
reg.generosity<-lm(Happiness ~ Generosity)
summary(reg.generosity)

#Perceptions of Corruption
ggplot(happy17, aes(x=PerceptionsOfCorruption, y=Happiness)) + geom_point() + labs(title="Happiness Score vs. Perceptions of Corruption", subtitle="Red is Least Squares Line, Blue is Loess Fit", x="Perceptions of Corruption", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) + geom_smooth(color="blue") + geom_smooth(method="lm", color="red")
reg.corruption<-lm(Happiness ~ PerceptionsOfCorruption)
summary(reg.corruption)

#Positive Affect
ggplot(happy17, aes(x=PositiveAffect, y=Happiness)) + geom_point() + labs(title="Happiness Score vs. PositiveAffect", subtitle="Red is Least Squares Line, Blue is Loess Fit", x="PositiveAffect", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) + geom_smooth(color="blue") + geom_smooth(method="lm", color="red")
reg.positive<-lm(Happiness ~ PositiveAffect)
summary(reg.positive)

#Negative Affect
ggplot(happy17, aes(x=NegativeAffect, y=Happiness)) + geom_point() + labs(title="Happiness Score vs. Negative Affect", subtitle="Red is Least Squares Line, Blue is Loess Fit", x="Negative Affect", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) + geom_smooth(color="blue") + geom_smooth(method="lm", color="red")
reg.negative<-lm(Happiness ~ NegativeAffect)
summary(reg.negative)

#Confidence in National Government
ggplot(happy17, aes(x=ConfidenceInNationalGovernment, y=Happiness)) + geom_point() + labs(title="Happiness Score vs. Confidence in National Government", subtitle="Red is Least Squares Line, Blue is Loess Fit", x="Confidence in National Government", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) + geom_smooth(color="blue") + geom_smooth(method="lm", color="red")
reg.government <- lm(Happiness ~ ConfidenceInNationalGovernment)
summary(reg.government)

#Gini Index Average
ggplot(happy17, aes(x=GiniIndexGallup, y=Happiness)) + geom_point() + labs(title="Happiness Score vs. Gini Index Gllup", subtitle="Red is Least Squares Line, Blue is Loess Fit", x="Gini Index Gallup", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle=element_text(hjust = 0.5)) + geom_smooth(color="blue") + geom_smooth(method="lm", color="red")
reg.gallup <- lm(Happiness ~ GiniIndexGallup)
summary(reg.gallup)

#Question 3
median(SocialSupport)

happy17$SocialSupportCategory <- factor(
  ifelse(happy17$SocialSupport <= median(happy17$SocialSupport, na.rm = TRUE), 0, 
         ifelse(happy17$SocialSupport > median(happy17$SocialSupport, na.rm = TRUE), 1, NA)), 
  0:1, labels = c("low", "high"))

conservation$alt.factor <- factor(
  ifelse(conservation$alt < median(conservation$alt, na.rm = TRUE), 1, 
         ifelse(conservation$alt > median(conservation$alt, na.rm = TRUE), 2, NA)), 
  1:2, labels = c("below", "above"))

happy17$SocialSupportCategory <- factor(
  ifelse(happy17$SocialSupport <= median(happy17$SocialSupport, na.rm = TRUE), 0, 
         ifelse(happy17$SocialSupport > median(happy17$SocialSupport, na.rm = TRUE), 1, NA)), 
  0:1, labels = c("low", "high"))
head(happy17$SocialSupportCategory)
cbind(happy17, SocialSupportCategory)
attach(happy17)
ggplot(happy17, aes(happy17$SocialSupportCategory)) + geom_bar() + labs(title="Distribution of Social Support Category", x="Social Support Category", y="Count")

ggplot(happy17, aes(x=LogGDPPerCapita, y=Happiness, color=SocialSupportCategory)) + geom_point() + labs(title="Happiness Score vs. Log GDP Per Capita", x="Log GDP Per Capita", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5)) 

happy5 <- filter(World_Happiness_Data_2018, country %in% c("France", "United States", "Greece", "Ireland", "Portugal"))
head(happy5)

ggplot(happy5, aes(x=year, y=Happiness, color=country)) + geom_point() + labs(title="Happiness Score vs. Year", x="Year", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth() + geom_smooth(method="lm") + scale_x_continuous(labels=c("2005", "2008", "2010", "2012", "2014", "2016"))
  
  scale_x_discrete(labels=("1880", "1900", "1920", "1940", "1960", "2000"))
  
happy5_17 <-filter(happy5, year==2017)
head(happy5_17)

ggplot(happy5, aes(x=year, y=Happiness, color=country)) + geom_point() + labs(title="Happiness Score vs. Year", x="Year", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth() + geom_smooth(method="lm") + scale_x_continuous(labels=c("2005", "2008", "2010", "2012", "2014", "2016"))

ggplot(happy5, aes(x=LogGDPPerCapita, y=Happiness, color=country)) + geom_point() + labs(title="Happiness Score vs. Log GDP Per Capita", x="Log GDP Per Capita", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth() + geom_smooth(method="lm")

ggplot(happy5, aes(x=PositiveAffect, y=Happiness, color=country)) + geom_point() + labs(title="Happiness Score vs. Positive Affect", x="Positive Affect", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth() + geom_smooth(method="lm") 

ggplot(happy5_17, aes(x=LogGDPPerCapita, y=Happiness, color=country)) + geom_point() + labs(title="Happiness Score vs. Log GDP Per Capita", x="Log GDP Per Capita", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=country)) 
warnings()


ggplot(happy5_17, aes(x=LogGDPPerCapita, y=Happiness, color=country)) + geom_point() + labs(title="Happiness Score vs. Log GDP Per Capita", x="Log GDP Per Capita", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=country)) + geom_smooth()




happy17$SocialSupportCategory <- factor(
  ifelse(happy17$SocialSupport <= median(happy17$SocialSupport, na.rm = TRUE), 0, 
         ifelse(happy17$SocialSupport > median(happy17$SocialSupport, na.rm = TRUE), 1, NA)), 
  0:1, labels = c("low", "high"))
head(happy17$SocialSupportCategory)
cbind(happy17, SocialSupportCategory)
attach(happy17)
ggplot(happy17, aes(happy17$SocialSupportCategory)) + geom_bar() + labs(title="Distribution of Social Support Category", x="Social Support Category", y="Count")

ggplot(happy17, aes(x=LogGDPPerCapita, y=Happiness, color=SocialSupportCategory)) + geom_point() + labs(title="Happiness Score vs. Log GDP Per Capita", x="Log GDP Per Capita", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5)) 

happy5 <- filter(World_Happiness_Data_2018, country %in% c("France", "United States", "Greece", "Ireland", "Portugal"))
head(happy5)

happy_scandinavia <-filter(World_Happiness_Data_2018, country %in% c("Denmark", "Iceland", "Sweden", "Finland"))
happy_scandinavia$region="Scandinavia"
View(happy_scandinavia)

happy_persiangulf <-filter(World_Happiness_Data_2018, country %in% c("Bahrain", "Saudi Arabia", "United Arab Emirates", "Kuwait"))
happy_persiangulf$region="PeacefulPersianGulf"
View(happy_persiangulf)

warpersiangulf <-filter(World_Happiness_Data_2018, country %in% c("Iraq", "Syria", "Yemen"))    
warpersiangulf$region="WarPersianGulf"

View(war_persiangulf)
happy_US <- filter(World_Happiness_Data_2018, country=="United States")
happy_US$region="United States"
happy_africa <- filter(World_Happiness_Data_2018, country %in% c("Botswana", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo (Brazzaville)", "Congo (Kinshasa)", "Gabon", "Ghana", "Guinea", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mauritania", "Mauritius", "Mozambique", "Namibia", "Nigeria", "Rwanda", "Senegal", "Sierra Leone", "Somalia", "South Africa"))
happy_africa$region="Sub-saharan Africa"
View(happy_africa)

happy_region <-rbind(happy_scandinavia, happy_persiangulf, happy_US, warpersiangulf)
happy_region_with_africa <-rbind(happy_region, happy_africa)

ggplot(happy_region, aes(x=LogGDPPerCapita, y=Happiness, color=region)) + labs(title="Happiness Score vs. Log GDP Per Capita", x="Log GDP Per Capita", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5)) + geom_point() + scale_color_manual(values=c("Peaceful Persian Gulf: Bahrain, Saudi Arabia, UAE, Kuwait"="#00ba38", "Scandinavia: Denmark, Sweden, Norway, Iceland"="#E69F00", "United States"="#f8766d", "WarPersian Gulf: Syria, Yemen, Iraq, Iran"="#99999"))
  
ggplot(happy_region, aes(x=LogGDPPerCapita, y=Happiness, color=region)) + labs(title="Happiness Score vs. Log GDP Per Capita", x="Log GDP Per Capita", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5)) + geom_point()

ggplot(happy_region_with_africa, aes(x=LogGDPPerCapita, y=Happiness, color=region)) + labs(title="Happiness Score vs. Log GDP Per Capita", x="Log GDP Per Capita", y="Happiness Score") + theme(plot.title = element_text(hjust = 0.5)) + geom_point()



