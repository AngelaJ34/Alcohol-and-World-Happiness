Happiness_Alcohol<-na.omit(Happiness_Alcohol)
Happiness_Alcohol$TotalAlcohol<-Happiness_Alcohol$Beer_PerCapita+Happiness_Alcohol$Wine_PerCapita+Happiness_Alcohol$Spirit_PerCapita

happy_countries<-subset(Happiness_Alcohol,Happiness_Alcohol$HappinessScore>6)
middle_happy_countries<-subset(Happiness_Alcohol,Happiness_Alcohol$HappinessScore>5 & Happiness_Alcohol$HappinessScore<6)
not_happy_countries<-subset(Happiness_Alcohol,Happiness_Alcohol$HappinessScore<5)

Happiness_Alcohol$Level_of_Happiness<-"NULL"

happy_countriesMean<-tapply(happy_countries$TotalAlcohol,happy_countries$TotalAlcohol, mean)
middle_happy_countriesMean<-tapply(middle_happy_countries$TotalAlcohol,middle_happy_countries$TotalAlcohol, mean)
not_happy_countriesMean<-tapply(not_happy_countries$TotalAlcohol,not_happy_countries$TotalAlcohol, mean)

Happiness_Alcohol[Happiness_Alcohol$HappinessScore>6,]$Level_of_Happiness<-"High"
Happiness_Alcohol[Happiness_Alcohol$HappinessScore>5 & Happiness_Alcohol$HappinessScore<6,]$Level_of_Happiness<-"Medium"
Happiness_Alcohol[Happiness_Alcohol$HappinessScore<5,]$Level_of_Happiness<-"Low"

Happiness_Alcohol_not_medium<-subset(Happiness_Alcohol,Happiness_Alcohol$Level_of_Happiness!="Medium")
Happiness_Alcohol_not_low<-subset(Happiness_Alcohol, Happiness_Alcohol$Level_of_Happiness!="Low")

meanHappy <-mean(happy_countries$TotalAlcohol)
meanMediumHappy <-mean(middle_happy_countries$TotalAlcohol)
meanNotHappy <-mean(not_happy_countries$TotalAlcohol)

meanMediumHappy
meanNotHappy
meanHappy

sd.MediumHappy<-sd(middle_happy_countries$TotalAlcohol)
sd.NotHappy<-sd(not_happy_countries$TotalAlcohol)
sd.Happy<-sd(happy_countries$TotalAlcohol)

NotHappy.length<-length(not_happy_countries$TotalAlcohol)
Happy.length<-length(happy_countries$TotalAlcohol)
MediumHappy.length<-length(middle_happy_countries$TotalAlcohol)

#below is for nothappy and happy
sd.NotHappy.Happy <- sqrt(sd.Happy^2/Happy.length + sd.Happy^2/NotHappy.length)
zeta <- (meanNotHappy - meanHappy)/sd.NotHappy.Happy
zeta
p<- 1-pnorm(zeta)
p

#below is for happy and mediumhappy
sd.Happy.MediumHappy <- sqrt(sd.Happy^2/Happy.length + sd.Happy^2/MediumHappy.length)
zeta <- (meanMediumHappy - meanHappy)/sd.Happy.MediumHappy
zeta
p<- 1-pnorm(zeta)
p

#below is for nothappy and mediumhappy
sd.NotHappy.MediumHappy <- sqrt(sd.NotHappy^2/NotHappy.length + sd.NotHappy^2/MediumHappy.length)
zeta <- (meanMediumHappy - meanNotHappy)/sd.NotHappy.MediumHappy
zeta
p<- 1-pnorm(zeta)
p
PermutationTestSecond::Permutation(Happiness_Alcohol_not_medium,"Level_of_Happiness","TotalAlcohol",10000,"High","Low")

barplot(table(Happiness_Alcohol$Country),col=c("green","blue","orange","red","yellow"),main = "Happiness Of Country")
barplot(table(Happiness_Alcohol$Region),col=c("green","blue","orange","red","yellow"),main = "Region With Highest Alcohol Consumption")
barplot(table(Happiness_Alcohol$Beer_PerCapita),col =c("green","blue","orange","red","yellow"),main = "Beer_PerCapita")
barplot(table(happy_countries$TotalAlcohol,happy_countries$HappinessScore), main = "Happiness By Region")

barplot(table(happy_countries$Wine_PerCapita, happy_countries$HappinessScore),col =c("green","blue","orange","red","yellow"),main = "Happy_Countries_Wine_PerCapita")
barplot(table(happy_countries$Spirit_PerCapita, happy_countries$Spirit_PerCapita),col =c("green","blue","orange","red","yellow"),main = "Happy_Countries_Spirites_PerCapita")
barplot(table(happy_countries$Beer_PerCapita,happy_countries$HappinessScore),col = c("green","blue","orange","red","yellow"),main = "Happy_Countries_Beer_PerCapita")

barplot(middle_happy_countries$Wine_PerCapita, middle_happy_countries$HappinessScore, main = "middle_happy_countries by Wine_perCapita")
barplot(middle_happy_countries$Beer_PerCapita, middle_happy_countries$HappinessScore, main = "middle_happy_countries by Beer_percapita")
barplot(middle_happy_countries$Spirit_PerCapita, middle_happy_countries$HappinessScore, main = "middle_happy_countries by Spirites_percapita")

barplot(not_happy_countries$Wine_PerCapita, not_happy_countries$Wine_PerCapita, main = "not_happy_countries by wine per-capita")
barplot(not_happy_countries$Spirit_PerCapita, not_happy_countries$Spirit_PerCapita, main = "not_happy_countries by spirit per-capita ")
barplot(not_happy_countries$Beer_PerCapita, not_happy_countries$Beer_PerCapita, main = "not_happy_countries by Beer_per-capita")

barplot(table(not_happy_countries$TotalAlcohol,not_happy_countries$HappinessScore),col =c("green","blue","orange","red","yellow"),main = "not_happy_countries")
barplot(table(middle_happy_countries$TotalAlcohol,middle_happy_countries$HappinessScore),col =c("green","blue","orange","red","yellow"),main = "Middle Happy Countries")

Happiness_Alcohol[1,]
Happiness_Alcohol[2,]
Happiness_Alcohol[3,]
Happiness_Alcohol[4,]
Happiness_Alcohol[5,]
Happiness_Alcohol[111,]
Happiness_Alcohol[84,]
Happiness_Alcohol[115,]
Happiness_Alcohol[113,]
Happiness_Alcohol[102,]
Happiness_Alcohol[97,]

