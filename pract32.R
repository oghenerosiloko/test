 mydata <- read.csv( file.choose())
mydata
nrow(mydata)
ncol(mydata)
head(mydata, n=10)
tail(mydata, n=10)
str(mydata)
summary(mydata)
head(mydata$carat)
max(mydata$carat)
min(mydata$carat)
mean(mydata$carat)
rm(mydata)
mydata<-read.csv(choose.files())
mydata
head(mydata)
tail(mydata)
str(mydata)
ncol(mydata)
nrow(mydata)
mydata$Birth.rate
summary(mydata$Birth.rate)
mydata$Country.Name
mydata[1:10,]
mydata[c(120:130),]
max(mydata$Birth.rate)
round(mydata$Birth.rate*mydata$Internet.users, 2)
mydata[,c(1,3), drop=F]
mydata$birtintern<- round(mydata$Birth.rate*mydata$Internet.users, 2)
head(mydata)
mydata$birtintern<-NULL
head(mydata)
# Filtering data
mydata[mydata$Birth.rate>40,]
mydata[mydata$Birth.rate<20,]
mydata[mydata$Income.Group == "High income",]
mydata[mydata$Birth.rate>40 & mydata$Income.Group == "Low income",]
summary(mydata[mydata$Income.Group == "Low income",])
mydata[mydata$Country.Name== "Ghana",]
mydata[mydata$Country.Code== "USA",]
mydata[mydata$Internet.users >60,]
mydata[mydata$Country.Name== "China",]
mydata[mydata$Internet.users<50 & mydata$Income.Group =="High income",]


#Introduction to Qplot------------------


qplot(data=mydata, x=Internet.users)
qplot(data=mydata, x=Income.Group, y= Birth.rate, size=I(3), colour=I("purple"))
qplot(data=mydata, x=Income.Group, y= Birth.rate, geom = "boxplot", colour=I("purple"))



#visualizing with qplot......................
qplot(data=mydata, x= Internet.users, y= Income.Group, colour= Birth.rate, 
        size=I(3))

qplot(data=mydata, x= Income.Group, y= Internet.users, colour= Birth.rate, 
      size=I(3))

qplot(data=mydata, x= Income.Group, y= Birth.rate, colour= Internet.users, 
      size=I(3))

qplot(data=mydata, x= Internet.users, y= Birth.rate, colour= Income.Group, 
      size=I(3))


# creating dataframe----------------------


newdata<- data.frame(Codes_2012_Dataset, Countries_2012_Dataset, Regions_2012_Dataset)
head(newdata)
tail(newdata)
newdata$Regions_2012_Dataset == "Africa"

newdata<- data.frame(Countries= Countries_2012_Dataset, Code= Codes_2012_Dataset ,
                    Regions= Regions_2012_Dataset)
head(newdata)
str(newdata)
summary(newdata)
tail(newdata)
summary(newdata)
summary(mydata)
newdata=NULL
newdata<- data.frame(Codes_2012_Dataset, Countries_2012_Dataset, 
                     Regions_2012_Dataset)
colnames(newdata)<- c("Code", "Countries","Regions")

head(newdata)
head(mydata)




#mergaing data frames---------

wholedata<- merge(mydata, newdata, by.x= "Country.Code", by.y= "Code")

head(wholedata)
wholedata$Countries=NULL
head(wholedata)

qplot(data = wholedata, x= Income.Group, y= Birth.rate,
      colour= Regions, size=I(4))

qplot(data = wholedata, x= Birth.rate, y= Internet.users,
      colour= Regions, size=I(4))

#How TO CREATE SHAPES IN QPLOT--------

qplot(data = wholedata, x= Birth.rate, y= Internet.users,
      colour= Regions, size=I(4), shape=I(17))

#HOW TO TRANSPARENCY
qplot(data = wholedata, x= Birth.rate, y= Internet.users,
      colour= Regions, size=I(4), shape=I(17), alpha=I(0.4))

#How to add title to your plots

qplot(data = wholedata, x= Birth.rate, y= Internet.users,
      colour= Regions, size=I(4), shape=I(17), alpha=I(0.4),
      main=" Internet Users Vs Birth Rate")








#HOMEWORK-----------------
Homework<- read.csv(choose.files())
Homework
head(Homework)
tail(Homework)
summary(Homework)

expdata<- data.frame(Country_Code, Life_Expectancy_At_Birth_1960, 
                     Life_Expectancy_At_Birth_2013)
expdata
head(expdata)
colnames(expdata)<- c("Code", "Life_Exp@1960", "Life_Exp@2013")
head(expdata)
head(Homework)
fil<-Homework$Year=="1960"
Homework[fil,]
Homework1960<-Homework[fil,]

head(Homework1960)
data1960<-merge(Homework1960, expdata, by.x = "Country.Code", by.y= "Code")
head(data1960)
data1960$`Life_Exp@2013`=NULL
head(data1960)
summary(data1960)
tail(data1960)

filter1<-Homework$Year=="2013"
Homework[filter1,]
Homework2013<-Homework[filter1,]
head(Homework2013)
data2013<- merge(Homework2013, expdata, by.x ="Country.Code", by.y= "Code")
head(data2013)
data2013$`Life_Exp@1960`=NULL
head(data2013)
head(data1960)
colnames(data1960)<-c("Country.Code", "Country.Name",
                      "Region", "Year", "Fertility.Rate", "Life.Expectancy1960")

head(data1960)

colnames(data2013)<-c("Country.Code", "Country.Name",

                          "Region", "Year", "Fertility.Rate", "Life.Expectancy2013")

head(data2013)
#the scatter plot for the two years--------

qplot(data= data1960, x= Fertility.Rate, y= Life.Expectancy1960, size=I(3), 
      colour=Region, shape=I(17), main= "Life Expectancy Rate Vs Fertility Rate 1960")



qplot(data= data2013, x= Fertility.Rate, y= Life.Expectancy2013, size=I(3), 
      colour=Region, shape=I(17), main= "Life Expectancy Rate Vs Fertility Rate 2013")




#ADVANCED VISUAZLIAZTION

movies<-read.csv(choose.files())
head(movies)
tail(movies)
ncol(movies)
nrow(movies)
str(movies)
summary(movies)
is.factor(movies$Year.of.release)

colnames(movies)
colnames(movies)<-c("Film","Genre","Critic.Ratings","Audience.Ratings","Budget","Year")
colnames(movies)
head(movies)
movies$Genre<-factor(movies$Genre)
movies$Year<-factor(movies$Year)
summary(movies)
str(movies)

#Visualiazation with GGplot
library(ggplot2)
ggplot(data=movies, aes(x=Critic.Ratings, y= Audience.Ratings))

#adding Geometry

ggplot(data=movies, aes(x=Critic.Ratings, y= Audience.Ratings, 
       colour= Genre, size= Budget))+geom_point()

#Plotting with Layers
l<- ggplot(data=movies, aes(x=Critic.Ratings, y= Audience.Ratings, 
                            colour= Genre, size= Budget))

#adding geom point to your plots
l + geom_point()
l+ geom_line()+geom_point()
#the size of the lines or points can be reduced or increased for better visualization
l+ geom_line(size=1)+geom_point(size=1)
#overriding aesthestics
p<- ggplot(data=movies, aes(x=Critic.Ratings, y= Audience.Ratings, 
                            colour= Genre, size=I(3)))

p+ geom_point()
p+ geom_point(aes(colour= Budget))
p+geom_point(aes(x=Budget))


#the axis label are still retained afther overidden, so we change that by xlab funtion

p+geom_point(aes(x=Budget)) +xlab("Budget in Million$$") +ylab("Audience Ratings")



#Making Histogram
t<-ggplot(data=movies, aes(x=Budget))
t+ geom_histogram()
#Add Colour
t+ geom_histogram(aes(fill=Genre), binwidth = 12)
#Adding a border
t+ geom_histogram(aes(fill=Genre), colour="Black", binwidth = 12)

