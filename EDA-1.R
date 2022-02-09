library(dplyr)
library(ggplot2)
library(forecast)
library(datasets)
library(lubridate)
library(rpart)
library(randomForest)
library(readr)

arbuthnot.df <- read.csv("arbuthnot.csv")
arbuthnot.df
# What command would you use to extract just the count of girls baptized
sum(arbuthnot.df$girls)

# Is there an apparent trend in the number of girls baptized over the years?
ggplot(data=arbuthnot.df, mapping= aes(x = year, y= girls)) + geom_line() +
  ggtitle("Girl births by year") +xlab("Year") + ylab("Births")
# We see the number of girls baptized is increasing over the years with a constant increase from 1660s to 1700s 
# We can observe the number dropped low from the years 1640's to 1660's primarily due to political upheavals (1) (changes in the ownership of the real estate, 
# and hence in the composition of the governing class) including the Bourgeois Revolution and the Civil War (2)

# Now, make a plot of the proportion of boys over time. What do you see?
ggplot(data=arbuthnot.df, mapping= aes(x = year, y= boys)) + 
  geom_line() +ggtitle("Boy Births by Year") +xlab("Year") + ylab("Births")
# We see a very similar trends for boys baptized over the years as of girls because 
# the Civil War had a detrimental effect on child-births in general

# In what year did we see the most total number of births in the London?
data1<-arbuthnot.df
data1$TotalBirth<- data1$boys+data1$girls
data1$year[data1$TotalBirth==max(data1$TotalBirth)]

## Attitude data
data("attitude")
View(attitude)

summary(attitude)

#Produce a scatterplot matrix of the variables in the attitude dataset. 
#What seems to be most correlated with the overall rating?
pairs(attitude)
cor(attitude)
# Complaints seem to be most correlated with attutude

#Produce a scatterplot of rating (on the y-axis) vs. learning (on the x-axis). Add a title to the plot
ggplot(data=attitude, mapping= aes(x = learning, y= rating)) + geom_point() 
+ggtitle("Learning vs Rating") +xlab("Learning") + ylab("Rating")


#Produce 2 side-by-side histograms, one for rating and one for learning
par(mfrow=c(1,2))
hist(attitude$rating)
hist(attitude$learning)

#mtcars
#Create a box plot using ggplot showing the range of values of 1/4 mile time (qsec) for each tansmission type (am, 0 = automatic, 1 = manual) from the mtcars data set. 
#Use "Transmission Type" and \1/4 Mile Time" for your y- and x-axes respectively. Also, add the title to your graph.
mtcars[mtcars$am==1,]$am <- "Manual"
mtcars[mtcars$am==0,]$am <- "Automatic"
ggplot(data=mtcars, mapping= aes(x = qsec, y= am)) + geom_boxplot()+
  ggtitle("1/4 mile vs Tranmission Type ") +xlab("1/4 Mile") + ylab("Transmission Type")

# Create a bar graph using ggplot, that shows the number of each carb type in mtcars
mtcars %>%   ggplot(aes(x = carb,)) + geom_bar() + ggtitle("Frequency by Carb")

#Next show a stacked bar graph using ggplot of the number of each gear type and how they are further divided out by cyl
df1<-mtcars %>%
  group_by(gear,cyl)%>% summarise(n())
names(df1)[names(df1)=="n()"] <-"count1"
df1$cyl <- as.factor(df1$cyl)

ggplot(data=df1, mapping= aes(fill=cyl, x=gear,y=count1)) + geom_bar(position="stack",stat="identity")+
  ggtitle("Question 6d") +xlab("Gear") + ylab("Count")

# Draw a scatter plot using ggplot showing the relationship between wt and mpg

ggplot(data=mtcars, mapping= aes(x = wt, y= mpg)) + geom_point()
+ggtitle("Weight vs MPG") +xlab("Weight") + ylab("MPG")

#Draw a scatter plot to investigate the relationship between "disp" and "mpg"
ggplot(data=mtcars, mapping= aes(x = disp, y= mpg)) + geom_point()+
  ggtitle("Displacement vs MPG") +xlab("Displacement") + ylab("MPG")

#Create a scatter plot that shows the relationship between various car weights (wt), 
# miles per gallon (mpg) and engine cylinders (cyl)
ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, col = cyl)) + 
  geom_point(shape = 18, size = 3) +
  ggtitle("Miles per Gallon against Weight") + xlab("Weight") + 
  ylab("Miles Per Gallon")

# Using the solution from part (g), create a new plot using shapes to differentiate the various engine cylinders
mtcars$cyl  <-as.factor(mtcars$cyl)

ggplot(data = mtcars, mapping= aes(x = wt, y = mpg, group = cyl)) + 
  geom_point(aes(shape =cyl, col=cyl, size = 2)) +
  ggtitle("Miles per Gallon against Weight") + xlab("Weight") + 
  ylab("Miles Per Gallon")

# Gapminder
gapminder.df <- read.csv("gapminder.csv")

#Unique countries represented per continent
uni_countries <- distinct(gapminder.df,continent,country)
uni_countries %>% group_by(continent) %>%summarise(n=n())

#)European nation with the lowest GDP per capita in 1997
Eur_gdp <- filter(gapminder.df,year=="1997",continent=="Europe") %>% 
  arrange(gdpPercap) %>% 
  head(.,1)
Eur_gdp

# Average life expectancy across each continent in 1980's
Avg_lexp <- filter(gapminder.df,between(year,1980,1989)) %>%
  group_by(continent) %>%
  summarise_at(vars(lifeExp),list(Avg_lifeExp=mean))
Avg_lexp

#) Top 5 countries with the highest GDP's overall all the years
top5_c <- group_by(gapminder.df,country) %>%
  summarise_at(vars(gdpPercap),list(totalgdp=sum)) %>%
  arrange(desc(totalgdp)) %>%
  head(.,5)
top5_c

#) Countries and years with at least 80 years of life expectancies
life80 <- select(gapminder.df,country,lifeExp,year) %>%
  filter(lifeExp >= "80")
life80

# Hflights
library("hflights")
hflights=as.data.frame(hflights)

#First 20 instances in the data
head(hflights,20)

# All flights on January 1st
jan1 <- filter(hflights,Month == 1,DayofMonth == 1) %>%
  head(.,10)
jan1

#) Data related to American or United Airlines
Am_uni <- filter(hflights,UniqueCarrier=="AA"|UniqueCarrier=="UA") %>%
  head(.,10)
Am_uni

#)Subset of data with only variables "Year,Month,DayofMonth" and any other variables with "Taxi" or "Delay" in them
subset_hflights <- select(hflights,Year,Month,DayofMonth | contains("Taxi") | contains("Delay")) %>%
  head(.,10)
subset_hflights

#) Subset of the dataset with variables "Departure Time","Arrivals Time" and "Flight Number"
subset_hflights2 <- select(hflights,DepTime,ArrTime,FlightNum) %>%
  head(.,10)
subset_hflights2

#) All aircrafts carriers with departure time delay more than 60 mins
departure60 <- select(hflights,UniqueCarrier,DepDelay)  %>%
  filter(DepDelay >= 60)
dep60_carriers <- distinct(departure60,UniqueCarrier)
dep60_carriers

#) Carriers sorted on their departure delays
Dep_delays <- select(hflights,UniqueCarrier,DepDelay) %>%
  na.omit(.) %>%
  group_by(UniqueCarrier) %>%
  summarise_at(vars(DepDelay),list(Avg_delay=mean)) %>%
  arrange(desc(Avg_delay))
Dep_delays







