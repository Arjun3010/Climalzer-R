
# Climatic Changes in World and detailed analysis for India


library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(plotly)
library(scales)

# Temperature Changes in World and India

dataset = read.csv('GlobalLandTemperaturesByCountry.csv')
n = nrow(dataset)

#Removing NA values
dataset[is.na(dataset)] = -990
dataset = dataset[dataset$AverageTemperature != -990,]

#Replacing date by year for easy group by
dataset$dt = as.Date(dataset$dt, "%Y-%m-%d")
data1 = dataset

dataset$dt = as.numeric(format(dataset$dt,'%Y'))
dataset = dataset[dataset$dt > 1949,]
temp = dataset
dataind = dataset[dataset$Country%in%'India',]

# Finding Average Global Temperature
dt = aggregate(dataset[, 2:2], list(dataset$dt), mean)
ggplot(data=dt, aes(x=Group.1, y=x, group=1))+geom_line(colour="green", linetype="solid", size=1) 

# Finding Average Temperature in india
dti = aggregate(dataind[, 2:2], list(dataind$dt), mean)
ggplot(data=dti, aes(x=Group.1, y=x, group=1))+geom_line(colour="blue", linetype="solid", size=1) 


# Heat Map and bar graph year by month
year = as.numeric(format(data1$dt,'%Y'))
data1 = cbind(data1,year)
data1 = data1[data1$year>1949,]
month = month.abb[as.numeric(format(data1$dt,'%m'))]
data1 = cbind(data1,month)

ggplot(data1,aes(year,month,fill=AverageTemperature)) + theme_bw() +
  geom_tile(color = 'white') +
  labs(title="Average Temperature 1950-2013",y ="Average Temperature(celcius)", x = "Year") 


ggplot(data1,aes(year,AverageTemperature,fill=month)) + theme_bw() +
  geom_bar( stat = "identity") +
  labs(title="Year by Month from 1950-2013",y ="Average Temperature(celcius)", x = "Year")


####################################################

#Average Temperature of Major cities in India

dat = read.csv('GlobalLandTemperaturesByMajorCity.csv')
dat[is.na(dat)] = -990
dat = dat[dat$AverageTemperature != -990,]
dat  = dat[dat$Country%in%'India',]
dat$dt = as.Date(dat$dt, "%Y-%m-%d")
year = as.numeric(format(dat$dt,'%Y'))
dat = cbind(dat,year)
dat = dat[dat$year > 1950,]

#Bar graph
ggplot(dat,aes(year,AverageTemperature,fill=City)) + theme_bw() +
  geom_bar( stat = "identity") +
  labs(title="Average Temperature in Major cities 1950-2013",y ="Average Temperature(celcius)", x = "Year") +
  scale_y_discrete()

# Heat Map

ggplot(dat,aes(year,City,fill=AverageTemperature)) + theme_bw() +
  geom_bar( stat = "identity") +
  labs(title="Annual average seasonal Rainfall for each district from 1901 - 2013",y ="Annual rainfall(mm)", x = "Year") +
  scale_y_discrete()



#Average Temperature for Major countries

data2 = temp[temp$Country %in%'Australia',]
t = temp[temp$Country%in%'Canada',]
data2 = rbind(data2,t)
t = temp[temp$Country%in%'Brazil',]
data2 = rbind(data2,t)
t = temp[temp$Country%in%'China',]
data2 = rbind(data2,t)
t = temp[temp$Country%in%'Germany',]
data2 = rbind(data2,t)
t = temp[temp$Country%in%'India',]
data2 = rbind(data2,t)
t = temp[temp$Country%in%'Netherlands',]
data2 = rbind(data2,t)
t = temp[temp$Country%in%'Russia',]
data2 = rbind(data2,t)
t = temp[temp$Country%in%'Switzerland',]
data2 = rbind(data2,t)
t = temp[temp$Country%in%'United Kingdom',]
data2 = rbind(data2,t)
t = temp[temp$Country%in%'United States',]
data2 = rbind(data2,t)

#bar graph
ggplot(data2,aes(dt,AverageTemperature,fill=Country)) + theme_bw() +
  geom_bar( stat = "identity") +
  labs(title="Average Temperature in Major countries 1950-2013",y ="Average Temperature(celcius)", x = "Year") 
  

# Rainfall changes in India

rainfall <- read.csv('rainfall.csv')
rainfall[is.na(rainfall)] = 0

# Provides average rainfall data in India for each district from
# 1901 - 2015. Please hover over the plot for district names.

b <- rainfall %>% select(SUBDIVISION,ANNUAL)
c <- b %>% group_by(SUBDIVISION) %>% summarise(ANNUAL = (mean(ANNUAL)))

ggplotly(
  ggplot(c, aes(SUBDIVISION,ANNUAL)) + geom_bar(fill = 'blue', stat = "identity") + theme_bw() +
  theme(axis.text.x = element_blank()) +
  labs(title="Annual average Rainfall in each district from 1901 - 2015",y ="Annual rainfall(mm)", x = "State")
)


# Provides average rainfall data in each district of India for each month from
# 1901 - 2015. Please hover over the plot for district names.


b <- rainfall %>% select(SUBDIVISION,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)
b <- b %>%  gather(Months,values,JAN:DEC)
x <- unique(b$Months)
b$Months <- factor(b$Months,levels = x)
b <- b %>% group_by(SUBDIVISION,Months) %>% summarise(values = mean(values))

# Bar graph

ggplotly(
  ggplot(b,aes(SUBDIVISION,values,fill=Months)) + theme_bw() +
  geom_bar( stat = "identity") +
  theme(axis.text.x = element_blank()) +
  labs(title="Annual average Rainfall in each district from 1901 - 2015",y ="Annual rainfall(mm)", x = "State") 
)

# Heat Map

ggplotly(
  ggplot(b,aes(SUBDIVISION,Months,fill=values)) + theme_bw() +
    geom_tile(color = "white") +
    theme(axis.text.x = element_blank()) +
    labs(title="Annual average Rainfall in each district from 1901 - 2015",y ="Annual rainfall(mm)", x = "State") +
    scale_y_discrete() 
)

# Provides average rainfall data in each district of India for each season from
# 1901 - 2015. Please hover over the plot for district names.

b <- rainfall %>% select(SUBDIVISION,Jan.Feb,Mar.May,Jun.Sep,Oct.Dec) 
b <- b %>% rename(Spring = Jan.Feb,Summer = Mar.May,Rainy = Jun.Sep,Winter = Oct.Dec)
b <- b %>%  gather(Season,Rainfall,Spring:Winter)
x <- unique(b$Season)
b$Season <- factor(b$Season,levels = x)
b <- b %>% group_by(SUBDIVISION,Season) %>% summarise(Rainfall = mean(Rainfall))

# Bar Graph

ggplotly(
  ggplot(b,aes(SUBDIVISION,Rainfall,fill=Season)) + theme_bw() +
    theme(axis.text.x = element_blank()) +
    geom_bar( stat = "identity") +
    labs(title="Annual average seasonal Rainfall for each district from 1901 - 2015",y ="Annual rainfall(mm)", x = "Year") +
    scale_y_continuous(labels = comma)
)

# Heat Map

ggplotly(
  ggplot(b,aes(SUBDIVISION,Season,fill=Rainfall)) + theme_bw() +
    theme(axis.text.x = element_blank()) +
    geom_tile(color = "white") +
    labs(title="Annual average seasonal Rainfall for each district from 1901 - 2015",y ="Annual rainfall(mm)", x = "Year") +
    scale_y_discrete()
)


################################################################################################

# For average rainfall in india, We cannot use this dataset since 
# based on the region, the percentage of rainfall varies which cannot 
# be calculated using this dataset. Hence, a new data set is introduced.

rainfall_india <- read.csv('rainfall_annual.csv')
rainfall_india[is.na(rainfall_india)] = 0


# Average rainfall in India for each year from 1901 - 2013

b <- rainfall_india %>% select(YEAR,ANN)

# Line Graph

ggplot(b,aes(YEAR,ANN)) + geom_line(color='green') +
  theme_bw()

# Histogram Graph

ggplot(b,aes(YEAR,ANN)) + geom_histogram(stat = 'identity',color='green') +
  theme_bw()

# Average rainfall in India for each year from 1901 - 2013 
# with every month's contribution

b <- rainfall_india %>% select(YEAR,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)
b <- b %>%  gather(Months,Rainfall,JAN:DEC)
x <- unique(b$Months)
b$Months <- factor(b$Months,levels = x)
b <- b %>% group_by(YEAR,Months) %>% summarise(Rainfall = mean(Rainfall))

# Bar Graph

ggplotly(
  ggplot(b,aes(YEAR,Rainfall,fill=Months)) + theme_bw() +
  geom_bar( stat = "identity") +
  labs(title="Annual average Rainfall in India from 1901 - 2013",y ="Annual rainfall(mm)", x = "Year") +
  scale_y_continuous(labels = comma)
)

# Heat Map

ggplotly(
  ggplot(b,aes(YEAR,Months,fill=Rainfall)) + theme_bw() +
    geom_tile(color = 'white') +
    labs(title="Annual average Rainfall in India from 1901 - 2013",y ="Annual rainfall(mm)", x = "Year") +
    scale_y_discrete()
)


# Average rainfall in India for each year from 1901 - 2013 
# with every season's contribution

b <- rainfall_india %>% select(YEAR,Jan.Feb,Mar.May,Jun.Sep,Oct.Dec) 
b <- b %>% rename(Spring = Jan.Feb,Summer = Mar.May,Rainy = Jun.Sep,Winter = Oct.Dec)
b <- b %>%  gather(Season,Rainfall,Spring:Winter)
x <- unique(b$Season)
b$Season <- factor(b$Season,levels = x)
b <- b %>% group_by(YEAR,Season) %>% summarise(Rainfall = mean(Rainfall))

# Bar Graph

ggplotly(
  ggplot(b,aes(YEAR,Rainfall,fill=Season)) + theme_bw() +
    geom_bar( stat = "identity") +
    labs(title="Annual average seasonal Rainfall in India from 1901 - 2013",y ="Annual rainfall(mm)", x = "Year") +
    scale_y_continuous(labels = comma)
)

# Heat Map

ggplotly(
  ggplot(b,aes(YEAR,Season,fill=Rainfall)) + theme_bw() +
    geom_tile(color ='white') +
    labs(title="Annual average seasonal Rainfall in India from 1901 - 2013",y ="Annual rainfall(mm)", x = "Year") +
    scale_y_discrete()
)




# Average Rainfall in each season in India
# from 1901 - 2013

b <- rainfall_india %>% select(Jan.Feb,Mar.May,Jun.Sep,Oct.Dec)
b <- b %>% rename(Spring = Jan.Feb,Summer = Mar.May,Rainy = Jun.Sep,Winter = Oct.Dec)
b <- b %>%  gather(Season,Rainfall,Spring:Winter)
x <- unique(b$Season)
b$Season <- factor(b$Season,levels = x)
b <- b %>% group_by(Season) %>% summarise(Rainfall = mean(Rainfall))

ggplotly(
  ggplot(b,aes(Season,Rainfall)) + theme_bw() +
    geom_bar( stat = "identity",width = 0.3,color='darkgreen',fill='lightgreen') +
    labs(title="Contribution of Rainfall in India for each season from 1901 - 2013",y ="Annual rainfall(mm)", x = "Year") +
    scale_y_continuous(labels = comma)
)