###########################################################################################################


library(highcharter)
library(dplyr)
library(bindr)
library(tidyr)
library(viridis)
library(plotly)
library(lubridate)
library(xts)
library(maps)
library(ggmap)
library(gridExtra)
library(hc_legend)
library(tseries)
library(forecast)
library(TSA)
library(prophet)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library("dplyr")
install.packages("chron")
library(chron)
library(plyr)
library(utils)
install.packages("doBy")  
library(doBy)  
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("NLP")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

##############################################################################################################

##load csv in R variable
chicagocrimes20122016 <- read.csv("F:/poonam/Analytics_2017/Chicago_Crimes_2012_to_2017.csv")


##read the dataset
crime <- read.csv("F:/poonam/Analytics_2017/Chicago_Crimes_2012_to_2017.csv")

##Handeling time
crime$Date <- as.POSIXlt(crime$Date,format= "%m/%d/%Y %H:%M")
crime$time <- times(format(crime$Date, "%H:%M:%S" ))
head(crime$time)
time.tag<-chron(times=c("00:00:00","06:00:00","12:00:00","18:00:00","23:59:00"))
time.tag
crime$time.tag <- cut(crime$time, breaks= time.tag,labels=c("00-06","06-12","12-18","18-00"), include.lowest=TRUE)  
table(crime$time.tag)
crime$Date <- as.POSIXlt(strptime(crime$Date,format="%Y-%m-%d"))
head(crime$Date) 
crime$day <- weekdays(crime$Date, abbreviate= TRUE)  
crime$month <- months(crime$Date, abbreviate= TRUE)  

##Primary Crime types
table(crime$Primary.Type)
length(unique(crime$Primary.Type))
crime$crime <- as.character(crime$Primary.Type)  
crime$crime<-ifelse(crime$crime %in% c("CRIM SEXUAL ASSAULT","PROSTITUTION","SEX OFFENSE"),'SEX',crime$crime) 
crime$crime <- ifelse(crime$crime %in% c("MOTOR VEHICLE THEFT"),"MVT",crime$crime)
crime$crime<-ifelse(crime$crime %in% c("GAMBLING","INTERFERE WITH PUBLIC OFFICER","INTERFERENCE WITH PUBLIC OFFICER","INTIMIDATION","LIQUOR LAW VIOLATION","OBSCENITY","NON-CRIMINAL","PUBLIC PEACE VIOLATION","PUBLIC INDECENCY","STALKING","NON-CRIMINAL"),"NONVIO",crime$crime)
crime$crime <- ifelse(crime$crime =="CRIMINAL DAMAGE","DAMAGE",crime$crime)  
crime$crime <- ifelse(crime$crime=="CRIMINAL TRESPASS","TRESPASS",crime$crime) 
crime$crime <- ifelse(crime$crime %in% c("NARCOTICS","OTHER NARCOTIC VIOLATION"),"DRUG",crime$crime)
crime$crime<-ifelse(crime$crime=="DECEPTIVE PRACTICE","FRAUD",crime$crime)
crime$crime<-ifelse(crime$crime %in% c("OTHER OFFENSE","OTHER OFFENSE"),"OTHER",crime$crime)
crime$crime<-ifelse(crime$crime %in% c("KIDNAPPING","WEAPONS VIOLATION","OFFENSE INVOLVING CHILDREN"),"VIO",crime$crime)
table(crime$crime)  
crime$Arrest <- ifelse(as.character(crime$Arrest)=="TRUE",1,0)

##graph for crimes in chicago
qplot(crime$crime,xlab="Crimes in Chicago")+ scale_y_continuous("Number of crimes")  

##graph for crimes by time of day
qplot(crime$time.tag,xlab="Time of day",main="Crimes by time of day")+scale_y_continuous("Number of crimes")

##graph for crime of day
crime$day<-factor(crime$day,levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

##graph for day of week
qplot(crime$day,xlab="Day of week",main="Crimes by day of week")+scale_y_continuous("Number of crimes")

crime$month<-factor(crime$month,levels=c("Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sep","Oct","Nov","Dec"))  

##graph for crimes by month
qplot(crime$month,xlab="Month",main="Crimes by Month")+scale_y_continuous("Number of crimes")  

temp<-aggregate(crime$crime,by=list(crime$crime,crime$time.tag),FUN=length)  


names(temp)<-c("crime","time.tag","count")
##graph for crime of day
ggplot(temp,aes(x=crime,y=factor(time.tag)))+geom_tile(aes(fill=count))+scale_x_discrete("Crime",expand=c(0,0))+scale_y_discrete("Time of day",expand=c(0,-2))+scale_fill_gradient("Number of crimes",low="white",high="steelblue")+theme_bw()+ggtitle("Crimes by time of day")+theme(panel.grid.major=element_line(colour=NA),panel.grid.minor=element_line(colour=NA)) 


temp <- ddply(crime,.(crime, day), summarise, count = length(date))

##graph for crimes by day of week
ggplot(temp, aes(x= crime, y= day, fill= count)) +
  geom_tile(aes(fill= count)) +
  scale_x_discrete("crime", expand = c(0,0)) +
  scale_y_discrete("Day of week", expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes", low = "white", high =
                        "Steelblue") +
  theme_bw() + ggtitle("Crimes by day of week") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor =
          element_line(colour = NA)) 

temp <- summaryBy(Case.Number ~crime + month, data= crime, FUN= length)
names(temp)[3] <- 'count'  

##plot graph for crimes by month for year 2012-2016
ggplot(temp, aes(x= crime, y=month, fill= count)) +
  geom_tile(aes(fill= count)) +
  scale_x_discrete("Crime", expand = c(0,0)) +
  scale_y_discrete("Month", expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes", low = 'White', high = "steelblue") +
  theme_bw() + ggtitle("Crimes by month") + theme(panel.grid.major = element_line
                                                  (colour = NA), panel.grid.minor = element_line(colour = NA)) 


##world cloud working
text <- readLines(file.choose())

docs <- Corpus(VectorSource(text))
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 6,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


thefts <- crime[crime$Primary.Type=="THEFT",] 
## Creating timeseries
thefts_by_Date <- na.omit(thefts) %>% group_by(Date) %>% summarise(Total = n())
thefts_tseries <- xts(thefts_by_Date$Total, order.by=as.POSIXct(by_Date$Date))


## Heat map
c <- read.csv("CCA.csv")
c$Location[c$Location == ''] <- NA
c <- na.omit(c)
c <- c %>% extract(Location, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
c$Longitude <- round(as.numeric(c$Longitude), 2)
c$Latitude <- round(as.numeric(c$Latitude), 2)

##Plot Chicago map
chicago <- get_map(location = 'chicago', zoom = 11)

##Use package ggmap for chicago map
install.packages("ggmap")
ggmap(chicago)

##get latitude,longitude
c <- extract(c, Location, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')

##Plot crime location in Chicago map
locationCrimes <- as.data.frame(table(c$Longitude, c$Latitude))
names(locationCrimes) <- c('long', 'lat', 'Frequency')
locationCrimes$long <- as.numeric(as.character(locationCrimes$long))
locationCrimes$lat <- as.numeric(as.character(locationCrimes$lat))
locationCrimes <- subset(locationCrimes, Frequency > 0)
c <- c %>% extract(Location, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)' if c$Primary.Type="THEFT")
c$Longitude <- round(as.numeric(c$Longitude), 2)
c$Latitude <- round(as.numeric(c$Latitude), 2)

##Plot graph for crime location in Chicago map
ggmap(chicago) + geom_tile(data = locationCrimes, aes(x = long, y = lat, alpha = Frequency),
                           fill = 'red') + theme(axis.title.y = element_blank(), axis.title.x = element_blank())

###############################################################################################################
##cleanse data for 2017 year
chicagocrimes20122016 <- chicagocrimes20122016[chicagocrimes20122016$Year!='2017',]

### Working with Date and Time
chicagocrimes20122016$Day <- factor(day(as.POSIXlt(crimedata$Date, format="%m/%d/%Y %I:%M:%S %p")))

### Working with Date and Time
chicagocrimes20122016$Day <- factor(day(as.POSIXlt(chicagocrimes20122016$Date, format="%m/%d/%Y %I:%M:%S %p")))

chicagocrimes20122016$Month <- factor(month(as.POSIXlt(chicagocrimes20122016$Date, format="%m/%d/%Y %I:%M:%S %p"), label = TRUE))
chicagocrimes20122016$Year <- factor(year(as.POSIXlt(chicagocrimes20122016$Date, format="%m/%d/%Y %I:%M:%S %p")))
chicagocrimes20122016$Weekday <- factor(wday(as.POSIXlt(chicagocrimes20122016$Date, format="%m/%d/%Y %I:%M:%S %p"), label = TRUE))

chicagocrimes20122016$Date <- as.Date(chicagocrimes20122016$Date, "%m/%d/%Y %I:%M:%S %p")

by_Date <- na.omit(chicagocrimes20122016) %>% group_by(Date) %>% summarise(Total = n())

tseries <- xts(by_Date$Total, order.by=as.POSIXct(by_Date$Date))

## Creating timeseries of arrests made
Arrests_by_Date <- na.omit(chicagocrimes20122016[chicagocrimes20122016$Arrest == 'True',]) %>% group_by(Date) %>% summarise(Total = n())

arrests_tseries <- xts(Arrests_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

## BY Location
by_location <- chicagocrimes20122016 %>% group_by(Location.Description) %>% summarise(Total = n()) %>% arrange(desc(Total))

## By Primary Type
by_type <- chicagocrimes20122016 %>% group_by(Primary.Type) %>% summarise(Total = n()) %>% arrange(desc(Total))

## By District
by_district <- chicagocrimes20122016 %>% group_by(District) %>% summarise(Total = n()) %>% arrange(desc(Total))

## By Primary Type
by_ward <- chicagocrimes20122016 %>% group_by(Ward) %>% summarise(Total = n()) %>% arrange(desc(Total))

## By FBI Code
by_fbi <- chicagocrimes20122016 %>% group_by(FBI.Code) %>% summarise(Total = n()) %>% arrange(desc(Total))

## By Arrest
by_arrest <- chicagocrimes20122016 %>% group_by(Arrest) %>% summarise(Total = n()) %>% arrange(desc(Total))

## By Domestic
by_domestic <- chicagocrimes20122016 %>% group_by(Domestic) %>% summarise(Total = n()) %>% arrange(desc(Total))

## By Year
by_year <- chicagocrimes20122016 %>% group_by(Year) %>% summarise(Total = n()) %>% arrange(desc(Total))

## Lat and Long of Crimes
LatLonCounts <- as.data.frame(table(round(chicagocrimes20122016$Longitude,2), round(chicagocrimes20122016$Latitude,2)))
LatLonCounts$Long <- as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat <- as.numeric(as.character(LatLonCounts$Var2))
LatLonCounts2 <- subset(LatLonCounts, Freq > 0)

## Lat and Long of Arrests
arrests_data <- na.omit(chicagocrimes20122016[chicagocrimes20122016$Arrest == 'True',])
LatLonArrestsCounts <- as.data.frame(table(round(arrests_data$Longitude,2), round(arrests_data$Latitude,2)))
LatLonArrestsCounts$Long <- as.numeric(as.character(LatLonArrestsCounts$Var1))
LatLonArrestsCounts$Lat <- as.numeric(as.character(LatLonArrestsCounts$Var2))
LatLonArrestsCounts2 <- subset(LatLonArrestsCounts, Freq > 0)

## graph of crimes and arrest happened with respect to time
hchart(tseries, name = "Crimes") %>% 
  hc_add_series(arrests_tseries, name = "Arrests") %>%
  hc_add_theme(hc_theme_sandsignika()) %>%
  hc_credits(enabled = TRUE, text = "the Chicago Police Department as source", style = list(fontSize = "12px")) %>%
  hc_title(text = "Chicago Crimes and Arrests plot using time") %>%
  hc_legend(enabled = TRUE)

## graph of arrest with tseries happened with respect to time
hchart(arrests_tseries) %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_title(text = "Arrests made in Chicago (2012-2016) plot using tseries") %>%
  hc_credits(enabled = TRUE, text = "City of Chicago Administration and the Chicago Police Department as source", style = list(fontSize = "12px"))

arrests_count <- arrests_data %>% group_by(Year, Month) %>% summarise(Total = n())

arrests <- ggplot(arrests_count, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_gradient2()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("For 2012-2016 duration, Year and Month in which Arrests made")


crime_count <- chicagocrimes20122016 %>% group_by(Year, Month) %>% summarise(Total = n())

crimes <- ggplot(crime_count, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_gradient2()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("For 2012-2016 duration, Year and Month in which Crimes occured")

##plot graph to visualise crime and arrest in 2012-2016 year
grid.arrange(crimes, arrests, ncol = 2)

##plot graph to get top 10 locations with most crimes
hchart(by_location[1:20,], "column", hcaes(x = Location.Description, y = Total, color = Total)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#FDA725", "440152", "#21909C"))) %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_title(text = "Top 20 Locations with most Crimes") %>%
  hc_credits(enabled = TRUE, text = "City of Chicago Administration and the Chicago Police Department as source", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)


streets <- chicagocrimes20122016[chicagocrimes20122016$Location.Description=="STREET",]
## Creating timeseries
streets_by_Date <- na.omit(streets) %>% group_by(Date) %>% summarise(Total = n())
streets_tseries <- xts(streets_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

residence <- chicagocrimes20122016[chicagocrimes20122016$Location.Description=="RESIDENCE",]
## Creating timeseries
residence_by_Date <- na.omit(residence) %>% group_by(Date) %>% summarise(Total = n())
residence_tseries <- xts(residence_by_Date$Total, order.by=as.POSIXct(by_Date$Date))
apartment <- chicagocrimes20122016[chicagocrimes20122016$Location.Description=="APARTMENT",]

## Creating timeseries
apartment_by_Date <- na.omit(apartment) %>% group_by(Date) %>% summarise(Total = n())
apartment_tseries <- xts(apartment_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

sidewalk <- chicagocrimes20122016[chicagocrimes20122016$Location.Description=="SIDEWALK",] 
## Creating timeseries
sidewalk_by_Date <- na.omit(sidewalk) %>% group_by(Date) %>% summarise(Total = n())
sidewalk_tseries <- xts(sidewalk_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

## Plot graph to get crimes location wise
hchart(streets_tseries, name = "Streets") %>% 
  hc_add_series(residence_tseries, name = "Residence") %>% 
  hc_add_series(apartment_tseries, name = "Apartment") %>%
  hc_add_series(sidewalk_tseries, name = "Sidewalk") %>%
  hc_add_theme(hc_theme_economist()) %>%
  hc_credits(enabled = TRUE, text = "City of Chicago Administration and the Chicago Police Department as Source", style = list(fontSize = "12px")) %>%
  hc_title(text = "Crimes in Streets/Residence/Apartment/Sidewalk") %>%
  hc_legend(enabled = TRUE)

## Plot graph for crime types
hchart(by_type, "column", hcaes(Primary.Type, y = Total, color = Total)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Crime Types") %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)


thefts <- chicagocrimes20122016[chicagocrimes20122016$Primary.Type=="THEFT",] 
## Creating timeseries
thefts_by_Date <- na.omit(thefts) %>% group_by(Date) %>% summarise(Total = n())
thefts_tseries <- xts(thefts_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

battery <- chicagocrimes20122016[chicagocrimes20122016$Primary.Type=="BATTERY",] 
## Creating timeseries
battery_by_Date <- na.omit(battery) %>% group_by(Date) %>% summarise(Total = n())
battery_tseries <- xts(battery_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

criminals <- chicagocrimes20122016[chicagocrimes20122016$Primary.Type=="CRIMINAL DAMAGE",]
## Creating timeseries
criminals_by_Date <- na.omit(criminals) %>% group_by(Date) %>% summarise(Total = n())
criminals_tseries <- xts(criminals_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

narcotics <- chicagocrimes20122016[chicagocrimes20122016$Primary.Type=="NARCOTICS",] 
## Creating timeseries
narcotics_by_Date <- na.omit(narcotics) %>% group_by(Date) %>% summarise(Total = n())
narcotics_tseries <- xts(narcotics_by_Date$Total, order.by=as.POSIXct(by_Date$Date))

##plot graph for crimes in year 2012-2016
hchart(thefts_tseries, name = "Thefts") %>% 
  hc_add_series(battery_tseries, name = "Battery") %>% 
  hc_add_series(criminals_tseries, name = "Criminal Damage") %>%
  hc_add_series(narcotics_tseries, name = "Narcotics") %>%
  hc_add_theme(hc_theme_db()) %>%
  hc_credits(enabled = TRUE, text = "City of Chicago Administration and the Chicago Police Department as Source", style = list(fontSize = "12px")) %>%
  hc_title(text = "Crimes in year 2012-2016") %>%
  hc_legend(enabled = TRUE)

homicide <- chicagocrimes20122016[chicagocrimes20122016$Primary.Type=="HOMICIDE",] 

homicide_year <-  homicide %>% group_by(Year) %>% summarise(Total = n())

##plot graph for Homicide rate
hchart(homicide_year, "column", hcaes(Year, Total, color = Year)) %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_title(text = "For the year 2012-2016 Homicide Rate")  %>%
  hc_credits(enabled = TRUE, text = "City of Chicago Administration and the Chicago Police Department as source", style = list(fontSize = "12px"))

homicide_count <- homicide %>% group_by(Year, Month) %>% summarise(Total = n())


##plot homiside crime type count
ggplot(homicide_count, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_color_gradient()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Homicides in Chicago (2012-2016)")

############################################################################################################################
#Model        : Time Series model
#Description  : To check the crime rates for particular location , season and time. 
#Attributes   : ID,Date,Primary crime type, Description, Arrest, Domestic, Location description, Latitude, Longitude, Location, Year,
#               X-Cordinate, Y-Cordinate
#
############################################################################################################################

chicagocrimes20122016 <- chicagocrimes20122016[chicagocrimes20122016$Year %in% c('2012', '2013', '2014', '2015','2016'), c('Date', 'ID')]

## Creating timeseries
chicagocrimes20122016$Date <- as.Date(chicagocrimes20122016$Date, "%m/%d/%Y %I:%M:%S %p")
by_Date <- na.omit(chicagocrimes20122016) %>% group_by(Date) %>% summarise(Total = n())
tseries <- xts(by_Date$Total, order.by=as.POSIXct(by_Date$Date))

## get log value for time series models
df <- chicagocrimes20122016 %>% group_by(Date) %>% summarise(y = n()) %>% mutate(y = log(y))

names(df) <- c("ds", "y")
df$ds <- factor(df$ds)

##verify for stationarity of data
tempdata <- df$y
crime_data=ts(df$y, start= c(2012,1),end= c(2016,4),frequency=5)
summary(crime_data)
plot(crime_data)

dif_crime_data <-diff(crime_data)
plot(dif_crime_data)

##plot ar model
yearlyar <-arima(x=dif_crime_data, order = c(2,0,0))
yearlyar

##residual analysis for ar model
qqnorm(yearlyar$residuals)
qqline(yearlyar$residuals, col=2)
Box.test(yearlyar$residuals,lag=6,type='Ljung')

##calculating MA values
yearlyMA <-arima(x=dif_crime_data, order = c(0,0,2))
yearlyMA

##residual analysis for ma model
qqnorm(yearlyMA$residuals)
qqline(yearlyMA$residuals, col=2)
Box.test(yearlyMA$residuals,lag=6,type='Ljung')

##calculating Arima
yearlyarima <-arima(coredata(crime_data),order = c(0,1,2))
yearlyarima

##residual for arima model
tsdiag(yearlyarima)
qqnorm(yearlyarima$residuals)
qqline(yearlyarima$residuals, col=2)
Box.test(yearlyarima$residuals,lag=6,type='Ljung')

##calculating ARMA(2,2)
yearlyarma <-arima(x=dif_crime_data,order = c(2,0,2),,include.mean=T, method = 'ML')
yearlyarma

##QQplot for arma model
qqnorm(yearlyarma$residuals)
qqline(yearlyarma$residuals, col=2)
Box.test(yearlyarma$residuals,lag=6,type='Ljung')

##Predict the values for AR Model
ar_predict=predict(yearlyar, n.ahead=30,se.fit=T)
ar_predict

##Predict the values for MA Model
ma_predict=predict(yearlyMA, n.ahead=30,se.fit=T)
ma_predict

##Predict the values for ARIMA Model
arima_predict=predict(yearlyarima, n.ahead=30,se.fit=T)
arima_predict

##Predict the values for ARMA Model
arma_predict=predict(yearlyarma, n.ahead=30,se.fit=T)
arma_predict

###########################################################################################################
### Fitting the model

m <- prophet(df)
future <- make_future_dataframe(m, periods = 365 * 4)
head(future)
tail(future)

## Forecasting by using predict method
forecast <- predict(m, future)

##Basic plotting of the forecast
plot(m, forecast)

fit <- arima(crime_data, order=c(p, d, q))

##predict next 5 observations
library(forecast)
forecast(fit, 5)
plot(forecast(fit, 5))