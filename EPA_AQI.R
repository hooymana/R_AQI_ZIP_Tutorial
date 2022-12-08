rm(list=ls())

library(PWFSLSmoke)
library(ggplot2)
library(zoo)
library(REdaS)
library(zipcodeR)
library(geosphere)
library(ggmap)
library(rstudioapi)

#library(fuzzyjoin)

####IMPORT YOUR PARTICIPANT ZIP CODE DATA HERE!!
#setwd("C:/Users/Andrew Hooyman/Documents/ADIallzip")
#myzip=read.csv("MyZip.csv")

#EPA Air Quality API
#Air Quality for PM2.5

epa_downloadData(year = 2020,parameterCode = "88101",
                 downloadDir = "C:/Users/Andrew Hooyman/Documents/AQIzip",
                 baseUrl = "https://aqs.epa.gov/aqsweb/airdata/")

epa_downloadData(year = 2020,parameterCode = "88502",
                 downloadDir = "C:/Users/Andrew Hooyman/Documents/AQIzip",
                 baseUrl = "https://aqs.epa.gov/aqsweb/airdata/")


epa25=read.csv("C:/Users/Andrew Hooyman/Documents/AQIzip/hourly_88502_2021/hourly_88502_2021.csv")
epa25.agg=aggregate(Sample.Measurement~County.Name,epa25,mean)

#Not all the data we would expect...
table(epa25$State.Name)

#Directory for all sites within each state for the year of 2021
#Site to download data: https://www.epa.gov/outdoor-air-quality-data/download-daily-data
setwd("C:/Users/Andrew Hooyman/Documents/AQI")

#store AQI file names in variable
filenames=list.files()

#Quick visual of random file
temp=read.csv(filenames[15])
ggplot(temp,aes(x=as.Date(Date,"%m/%d/%Y"),y=DAILY_AQI_VALUE))+
  geom_point()+
  geom_line()+
  facet_wrap(~COUNTY)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5))

ggplot(temp,aes(DAILY_AQI_VALUE))+
  geom_histogram()+
  facet_wrap(~COUNTY,scales = 'free_y')

#Preallocate parent data frame
epa.agg=data.frame()
#Create mean AQI and PM25 parent dataframe for all states and counties
for (i in 1:53) {
  #read in first file into temp data frame
  temp=read.csv(filenames[i])
  #Get average AQI data for each county within the state and each site within county based on longitude and latitude
  epa.agg.temp=aggregate(DAILY_AQI_VALUE~COUNTY+STATE+SITE_LATITUDE+SITE_LONGITUDE,temp,mean)
  #Same thing for PM25
  epa.agg.temp$PM25=unlist(aggregate(Daily.Mean.PM2.5.Concentration~COUNTY+STATE+SITE_LATITUDE+SITE_LONGITUDE,temp,mean)[5])
  #row bind state data frame into parent data frame
  epa.agg=rbind(epa.agg,epa.agg.temp)
  #Track progress
  print(c(i,unique(epa.agg.temp$STATE)))
  
}

#Algorithm to add 0 to zip codes with 4 digits
for(i in 1:dim(myzip)[1]){
  if(nchar(myzip$zip5[i])<5){
    myzip$zip5[i]=paste("0",myzip$zip5[i],sep="")
  }
}


#register API key with google cloud/geocode
#Register with a google cloud account to get key: https://cloud.google.com/maps-platform/
#register_google(key = "YOUR KEY")

#Get longitudinal and latitude data
for(i in 1:dim(myzip)[1]){
  #Get data from zipcodeR
  z=zipcodeR::reverse_zipcode(myzip$zip5[i])
  myzip$COUNTY[i]=z$county
  myzip$lat[i]=z$lat
  #Get latitude from geocoder
  myzip$latg[i]=geocode(myzip$zip5[i])[2]
  myzip$lng[i]=z$lng
  #Get longitude from geocoder
  myzip$long[i]=geocode(myzip$zip5[i])[1]
  myzip$medianincome[i]=z$median_household_income

  print(i)
}

#missing zipcoderR
sum(is.na(myzip$lat))
#missing geocode
sum(is.na(unlist(myzip$latg)))

myzip$latg=unlist(myzip$latg)
myzip$long=unlist(myzip$long)

#comparison between zip code and geocoder
ggplot(myzip,aes(lat,latg))+
  geom_point()

###DON'T DO THIS. JOINS DATA BASED ON WORD MATCHING ACROSS STATE AND COUNTY.
# myzip$id=c(1:1397)
# 
# data=stringdist_join(myzip, epa.agg,
#                      by=c('COUNTY',"STATE"), #match based on team
#                      mode='left', #use left join
#                      method = "jw", #use jw distance metric
#                      max_dist=.45, #number of joins is super sensitive to the magnitude of this parameter
#                      distance_col='dist')
# 
# data=data[!duplicated(data$id),]
# sum(is.na(data$PM25))

#Find AQI site that is closest to participant zip and store distance from site, AQI, and PM25 for that participant
#Preallocate data
f=NA
#Not going to be perfect not all legit zip codes are supported in geocoder
for(j in 1:dim(myzip)[1]){

for(i in 1:dim(epa.agg)[1]){
  
  #Geocode lat and long
  lat1=myzip$latg[j]
  lon1=myzip$long[j]
  #AQI site lat and long
  lat2=epa.agg$SITE_LATITUDE[i]
  lon2=epa.agg$SITE_LONGITUDE[i]

  #Provide distance vector from participant zip code to every AQI site in kilometers
  f[i]=distHaversine(c(lon1,lat1),c(lon2,lat2))/1000


  
}
  #Find minimum distance
  ind=which.min(f)
  #skip indexing if index is empty
  if(!is_empty(ind)){
  #store minimum distance
  myzip$sitedist[j]=f[ind]
  #Store closest AQI site AQI
  myzip$AQI[j]=epa.agg$DAILY_AQI_VALUE[ind]
  #store closest AQI site PM25
  myzip$PM25[j]=epa.agg$PM25[ind]
  }else{
  #Added in this else statement so that zip codes with missing lat/long are not given a value
  myzip$sitedist[j]=NA
  #Store closest AQI site AQI
  myzip$AQI[j]=NA
  #store closest AQI site PM25
  myzip$PM25[j]=NA
  }
  #Track progress
  print(j)

}


sum(is.na(myzip$AQI))
hist(myzip$sitedist)
median(myzip$sitedist)
mean(myzip$sitedist)

###Quick comparison between participant AQI and mean participant zip code median income
# ggplot(myzip,aes(y=AQI,x=medianincome))+
#   geom_point()+
#   geom_smooth(method = 'lm')+
#   ylab("Mean AQI of 2021")+
#   xlab("Median Income of Zip Code (Census)")
# 
# summary(lm(AQI~scale(medianincome),myzip))
# sd(myzip$medianincome,na.rm = T)
# mean(myzip$medianincome,na.rm=T)
# (1/.6854)*23905.89 #For every $34878 in zip code median income AQI decrease by 1 point
