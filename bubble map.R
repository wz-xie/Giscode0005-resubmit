mydata <- read.csv("/Users/apple/Desktop/gisdata.csv")
library(maptools)   # A package for building maps
library(ggplot2)    # To have ggplot2 graphic interface
library(dplyr)       # To manipulate data
library(graphics) 
#read the shp. ducument
map_data <- readShapePoly("/Users/apple/Desktop/bkcasa0005/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
df_map <- map_data@data[,c('GSS_CODE', 'NAME')]
df_map$id <- as.character(0:32)
head(df_map)
latitude_longitude <- fortify(map_data)
head(latitude_longitude)
latitude_longitude <- latitude_longitude[, c('long', 'lat', 'id','group')] %>% left_join(., df_map, by = 'id')
latitude_longitude <- tbl_df(latitude_longitude)
#choose the data that we will use
group_id <- group_by(.data = latitude_longitude, id)
#calculate the latitude and longitude of center position for each borough
center <- function(x) mean(range(x))
borough_location <- summarise(.data = group_id, latitude = center(lat), longitude = center(long))
head(borough_location)
head(mydata)
borough_Info <- borough_location %>% left_join(., df_map, by = 'id') 
head(borough_Info)
names(mydata)[2]<-c("GSS_CODE")
borough_Info <- borough_Info %>% left_join(., mydata, by = 'GSS_CODE')
latitude_longitude <- latitude_longitude %>% left_join(., borough_Info, by = 'GSS_CODE')
head(latitude_longitude)
head(borough_Info)
#using the center position,independent variables data and crime data map the bubble map and heat map together
ggplot()+
  geom_polygon(data = latitude_longitude, aes(x = long, y = lat,  group = group,fill = BURGLARY.ROBBERY.THEFT), colour = "grey40")+
  scale_fill_gradient(low = 'white', high = 'red')+
  geom_point(mapping = aes(x = longitude, y = latitude, size= afforabel.housing),data = borough_Info,fill="black",
             color="black",alpha=0.6,shape=21)+
  geom_text(mapping = aes(x = longitude, y = latitude, label = NAME), data = borough_Info, colour = 'black',size=3)+
  scale_size_continuous(range=c(0.5,8),name="afforabel.housing")+
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = 'right')
#base on this funtion,change the data can get diffreent map.

  