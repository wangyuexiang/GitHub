##########
# ggmap
##########
library(ggmap)

geocode("New York")
#myLocation <- c(lon = 1, lat = 1)
#myMap<-get_map(location=myLocation,source="stamen", maptype="watercolor", crop=FALSE)

myLocation<-"Paris"
myMap<-get_map(location=myLocation,source="google",maptype="roadmap",zoom=10)

myLocation<-"France"
myMap<-get_map(location=myLocation,source="google",maptype="roadmap",zoom=6)

ggmap(myMap)+
geom_point(aes(x = Lng, y = Lat, size=1),
           data = na.omit(gares), alpha = .5, color="darkred")



