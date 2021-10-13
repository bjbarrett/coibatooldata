library(mapview)
library(sf) 
library(rgdal)

setwd("~/Dropbox/Coiba Tool Images/GPX/")


w201707201901 <- readOGR(dsn = "~/Dropbox/coibatooldata/data/gpx/Tool Use Sites July 2017 Jan 2018.GPX", layer="waypoints")

w201803 <- readOGR(dsn = "~/Dropbox/coibatooldata/data/gpx/Tool Use Sites Mar 2018.GPX", layer="waypoints")
w201807 <- readOGR(dsn = "~/Dropbox/coibatooldata/data/gpx/Tool Use Site July 2018.GPX", layer="waypoints")
w201901 <- readOGR(dsn = "~/Dropbox/coibatooldata/data/gpx/Tool Use Sites Jan 2019.GPX", layer="waypoints")
w201903 <- readOGR(dsn = "~/Dropbox/coibatooldata/data/gpx/Tool Sites Mar 2019.GPX", layer="waypoints")
w201903 <- readOGR(dsn = "~/Dropbox/coibatooldata/data/gpx/Tool Sites Mar 2019.GPX", layer="waypoints")

mapview(w201707201901 , col.region="red") + mapview(w201803 , col.region="orange") + mapview(w201807 , col.region="yellow") + mapview(w201901 , col.region="green") + mapview(w201903 , col.region="blue")

mapview(w201803)
w201803$name
str(w201903)