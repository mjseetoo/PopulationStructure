#code https://www.r-bloggers.com/2018/08/how-to-quickly-enrich-a-map-with-natural-and-anthropic-details/
#La parte de "Add elevation details"

setwd("C:\Users\Savydeviljho\Desktop\Cartogratree Development 2022\Population structure layer CartograPlant\Population structure layer CartograPlant")

#takes file and returns read.table value
createPopulation<- function(populationFile){
  population<-read.table(populationFile)
  return(population)
}

#takes file and returns read.csv value
createLocation <- function(locationFile){
  location<-read.csv(locationFile)
  return(location)
}

#hard coded version
#location<-read.csv("TGDR372_Plant_Accession_Populus_trichocarpa_0.csv")
location<-createLocation("TGDR372_Plant_Accession_Populus_trichocarpa_0.csv")
location2<-location[,c(1,4,5)]

#creating latitude and longitude columns
latCol<-location[,c(4)]
longCol<-location[,c(5)]

#finding max and min values of the latitude and longitude columns to give a range for the 
latitude<-max(latCol$col1) + 10
longitude<-min(longCol$col1) + 10 


#hard coded version
#population<-read.table("Panel1assignedPopDAPC.txt")
population<-createPopulation("Panel1assignedPopDAPC.txt")
population2<-population[,c(2,4)]

colnames(population2)<-c("Plant.Identifier","Population")
dim(population2)
locpop<-merge(population2,location2)

locpop$Population<-as.character(locpop$Population)

#install.packages("raster")
library(maps)
library(raster)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("mapdata")
library(mapdata)
library(ggplot2)
library(maps)
#install.packages("ggrepel")
library(ggrepel)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("ggspatial")
library(ggspatial)

mapa_mundo <- map_data("world")

dem.raster <- getData("SRTM", lat = latitude, lon = longitude, download = TRUE)#needs changed, 
dem.m  <-  rasterToPoints(dem.raster)
dem.df <-  data.frame(dem.m)
colnames(dem.df) = c("lon", "lat", "alt")

options(scipen = 999) # para evitar la anotaci??n cient??fica 



#altitud en escala continua de n valores: scale_fill_gradientn
mapa_mundo %>%
  ggplot() +
  geom_tile(data = dem.df, aes(lon, lat, fill = alt), alpha = .80) +
  scale_fill_gradientn(colours = terrain.colors(200)) +
  geom_polygon(aes( x= long, y = lat, group = group),
               fill = NA,
               color = "black") +
  geom_point(data= locpop, 
             aes(x= Longitude, y = Latitude, color = Population), 
             stroke = F) +
  coord_fixed(xlim= c(-153.61, -118.01),
              ylim= c(37.43,62.03),
              ratio= 1.1)+
  scale_color_manual(values = c( "green", "blue", "red"), name = " ") + #needs changed
  scale_shape_discrete(solid=T)+
  annotation_scale() +
  annotation_north_arrow(location='tr')

theme_map()



#install.packages("shapefiles")
library(shapefiles)
#install.packages("PBSmapping")
library(PBSmapping)
#install.packages("adehabitatHR")
library(adehabitatHR)
#install.packages("maptools")
library(maptools)
library(maps)
library(rgdal)
#install.packages("igraph")
library(igraph)

locpop2<-locpop[,c(3,4)]
locpopPoints<-SpatialPoints(locpop2) #convert locpop2 to spatial points
pop<-locpop[,c(1,2)]#include the plant identifier and the population as data 

obj<-SpatialPointsDataFrame(coords=locpop2,data=pop) 
dim(obj)
head(obj)
str(obj)
writePointsShape(obj,"Panel1PopulationStructure.shp")

