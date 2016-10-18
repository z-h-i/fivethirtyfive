library(tmap)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)


# grab census map data and Albers project it
us <- readOGR(dsn = ".", layer = "OGRGeoJSON")
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 
                              +lon_0=-100 +x_0=0 +y_0=0 
                              +a=6370997 +b=6370997 +units=m 
                              +no_defs"))


# fix alaska and hawaii
us_aea@data$id <- rownames(us_aea@data)
alaska <- us_aea[us_aea$STATEFP == "02", ]
alaska <- elide(alaska, rotate = -45)
alaska <- elide(alaska, scale = max(apply(bbox(alaska), 1, diff)) / 2.72)
alaska <- elide(alaska, shift = c(-2360000, -2300000))
proj4string(alaska) <- proj4string(us_aea)

hawaii <- us_aea[us_aea$STATEFP == "15", ]
hawaii <- elide(hawaii, rotate = -35)
hawaii <- elide(hawaii, shift = c(5100000, -1300000))
proj4string(hawaii) <- proj4string(us_aea)


# get rid of virgin islands, marianna islands,
# guam, puerto rico, etc. and throw in new alaska and hawaii
us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"), ]
us_aea <- rbind(us_aea, alaska, hawaii)


# read election data and add Trump
data_name <- "clinton_chance_sep19_2016.csv"
df <- read.csv(data_name)
colnames(df) <- c("State", "Hillary")
df$Chance <- 50 - df$Hillary


# link state names to join shape and election data
us_aea@data$STUSPS <- as.character(us_aea@data$STUSPS)
us_aea@data$NAME <- tolower(as.character(us_aea@data$NAME))
df$State <- tolower(as.character(df$State))
us_aea@data <- us_aea@data[order(shapefile@data$NAME), ]
df <- df[order(df$State), ]
data <- append_data(us_aea, df, key.shp = "NAME", key.data = "State")


# colors and text
gradient <- c("#33A2E9", "#55B1ED", "#77C1F0", "#99D1F4", "#BBE0F8", "#DDF0FB",
              "#FEE7E4", "#FCA9A1", "#FC9D93", "#FB9185", 
              "#FB8478", "#FA786B", "#FA6C5D", "#fa6c62")


# quick plot
tm_shape(data) +
  tm_fill("Chance", palette = gradient, style = "cat", legend.show = FALSE) + 
  tm_borders(col = "#FFFFFF") +
  tm_text("STUSPS", size = .7)




