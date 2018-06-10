
# From SiouJia Chen
library(leaflet)
library(htmltools)

library(RColorBrewer)
library(data.table)
library(maptools)
library(rgdal)
library(geosphere)
library(rgeos)
library(ggplot2)

library(sf)
library(htmlwidgets)

hos <- fread("Github/hospitalmap/Hosipital_Address.csv")
head(hos)
setnames(hos,c("twd_97_x","twd_97_y"),c("lon","lat"))
hos <- hos[,.(type_new, lon, lat)]

hos_xy <- st_as_sf(x=hos, coords=c("lon","lat"), crs=3826)
hos_xy<-st_transform(x=hos_xy, crs=4326)

hos[,long:=st_coordinates(hos_xy)[,1]]
hos[,lat:=st_coordinates(hos_xy)[,2]]
hos <- hos[,.(type_new, long, lat)]


# define data 
df.20 <- hos[1:199,]

# define "getColor"
getColor <- function(df.20) {
  sapply(df.20$type_new, function(type_new) {
    if(type_new == 1) {
      "green"
    } else if(type_new  == 2) {
      "orange"
    } else {
      "red"
    } })
}
# define "icons"
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df.20)
)


wgs.84 <- CRSargs(CRS("+init=epsg:4326"))
twd.97 <- CRSargs(CRS("+init=epsg:3826"))
TWD97.TM2.zone.119 <- CRSargs(CRS("+init=epsg:3825"))
TWD97.TM2.zone.121 <- CRSargs(CRS("+init=epsg:3826"))
Town <- st_read("population.SHP",options = "ENCODING=big5",crs=3826)

# turn wgs84
Town <-  st_transform(Town,crs=4326)
head(Town)

# color range (P_CNT_range need to be adjust)
bins <- c(0, 10000, 30000, 60000, 90000,120000,150000,180000, 210000,Inf)
pal <- colorBin("BuGn", domain = Town$P_CNT, bins = bins)


# add label custom info
labels <- sprintf(
  "<strong>%s<strong>%s</strong><br/>%g",
  Town$COUNTY, Town$TOWN,Town$P_CNT
) %>% lapply(htmltools::HTML)

x = read_excel("Hosipital_Address.xls")

pop <- sprintf(
  "<strong>%s</strong><br/><center>%s</center>",
  x$機構名稱, x$type
) %>% lapply(htmltools::HTML)

# intergate label custom info
m2 <- leaflet(data=Town) %>% addPolygons(
  fillColor = ~pal(P_CNT),
  weight = 1,
  opacity = 0.6,
  color = "white",
  dashArray = "0.8",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 3,
    color = "#666",
    dashArray = "0.8",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) 

m3 <- m2 %>% addTiles() %>%
  addLegend(pal = pal, values = ~P_CNT, opacity = 0.7, title = NULL,position = "bottomright") %>% 
  addTiles() %>% 
  addAwesomeMarkers(data=df.20,lng=~long,lat=~lat,icon=icons,clusterOptions=markerClusterOptions(),popup = pop )
saveWidget(m3, file='map-123.html', selfcontained=F)
