# Building a map of our sampling area for the SFRTP river cooters in the Santa Fe River, FL

# Libraries needed
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("maps")
library("tools")
library("dplyr")
library("stringr")
library("ggrepel")
library("ggspatial")
library("sp")
library("rgeos")
library("lwgeom")
library("cowplot")


# Map of US states
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))       

# Reading in the counties shape file
county<-st_read("counties/cntbnd_sep15.shp")

# Separating out the counties we are interested in
alachua<-county %>% filter(NAME == "ALACHUA")
gil<-county %>% filter(NAME=="GILCHRIST")
columbia<-county %>% filter(NAME=="COLUMBIA")

# Reading in the rivers shape file
rivers <- st_read(
  "Waterways_Florida.shp")

# Identifying and separating just the river we want
rivers_map<- rivers %>% #control shift m
  filter(NAME == "Santa Fe River - Lower" | NAME == "Santa Fe River - Upper")

# Start and end points of sampling sites
spring<-c("Rum Island","Rum Island","Poe Springs","Poe Springs","Ginnie Springs","Ginnie Springs")
lat<-c(29.83383,29.83370,29.82631,29.82752,29.84454,29.83661)
long<-c(-82.67722,-82.68814,-82.64964,-82.65398,-82.70946,-82.69927)
Start_and_end_points<-(data.frame(spring=spring,lat=lat,long=long))


## Maps ##


# Map zoomed in to show just the Santa Fe River
zoomed<-ggplot() +
  geom_sf(data = states, fill = "lightgreen") + 
  geom_sf(data = county, fill = NA, color = gray(.5)) +
  geom_sf(data= alachua, fill= "#E69F00") +
  geom_sf(data= gil, fill= "pink") +
  geom_sf(data= columbia, fill= "#F0E442") +
  geom_sf(data = rivers_map, size= 2, color="blue") + 
  labs(x="",y="",color="")+
  geom_point(data=Start_and_end_points, aes(x=long,y=lat,color=spring), size=5, shape=18)+
  scale_colour_manual(values = c("red", "green","purple"))+
  coord_sf(xlim = c(-82.9, -82.3), ylim = c(29.75, 29.98), expand = FALSE, crs= st_crs(4326)) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)



# Map showing all of Florida
not_zoom<-ggplot() +
  geom_sf(data = states, fill = "lightgreen") + 
  geom_sf(data = county, fill = NA, color = gray(.5)) +
  geom_sf(data= alachua, aes(fill="Alachua")) +
  geom_sf(data= gil, aes(fill="Gilchrist")) +
  geom_sf(data= columbia,  aes(fill="Columbia")) +
  geom_sf(data = rivers_map, size= 1.25, color="blue") + 
  labs(x="",y="",color="")+
  scale_fill_manual(values = c("#E69F00", "#F0E442","pink"))+
  coord_sf(xlim = c(-88, -79.9), ylim = c(24.5, 31.1), expand = FALSE, crs= st_crs(4326)) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme(legend.title = element_blank())

#4326 is WGS84,very common projection coordinate system

# Showing the 2 maps side by side
p4<-plot_grid(not_zoom,zoomed, labels = c('A', 'B'))
