# Loading packages (install if not yet installed)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest, raster, gepaf, sf, gtools, rStrava, ggrepel, plotly)

# Admin
sname <- "lazarus1"
sid <- 39680
stoken <- "3b681c5afe1168342f9ad1a551e1ea0bf473157d"

country <- "schweiz"
  
scrape <- 0

######### Scraping quäldich.de #########
if (scrape == 1) {
  
  countries <- c("frankreich","italien","spanien","schweiz")
  
  for (c in 1:length(countries)) {
    
    start_page <- read_html(paste0("https://www.quaeldich.de/regionen/",countries[c],"/paesse/?n=7000"))
    list_of_urls <- start_page %>% 
      html_nodes(".col-sm-5:nth-child(1) a") %>%
      html_attr(name = "href") %>%
      as.data.frame() %>%
      unique()
    
    names(list_of_urls)[1] <- "link"
    
    list_of_names <- start_page %>% 
      html_nodes(".col-sm-5:nth-child(1) a") %>%
      html_text() %>%
      as.data.frame()
    
    names(list_of_names)[1] <- "name"
    
    list_of_altitudes <- start_page %>% 
      html_nodes(".hidden-xs.col-sm-2") %>%
      html_text() %>%
      as.data.frame() 
    
    names(list_of_altitudes)[1] <- "altitude"
    list_of_altitudes$altitude <- trimws(list_of_altitudes$altitude, which = c("both"))
    list_of_altitudes$altitude <- gsub(" m","",list_of_altitudes$altitude)
    
    ds <- cbind(list_of_names, list_of_altitudes, list_of_urls)
    
    for (l in 1:nrow(ds)) {
      
      page <- read_html(paste0("https://www.quaeldich.de",ds$link[l]))
      
      node <- page %>%
        html_nodes(".coords a")
      ds$coords[[l]] <- ifelse(length(node)!= 0,
                               node[1] %>% 
                                 html_attr(name = "href"),
                               NA
      ) 
      
      ds$coords[[l]] <- gsub(".*lonlats=","",ds$coords[[l]])
      ds$coords[[l]] <- gsub("&profile=fastbike","",ds$coords[[l]])

      node <- page %>%
        html_nodes("#tag-cloud a")
      ds$type[[l]] <- ifelse(length(node)!= 0,
                             node[1] %>% 
                               html_text(),
                             NA
      ) 
      
      node <- page %>% 
        html_nodes("h1 img") 
      ds$country[[l]] <- ifelse(length(node)!= 0,
                                node %>% 
                                  html_attr(name = "src"),
                                NA
      )
      
      ds$country <- gsub("https://www.quaeldich.de/webinclude/img/flag/","",ds$country) 
      ds$country <- gsub(".png","",ds$country) 
      
      node <- page %>% 
        html_nodes(".pass-regionen a:nth-child(1)") 
      ds$region_1[[l]] <- ifelse(length(node)!= 0,
                                 node %>% 
                                   html_text(),
                                 NA
      )
      
      node <- page %>% 
        html_nodes(".pass-regionen a:nth-child(2)") 
      ds$region_2[[l]] <- ifelse(length(node)!= 0,
                                 node %>% 
                                   html_text(),
                                 NA
      )
      
      node <- page %>% 
        html_nodes(".pass-regionen a:nth-child(3)") 
      ds$region_3[[l]] <- ifelse(length(node)!= 0,
                                 node %>% 
                                   html_text(),
                                 NA
      )
      
      node <- page %>% 
        html_nodes(".pass-regionen a:nth-child(4)") 
      ds$region_4[[l]] <- ifelse(length(node)!= 0,
                                 node %>% 
                                   html_text(),
                                 NA
      )
      
      node <- page %>% 
        html_nodes(".pass-regionen a:nth-child(5)") 
      ds$region_5[[l]] <- ifelse(length(node)!= 0,
                                 node %>% 
                                   html_text(),
                                 NA
      )
      
      print(l)
      
    }
    
    # Adjustments
    ds <- separate(ds, coords, c("lon", "lat"), ",", remove = TRUE)
    ds$lat <- as.numeric(ds$lat)
    ds$lon <- as.numeric(ds$lon)
    ds$altitude <- as.numeric(ds$altitude)
    ds$id <- row.names(ds)
    
    # Remove relevant missing values
    ds <- ds %>% 
      drop_na(lat, lon)
    
    # Removing unused variables
    rm(list = c('list_of_altitudes','list_of_names','list_of_urls','node','page','start_page'))
    
    # Saving as rData
    save(ds, file = paste0("data/pass_data_",countries[c],".Rdata"))
    
  }
  
}

######### Data preparation #########
# Loading pass data
load(paste0("data/pass_data_",country,".Rdata"))

# Drop Schotter and Sackgasse from pass data
pass_info <- subset(ds, is.na(type) | type != "Schotter" & type != "Sackgasse")

# Define pass ID
pass_info$id <- NULL
pass_info$id <- as.integer(seq(length = nrow(pass_info)))
pass_info <- pass_info %>%
  dplyr::select(id, everything())
row.names(pass_info) <- pass_info$id

# Set the radius for the plots (radius in meters/ WGS84-Scaling Factor)
radius = 70/10000

# Define the plot edges based upon the plot radius 
yPlus <- pass_info$lat+radius
xPlus <- pass_info$lon+radius
yMinus <- pass_info$lat-radius
xMinus <- pass_info$lon-radius

# Calculate polygon coordinates for each plot centroid. 
square = cbind(xMinus, yPlus,  # NW corner
               xPlus, yPlus,  # NE corner
               xPlus, yMinus,  # SE corner
               xMinus, yMinus, # SW corner
               xMinus, yPlus)  # NW corner again - close ploygon

# Extract the plot ID information
ID = as.character(pass_info$id)

# Create SpatialPolygons from coordinates
pass_polygons <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID = id)
}, 
split(square, row(square)), ID), proj4string = CRS(as.character("+proj=longlat +datum=WGS84")))

# Preliminary plot
#plot(pass_polygons)

## Get strava data

# Create the authentication token
stoken <- httr::config(token = strava_oauth(sname, sid, stoken, app_scope = "activity:read_all"))

# Get activities
my_acts <- get_activity_list(stoken)
data <- as.data.frame(compile_activities(my_acts)) 

# Subset rides and runs and activities with coordinates, elevation higher than 1000 masl
data <- subset(data, type == "Ride")
data <- subset(data, !is.na(start_latlng1) | !is.na(start_latlng2))
data <- subset(data, elev_high >= 1000)

if (country == "italien") {
  data <- data %>%
    filter(str_detect(data$timezone, "Rome"))
} else if (country == "schweiz") {
  data <- data %>%
    filter(str_detect(data$timezone, "Zurich"))
} else if (country == "frankreich") {
  data <- data %>%
    filter(str_detect(data$timezone, "Paris"))
}  else if (country == "spanien") {
    data <- data %>%
      filter(str_detect(data$timezone, "Madrid"))  
}

# Get polylines 
map_list <- list()

for (i in 1:nrow(data)) {
  decoded_polyline <- gepaf::decodePolyline(data$map.summary_polyline[i])
  decoded_polyline$id <- i
  map_list[[i]] <- decoded_polyline
  if (i == 500) {
    Sys.sleep(60*5)
  } 
  print(i)
}

# Stack geocoded data
polyline_data <- do.call(rbind, map_list)

# Select relevant variables
rides <- polyline_data %>% 
  dplyr::select(lon, lat, id)

# Generate SpatialPoints dataframe with WGS84 as CRS
coordinates(rides) <- c("lon","lat")
rides_spatial <- as(rides,"SpatialPoints")
proj4string(rides_spatial) <- CRS("+proj=longlat +datum=WGS84")
proj4string(rides_spatial) <- proj4string(pass_polygons)

# Check if points are in pass polygons
output <- over(rides_spatial, pass_polygons)

# Keep only passed and unique SpatialPoints
passed <- unique(as.data.frame(na.omit(output)))
names(passed)[1] <- "id"
passed$passed <- "Check!"

# Merge passed df with Pass information df 
merged_passes <- left_join(pass_info, passed, by = "id")

# Fill NAs (= not (yet!) passed passes) with "No"
merged_passes$passed <- ifelse(is.na(merged_passes$passed), "No, not yet", "Yes, check!")

# Sort merged df in descending order and select highest passes for plotting
plot_data <- merged_passes %>% 
  arrange(desc(altitude)) %>%
  head(.,75)

# Add altitude category
plot_data$categorie <- ifelse(plot_data$altitude >= 2000, "Alpine catégorie (AC)", 
                              ifelse(plot_data$altitude < 2000 & plot_data$altitude >= 1500, "Haute catégorie (HC)", 
                                     ifelse(plot_data$altitude < 1500 & plot_data$altitude >= 1000, "Première catégorie (PC)",
                                            "Else"))) 

######### Plotting #########
# Plotting prelims
green = "springgreen4"
red = "tomato2"
    
capitalize <- function(x) {
    x <- strsplit(x, " ")
    for (i in seq(along = x)) {
      substr(x[[i]], 1, 1) <- toupper(substr(x[[i]], 1, 1))
    }
    sapply(x, function(z) paste(z, collapse = " "))
  }
  
# Plotting Checkbook 
p1 <- ggplot(data = plot_data, aes(x = reorder(name, +altitude), y = altitude)) + 
  facet_grid(. ~ reorder(categorie, +altitude), switch = "x", scales = "free_x", space = "free_x") +
  geom_text(aes(label = paste0(altitude,"")), hjust = -.4, angle = 90, size = 2, fontface = ifelse(plot_data$passed == "Yes, check!", "bold", "plain")) +
  geom_segment(aes(y = min(altitude)-50, 
                   x = name, 
                   yend = altitude, 
                   xend = name,
                   color = passed),
               size = 4, lineend = "butt") +
  scale_y_continuous(limits = c(min(plot_data$altitude)-50, max(plot_data$altitude)+50), expand = c(0, 0)) +
  scale_color_manual(breaks = c("Yes, check!","No, not yet"), values = c(green, red)) +
  ylab("") +
  xlab("") +
  labs(color = "Passed?") +
  theme_minimal() +
  theme(strip.placement = "outside",
        strip.text.x = element_text(size = 16, face = "bold", colour = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.text.x = element_text(face="bold", size = 6, angle = 45, hjust = 0.99), #element_text(size = 6, angle = 45, hjust = 0.99),
        legend.position = "top",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),    
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
#p1

ggsave(plot = p1, width = 297, height = 210, unit = "mm", bg = "white", dpi = 400, filename = paste0("plots/passed_",country,".png"))
  
l1 <- plotly::ggplotly(p1)

htmlwidgets::saveWidget(l1, paste0("plots/passed_",country,".html"))

# Spatial plot
# Convert coordinates from WGS 84 to LV95 
merged_passes_conv <- merged_passes
coordinates(merged_passes_conv) <- c("lon", "lat")
proj4string(merged_passes_conv) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+proj=somerc +lat_0=46.95240555555556
+lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000
+ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
merged_passes_conv <- spTransform(merged_passes_conv, CRS.new)
merged_passes_conv <- as.data.frame(merged_passes_conv)

country_data <- st_read("data/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp")
country_data <- st_transform(country_data, CRS.new)

# Loading lakes
lakes <- st_read("data/22_DKM500_GEWAESSER_PLY.shp")
lakes <- st_transform(lakes, crs = CRS.new)

lakes <- subset(lakes, lakes$SHP_AREA >= 6824223.5)
lakes <- subset(lakes, NAMN1 != "Lago di Garda" & NAMN1 != "Lac d'Annecy" & NAMN1 != "Lago di Como / Lario" &
                     NAMN1 != "Schluchsee" & NAMN1 != "Forggensee" & NAMN1 != "Lago d'Iseo" & NAMN1 != "Lago di Varese" &
                     NAMN1 != "Lago d'Orta" & NAMN1 != "Lago di Comabbio" & NAMN1 != "Lago di Pusiano" & NAMN1 != "Lago di Annone" &
                     NAMN1 != "Lago Didro" & NAMN1 != "Lac de Saint-Point" & NAMN1 != "Lago di Mezzola" & NAMN1 != "Lago di Como / Lario" &
                       NAMN1 != "Lac d'Annecy" & NAMN1 != "Lago Didro" & NAMN1 != "Lago di Garda" & NAMN1 != "Lago d'Orta")

relief <- raster("data/02-relief-georef-clipped-resampled.tif")
relief_spdf <- as(relief, "SpatialPixelsDataFrame")
relief <- as.data.frame(relief_spdf) 
relief <- relief %>% 
  rename(value = X02.relief.georef.clipped.resampled_1)
rm(relief_spdf)

merged_passes_conv$new_name <- ifelse(merged_passes_conv$passed != "Check!", as.character(merged_passes_conv$name), "")  

p2 <- ggplot() +
  geom_sf(data = subset(country_data, NAME == "Schweiz"), fill = "white", color = "black", size = 0.5) +
  geom_raster(data = relief, aes(x = x, y = y, alpha = value), fill = "grey30") +
  scale_alpha(name = "", range = c(1, 0), guide = "none") + 
  geom_sf(data = lakes, color = 'deepskyblue3', fill = 'deepskyblue3', size = 0.025, show.legend = FALSE) +
  geom_point(data = subset(merged_passes_conv, altitude > 1000), 
             aes(x = coords.x1, y = coords.x2, 
                 color = passed, 
                 size = altitude/250), alpha = 0.8) +
  geom_label_repel(data = subset(merged_passes_conv, altitude > 1000 & passed == "Yes, check!"), 
                   aes(x = coords.x1, y = coords.x2, 
                       color = passed, 
                       label = name), 
                   fontface = "bold",
                   size = 1.75, alpha = 0.8, min.segment.length = 0.1,
                   max.overlaps = 26, show.legend = FALSE, 
                   box.padding = unit(0.35, "lines")) +
  geom_label_repel(data = subset(merged_passes_conv, altitude > 1000 & passed != "Yes, check!"), 
                   aes(x = coords.x1, y = coords.x2, 
                       color = passed, 
                       label = name), 
                   fontface = "plain",
                   size = 1.75, alpha = 0.8, min.segment.length = 0.1,
                   max.overlaps = 26, show.legend = FALSE, 
                   box.padding = unit(0.35, "lines")) +
  scale_color_manual(name = "Passed?", values = c("springgreen4", "tomato2"), 
                     breaks = c("Yes, check!","No, not yet"),
                     labels = c("Yes, check!","No, not yet")) +
  theme_minimal() +
  guides(size = "none", color = guide_legend(override.aes = list(size = 6))) +
  xlab("") +
  ylab("") +
  theme(legend.position = "top",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        plot.caption = element_text(color = "black", face = "italic", size = 6)) +
  labs(caption = "Passes are 1'000 meters above sea level") +
  coord_sf()

#p2

ggsave(plot = p2, width = 330, height = 210, unit = "mm", bg = "white", dpi = 400, filename = paste0("plots/spatial_",country,".png"))

l2 <- plotly::ggplotly(p2)

htmlwidgets::saveWidget(l1, paste0("plots/spatial_",country,".html"))





# Preview graph
p2 <- ggplot() +
  geom_sf(data = subset(country_data, NAME == "Schweiz"), fill = "white", color = "black", size = 0.5) +
  geom_raster(data = relief, aes(x = x, y = y, alpha = value), fill = "grey30") +
  scale_alpha(name = "", range = c(1, 0), guide = "none") + 
  geom_sf(data = lakes, color = 'lightskyblue2', fill = 'lightskyblue2', size = 0.025, show.legend = FALSE) +
  geom_point(data = subset(merged_passes_conv, altitude > 1000), 
             aes(x = coords.x1, y = coords.x2, 
                 color = passed, 
                 size = altitude/250), alpha = 0.9) +
  geom_label_repel(data = subset(merged_passes_conv, altitude > 1500 & passed == "Yes, check!"),
                   aes(x = coords.x1, y = coords.x2,
                       color = passed,
                       label = name),
                   fontface = "bold",
                   size = 1.75, alpha = 0.9, min.segment.length = 0.1,
                   max.overlaps = 26, show.legend = FALSE,
                   box.padding = unit(0.35, "lines")) +
  geom_label_repel(data = subset(merged_passes_conv, altitude > 1500 & passed != "Yes, check!"),
                   aes(x = coords.x1, y = coords.x2,
                       color = passed,
                       label = name),
                   fontface = "bold",
                   size = 1.75, alpha = 0.8, min.segment.length = 0.1,
                   max.overlaps = 26, show.legend = FALSE,
                   box.padding = unit(0.35, "lines")) +
  scale_color_manual(name = "Passed?", values = c("springgreen4", "tomato2"), 
                     breaks = c("Yes, check!","No, not yet"),
                     labels = c("Yes, check!","No, not yet")) +
  guides(size = "none", color = guide_legend(override.aes = list(size = 6))) +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        plot.caption = element_text(color = "black", face = "italic", size = 6)) +
  theme_void() +
  theme(legend.position = "none")

ggsave(plot = p2, width = 330, height = 190, unit = "mm", bg = "lightskyblue4", dpi = 400, filename = paste0("plots/preview_spatial_",country,".png"))


