library(stats)
library(tidyverse)
library(viridis)
# GIS packages
library(sp)
source("Constants.R")
source("Functions_map_plot.R")

# ID_colliers <- c(6330, 6332, 6337, 6338, 6340, 6853, 6859, 6860)

# var_names = c("ID", "alpage", "date", "time", "TTF", "lat", "lon", "2D3D", "alt", "DOP", "SVs", "FOM", "actX", "actY")
# data = data.frame(matrix(nrow = 0, ncol = length(var_names)))
# colnames(data) = var_names
# for (ID in ID_colliers) {
#     traject <- read.csv(paste0(data_dir,"Colliers_2022_brutes/Colliers_PNM/collier_mouton_",ID,".csv"), header=TRUE, sep="\t")
#     traject <- traject[-c(1),]
#     traject <- subset(traject, select = -c(X.1, X.2))
#     traject <- cbind(rep(as.character(ALPAGES[paste0("a",ID)]), nrow(traject)), traject)
#     traject <- cbind(rep(ID, nrow(traject)), traject)
#     data <- rbind(data, traject)
# }
# colnames(data) <- var_names
# data$time = as.POSIXct(paste(data$date, data$time), format = "%Y %m %d %H:%M:%S", tz = "UTC")
# data$date = NULL
# data$lon = as.numeric(data$lon)
# data$lat = as.numeric(data$lat)


data <- SpatialPointsDataFrame(data[!is.na(data$lat),c("lon","lat")],
                                    data[!is.na(data$lat),],    #the R object to convert
                                    proj4string = CRS_WSG84)
data <- spTransform(data, CRS_L93)
data@data$x = data@coords[,"lon"]
data@data$y = data@coords[,"lat"]
data = data@data


### PLOTS
pdf(file = paste0(output_dir,"1 Etalonnage HMM/followit_activities.pdf"), width = 7, height = (12))

# actY vs actX
ggplot(data, aes(x=actX, y=actY)) +
    geom_bin2d(binwidth = c(1, 1)) +
    coord_equal() +
    scale_fill_viridis(option = "H", oob = scales::squish, trans = "log", limits = c(1, 800), guide = triangle_colourbar_up()) +
    facet_wrap(~ID, ncol = 2)

data$ODBA = sqrt(data$actX^2 + data$actY^2)

# Convert time to hour-min-sec only for plotting
hour <- as.POSIXlt(data$time)
hour <- hour$h + hour$min/60 + hour$s/3600 + 2 # + 2 to convert from UTC to local time
data$hour <- hour
rm(hour)

# ODBA vs hour
ggplot(data, aes(x = round(hour), y = ODBA, group = round(hour))) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90)) +
    xlab("hour") +
    facet_wrap(~ID, ncol = 2)

# actX vs hour
ggplot(data, aes(x = round(hour), y = actX, group = round(hour))) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90)) +
    xlab("hour") +
    facet_wrap(~ID, ncol = 2)

# actY vs hour
ggplot(data, aes(x = round(hour), y = actY, group = round(hour))) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90)) +
    xlab("hour") +
    facet_wrap(~ID, ncol = 2)

# ODBA 0s vs hour
ggplot(data, aes(x = round(hour))) +
    geom_bar(position = "fill", aes(fill = (ODBA == 0))) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(axis.text.x = element_text(angle = 90)) +
    xlab("hour") +
    ylab("") +
    facet_wrap(~ID, ncol = 2)

# Trajectories vs hour
# for (ID in ID_colliers) {
#     raster <- get_raster_cropped_L93("/home/moamo/These/QGIS/Donnees_sources/BDALTI/Fond_topo.tif", min(data[data$ID == ID,]$x)-100, max(data[data$ID == ID,]$x)+100, min(data[data$ID == ID,]$y)-100, max(data[data$ID == ID,]$y)+100)
#     raster <- as.data.frame(raster)
#     colnames(raster)[1] = "Ombrage"
#     plot_trajectory_over_ombrage(data[data$ID == ID,], "hour",
#     scale = scale_colour_gradient2(low = "blue", mid = "red", high = "blue", midpoint = 12, breaks = c(0, 12, 24)),
#     raster, ID, "Heure")
# }

# Trajectories
# data$ID = as.factor(data$ID)
# raster <- get_raster_cropped_L93("/home/moamo/These/QGIS/Donnees_sources/BDALTI/Fond_topo.tif", min(data$x)+100, 1005000, 6355000, max(data$y)-100)
# raster <- as.data.frame(raster)
# colnames(raster)[1] = "Ombrage"
# ggplot(data, aes(x, y, col = ID)) +
#     geom_raster(data=raster, aes(x=x, y=y, fill=Ombrage), alpha=0.7, inherit.aes = FALSE, show.legend=FALSE) +
#     scale_fill_gradientn(colours=c("black","white")) +
#     geom_path(size = 0.3) +
#     coord_equal() +
#     xlim(c(min(data$x)+100, 1005000)) +
#     ylim(c(6355000, max(data$y)-100))

dev.off()

saveRDS(data, file = paste0(data_dir,"followit_2022_unfiltered.rds"))
