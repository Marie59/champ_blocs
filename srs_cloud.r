#Rscript

######################
##    Cloud Mask    ##
######################

#####Packages : 
#               raster
#               sp

#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import the S2 data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    data_raster <- args[1]
    rasterheader <- args[2]
    data <- args[3]
 #   typepca <- as.character(args[4])
    source(args[4])
}

################################################################################
##              DEFINE PARAMETERS FOR DATASET TO BE PROCESSED                 ##
################################################################################
if (data_raster == "") {
  #Create a directory where to unzip your folder of data
  dir.create("data_dir")
  unzip(data, exdir = "data_dir")
  # Path to raster
  data_raster <- list.files("data_dir/results/CloudMask", pattern = "_RAW")
  input_image_file <- file.path("data_dir/results/CloudMask", data_raster[1])
  input_header_file <- file.path("data_dir/results/CloudMask", data_raster[2])

} else {
  input_image_file <- file.path(getwd(), data_raster, fsep = "/")
  input_header_file <- file.path(getwd(), rasterheader, fsep = "/")
}

################################################################################
##                              PROCESS IMAGE                                 ##
################################################################################
input_image_file <- raster::raster(input_image_file)

#cloud_tab <- convert_raster(input_image_file)
r_pts <- raster::rasterToPoints(input_image_file, spatial = TRUE)

# reproject sp object
geo_prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
r_pts <- sp::spTransform(r_pts, sp::CRS(geo_prj))


# Assign coordinates to @data slot, display first 6 rows of data.frame
r_pts@data <- data.frame(r_pts@data, longitude = sp::coordinates(r_pts)[, 1],
                         latitude = sp::coordinates(r_pts)[, 2])

write.table(r_pts@data, file = "cloud.tabular", sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)

#cloud_plot <- 
