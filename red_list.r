#Rscript

###############################
##    Red List Index (IUCN)  ##
###############################

#####Packages : 
#               raster
#               sp

#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import the S2 data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    source(args[1])
    redlist_type <- as.character(args[2])
    data_raster_1 <- args[3]
    rasterheader_1 <- args[4]
    data_raster_2 <- args[5]
    rasterheader_2 <- args[6]
    data_1 <- args[7]
    data_2 <- args[8]
}

################################################################################
##              DEFINE PARAMETERS FOR DATASET TO BE PROCESSED                 ##
################################################################################

if (data_raster_1 == "") {
    #Create a directory where to unzip your folder of data
    dir.create("data_dir_1")
    unzip(data_1, exdir = "data_dir_1")
    # Path to raster
    data_raster <- list.files("data_dir_1/results/Reflectance", pattern = "_Refl")
    input_image_file <- file.path("data_dir_1/results/Reflectance", data_raster[1])
    input_header_file <- file.path("data_dir_1/results/Reflectance", data_raster[2])
} else {
    input_image_file <- file.path(getwd(), data_raster_1, fsep = "/")
    input_header_file <- file.path(getwd(), rasterheader_1, fsep = "/")
}
if (data_raster_2 == "" && data_2 != "" ) {
    #Create a directory where to unzip your folder of data
    dir.create("data_dir_2")
    unzip(data_2, exdir = "data_dir_2")
    # Path to raster
    data_raster <- list.files("data_dir_2/results/Reflectance", pattern = "_Refl")
    input_image_file2 <- file.path("data_dir_2/results/Reflectance", data_raster[1])
    input_header_file2 <- file.path("data_dir_2/results/Reflectance", data_raster[2])

} else if (data_raster_2 != "" && data_2 == "") {
    input_image_file <- file.path(getwd(), data_raster_1, fsep = "/")
    input_header_file <- file.path(getwd(), rasterheader_1, fsep = "/")
    input_image_file2 <- file.path(getwd(), data_raster_2, fsep = "/")
    input_header_file2 <- file.path(getwd(), rasterheader_2, fsep = "/")
}

################################################################################
##                              PROCESS IMAGE                                 ##
################################################################################

input_image_file1 <- raster::raster(input_image_file)
stop(head(convert_raster(input_image_file1)))
### Plotting out data
## Basic information for the data

# r Calculate area of rasters
area_1 <- redlistr::getArea(input_image_file1)

#area_2 <- redlistr::getArea(input_image_file2)
### Creating binary ecosystem raster
# r Binary object from multiclass ???????????????
#An additional parameter in `getArea` is to specify which class to count if your 
#raster has more than one value (multiple ecosystem data stored within a single
#file). However, for further functions, it may be wise to convert your raster
#into a binary format, containing only information for your target ecosystem.

#First, if you do not know the value which represents your target ecosystem, you 
#can plot the ecosystem out, and use the `raster::click` function. Once you know
#the value which represents your target ecosystem, you can create a new raster
#object including only that value


if (criteria == "A") {
  ## Assessing Criterion A
  # The IUCN Red List of Ecosystems criterion A requires estimates of the magnitude of
  #change over time. This is typically calculated over a 50 year time period (past,
  #present or future) or against a historical baseline ([Bland et al., 2016](https://portals.iucn.org/library/sites/library/files/documents/2016-010.pdf)).
  #The first step towards acheiving this change estimate over a fixed time frame is
  #to assess the amount of change observed in your data.
  input_image_file2 <- raster::raster(input_image_file2)

  ### Plotting out data
  ## Basic information for the data
  #Plotter les 2 data de base l'un à coté de l'autre avec cowplot et gridextra

  area_2 <- redlistr::getArea(input_image_file2)
  ### Area change
  area_lost <- redlistr::getAreaLoss(area_1, area_2)
  
  area_text <- paste0("Area lost between your 2 data ", area_lost, "Km2")
  ### Rate of change
  #These rates of decline allow the use of two or more data points to extrapolate
  #to the full 50 year timeframe required in an assessment.

  decline_stats <- redlistr::getDeclineStats(area_1, area_1, 2000, 2017, 
                                 methods = c("ARD", "PRD", "ARC"))
  
  decline_text <- paste0("Rates of decline ", decline_stats)
  #Now, it is possible to extrapolate, using only two estimates of an ecosystems"
  #area, to the full 50 year period required for a Red List of Ecosystems
  #assessment. 

  extrapolated_area <- redlistr::extrapolateEstimate(area_1, year.t1 = 2000, 
                                        ARD = decline_stats$ARD, 
                                        PRD = decline_stats$PRD, 
                                        ARC = decline_stats$ARC, 
                                        nYears = 50)

  extrapo_text <- paste0("Extrapolation ", extrapolated_area)
  #If we were to use the Proportional Rate of Decline (PRD) for our example
  #assessment, our results here will be suitiable for criterion A2b (Any 50 year
  #period), and the percent loss of area is:
  predicted_percent_loss <- (extrapolated_area$A.PRD.t3 - area_1) / area_1 * 100

  pploss_text <- paste0("the predicted percent loss of area is ", predicted_percent_loss, "%")

  ##write tinto a text file those different results as a repport
  repport <- paste0("Repport on Criterion A", "\t", area_text, "\t", decline_text, "\t", extrapo_text, "\t", pploss_text)
  
  write.table(repport, "repport.txt")
  

}else {
  ## Assessing Criterion B (distribution size)
  #Criterion B utilizes measures of the geographic distribution of an ecosystem 
  #type to identify ecosystems that are at risk from catastrophic disturbances. 
  #This is done using two standardized metrics: the extent of occurrence (EOO) and 
  #the area of occupancy (AOO) 

  ### Subcriterion B1 (calculating EOO)
  #For subcriterion B1, we will need to calculate the extent of occurrence (EOO) of our
  #data. We begin by creating the minimum convex polygon enclosing all occurrences
  #of our focal ecosystem.

  eoo_polygon <- redlistr::makeEOO(input_image_file1)
  polygon_plot <- ggplot2::ggplot(eoo_polygon, ggplot2::aes(x = long, y = lat)) + 
   ggplot2::geom_polygon(colour = "black", fill = "white") 
### ADD THE RAW DATA ON TOP OF THE POLYGON !!!!!!!!!
  ggplot2::ggsave("polygon.png", polygon_plot, width = 12, height = 10, units = "cm")
 
 #Calculating EOO area
  eoo_area <- redlistr::getAreaEOO(eoo_polygon)

  ### Subcriterion B2 (calculating AOO)
  #For subcriterion B2, we will need to calculate the number of 10x10 km grid cells
  #occupied by our distribution. We begin by creating the appopriate grid cells.

  aoo_grid <- redlistr::makeAOOGrid(input_image_file1, grid.size = 10000,
                            min.percent.rule = F)
  aoo_plot <- ggplot2::ggplot(aoo_grid, ggplot2::aes(x = long, y = lat, group = "ID")) #+ 
   #ggplot2::geom_polygon(colour = "black", fill = "white")
### ADD THE RAW DATA ON TOP OF THE POLYGON !!!!!!!!!
  ggplot2::ggsave("aoo.png", aoo_plot, width = 12, height = 10, units = "cm")

  #Finally, we can use the created grid to calculate the AOO 
  n_aoo <- length(aoo_grid)
  tab <- as.data.frame(eoo_area)
  tab$aoo <- n_aoo
  write.table(tab, file = "repport.txt", sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)

  #### Grid uncertainty functions
  #`gridUncertainty` simply moves the AOO grid systematically (with a
  #small random movement around fixed points), and continues searching  for a minimum 
  #AOO until additional shifts no longer produce improved results.


  gu_results <- redlistr::gridUncertainty(input_image_file1, 10000,
                              n.AOO.improvement = 5, 
                              min.percent.rule = F)
### ADD THE RAW DATA ON TOP OF THE POLYGON !!!!!!!!!
  #gu_results <- as.data.frame(gu_results)
  #gu_plot <- ggplot2::ggplot(gu_results, ggplot2::aes(x = long, y = lat, group = "ID")) + 
   #ggplot2::geom_polygon(colour = "black", fill = "white")
  #ggplot2::ggsave("gu.png", gu_plot, width = 12, height = 10, units = "cm")

  #### One percent rule
  #?????????????

  #In addition to the size of the grids used (which will be different for species 
  #assessments), there is also an option in the `makeAOOGrid` function to specify 
  #whether a minimum of percent of the grid cell area must be occupied before they 
  #are included as an AOO grid. Typically, this is only used in very special cases
  #for ecosystems with highly skewed distribution of patch sizes ([Bland et al., 2016](https://portals.iucn.org/library/sites/library/files/documents/2016-010.pdf)).

  #Here, we demonstrate the differences between including, or not including the one
  #percent rule:

  #r One percent grid, fig.width=7, fig.height=7}
  AOO_grid_one_percent <- redlistr::makeAOOGrid(mangrove.2017, grid.size = 10000, 
                                    min.percent.rule = TRUE, percent = 1)
  ### ADD THE RAW DATA ON TOP OF THE POLYGON !!!!!!!!!
  #plot(AOO_grid_one_percent)
  #plot(mangrove_2017, add = T, col = "green", legend = FALSE)


  #There is an additional parameter - `percent` - which adjusts the threshold for
  #the AOO grids. Here, we set it to 0.1% to demonstrate its functionalities.

  #{r AOO Grid 0.1percent, fig.width=7, fig.height=7}
  AOO_grid_min_percent <- redlistr::makeAOOGrid(mangrove.2017, grid.size = 10000,
                                    min.percent.rule = TRUE, percent = 0.1)
  #par(mfrow = c(2,2))
  #plot(AOO.grid, main = 'AOO grid without one percent rule')
  #plot(mangrove.2017, add = T, col = "green", legend = FALSE)
  #plot(AOO.grid.one.percent, main = 'AOO grid with one percent rule')
  #plot(mangrove.2017, add = T, col = "green", legend = FALSE)
  #plot(AOO.grid.min.percent, main = 'AOO grid with one percent rule at 0.1%')
  #plot(mangrove.2017, add = T, col = "green", legend = FALSE)

### ADD THE RAW DATA ON TOP OF THE POLYGON !!!!!!!!!
}







