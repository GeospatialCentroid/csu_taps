# bring in libraries 
pacman::p_load("terra", "sf", "dplyr", "tmap","plotly","purrr")
tmap_mode("view")

# source functions --------------------------------------------------------
lapply(X = list.files("source", full.names = TRUE, pattern = ".R"),
       FUN = source)


# read in data ------------------------------------------------------------
## server 
images <- list.files("data/TAPS_2023/TAPS_2023/Tif Maps/TAPS_2023", pattern = "//.tif$",
                     full.names = TRUE, recursive = TRUE)
## centroid 
images <- list.files("data/TAPS_2023/TAPS_2023/Tif Maps/TAPS_2023", pattern = ".tif$",
                     full.names = TRUE, recursive = TRUE)

plot(spec[[1]]$Green)

# filter based on indice type
## this read in all images -- up to three
## SPEC images
spec <- images[grepl(pattern = "SPEC", x = images)] |>
  sort() |>
  map(rast)

# ## RBG images 
# rgb <- images[grepl(pattern = "RGB", x = images)] |>
#   sort() |>
#   map(rast)
# 
# ## NDVI
# ndvi <- images[grepl(pattern = "NDVI", x = images)] |>
#   sort() |>
#   map(rast)
# 
# ## NDRE 
# ndre <- images[grepl(pattern = "NDRE", x = images)] |>
#   sort() |>
#   map(rast)
  
## this will change onces we develop the new area of interest 
plots <- sf::read_sf("qgis/referenceGrid_0920.gpkg")
View(plots)
## export file for reference 
# st_write(obj = plots, dsn = "data/TAPS_2023/TAPS_2023/TAPS_Subplots/annotations/poly_withRef.gpkg")
  
# define variables  -------------------------------------------------------
dates <- c("080423","081723","082423") ## the order here needs to match the sort() order when reading in images above



# process data 

# generaete NDVI mask layer  ---------------------------------------------
ndviMask <- fullAreaNDVIMasked(image = spec[[4]],
                               NIR = "NIR",
                               red = "Red",
                               green = "Green",
                               greenMaskThres = 0.032
                                 )


## export raster
writeRaster(x = ndviMask, filename = "data/processedResults/maskNDVI_0920.tif")

# single image processing -------------------------------------------------
specResults <- processSpec(image = spec[[4]],
                           aoi = plots,
                           redEdge = "Red edge",
                           NIR = "NIR",
                           red = "Red",
                           green = "Green",
                           greenMaskThres = 0.032,
                           layerName = "ndvi"
                           )

# export multilayer raster 
writeRaster(x = specResults$maskedImage,
            filename = "data/processedResults/bands_indicies_Mask_0920.tif",
            overwrite = TRUE)

# export the data results
write.csv(x = specResults$data, file = "data/processedResults/ndvi09_20_summaryStats_labeled.csv")

# multiple image processing -----------------------------------------------
### older workflow that has been moved away from. (center on processing multiple dates
### of images at once). keeping for now be should probably be fully integrated or removed. 

## NDVI
### using purrr to process multiple images... not an active part of the workflow.
##! 20230901 not a current part of the analysis
# ndviResults <- map2(.x = ndvi,.y = dates, .f = ~processImagery(image = .x,
#                                                               aoi = aoi,
#                                                               captureDate = .y,
#                                                               indices = "NDVI",
#                                                               threshold = 0.7))
# ## NDRE
# ndreResults <- map2(.x = ndre,.y = dates, .f = ~processImagery(image = .x,
#                                                                aoi = aoi,
#                                                                captureDate = .y,
#                                                                indices = "NDRE",
#                                                                threshold = 0.7))

# Export Results  ---------------------------------------------------------
## this needs to be improved should probably be embedded in the function with some 
## additional parameters 
##! 20230901 not a current part of the analysis
# for(i in 1:3){
#   s1 <- ndviResults[[i]]$stats
#   View(s1)
#   write.csv(x = s1, file = paste0("data/processedResults/stats_",dates[i],".csv"))
# }
# indviResults[[1]]$histograms[1]
# ndviResults[[2]]$histograms[1]
# ndviResults[[3]]$histograms[1]
# ndviResults[[1]]$histogram_noShadow[1]

