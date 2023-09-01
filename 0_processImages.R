# bring in libraries 
pacman::p_load("terra", "sf", "dplyr", "tmap","plotly","purrr")
tmap_mode("view")

# source functions --------------------------------------------------------
lapply(X = list.files("source", full.names = TRUE, pattern = ".R"),
       FUN = source)


# read in data ------------------------------------------------------------
images <- list.files("data/Tif Maps/TAPS_2023", pattern = ".tif",
                     full.names = TRUE, recursive = TRUE)

# filter based on indice type
## this read in all images -- up to three
## SPEC images
spec <- images[grepl(pattern = "SPEC", x = images)] |>
  sort() |>
  map(rast)

## RBG images 
rgb <- images[grepl(pattern = "RGB", x = images)] |>
  sort() |>
  map(rast)

## NDVI
ndvi <- images[grepl(pattern = "NDVI", x = images)] |>
  sort() |>
  map(rast)

## NDRE 
ndre <- images[grepl(pattern = "NDRE", x = images)] |>
  sort() |>
  map(rast)
  
## this will change onces we develop the new area of interest 
plots <- read_sf("data/8 5 23 TAPS_2023.geojson")%>%
  filter(name != "Farm 0")%>%
  sf::st_transform(crs = 32613)

# define variables  -------------------------------------------------------
dates <- c("080423","081723","082423") ## the order here needs to match the sort() order when reading in images above



# process data 

# single image processing -------------------------------------------------
specResults <- processSpec(image = rast(spec),
                           aoi = plots,
                           redEdge = "Red edge",
                           NIR = "NIR",
                           red = "Red",
                           green = "Green",
                           greenMaskThres = 0.032,
                           layerName = "ndvi"
                           )


# writeRaster(x = specResults$ndvi, 
#             filename = "data/processedResults/ndviMask_0817.tif",
#             overwrite = TRUE)

# writeRaster(x = specResults$greenMask, 
#             filename = "data/processedResults/mask_0817.tif",
#             overwrite = TRUE)


# multiple image processing -----------------------------------------------
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

