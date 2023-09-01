
fullAreaNDVIMasked <- function(image, NIR,red, green, greenMaskThres){
  ## processing on the full image area rather then a specific subset 
  r1 <- terra::subset(image, subset = "Alpha", negate = TRUE)
  
  print("generating indicies")
  # calculate indicies 
  ndvi <- calcNDVI(image = r1, 
                   NIR = NIR,
                   red = red)

  print("producing mask")
  # green mask layer 
  mask1 <- greenBandMask(image = r1,
                         layerName = green,
                         threshold = greenMaskThres)
  
  # apply mask 
  print("applying mask")
  r2 <- applyMask(mask = mask1, 
                  image = ndvi)
  
  return(r2)
}



processSpec<- function(image, aoi, redEdge, NIR,red, green, greenMaskThres, layerName){
  print("cropping and masking images")
  r1 <- clipCropMask(image = image, aoi = aoi)
  
  print("generating indicies")
  # calculate indicies 
  i1 <- calcIndicies(image = r1, 
               redEdge = redEdge,
               NIR = NIR,
               red = red)
  
  print("producing mask")
  # green mask layer 
  mask1 <- greenBandMask(image = r1,
                         layerName = green,
                         threshold = greenMaskThres)
  # bind datasets
  allRasters <- c(r1, rast(i1))
  
  # apply mask 
  print("applying mask")
  r2 <- applyMask(mask = mask1, 
                  image = allRasters)
  
  rastWithMask <- c(r2, mask1)
  
  print("....extracting values")
  r3 <- extractValuestoAreas(image = subset(r2, subset = layerName), plots = aoi, layerName)
  # convert to a list of data frames before applying summary functions
  r4 <- split(x = r3, f = r3$plotReference)
  
  ## summary stats
  print("....generating statistics")
  stats <- map(r4,calculateStatistics)%>%
    bind_rows()
  
  
  return(list(maskedImage = rastWithMask,
              data = stats)) 
}




#' Process image
#' 
#' @description : take in some reference values, images, and plot spatial layers and returned histograms and summary stats
#' @param image : An indice based TIF. (NDVI or NDRE)
#' @param aoi : specific plots that the data area generate at
#' @param captureDate : mmdd format
#' @param indices : NDVI or NDRE
#' @param threshold : cut off value to remove shadows from the imagery 
#'
#' @return
#' @export
#'
#' @examples
processImagery <- function(image, aoi, captureDate, indices, threshold){
  # crop image to aio 
  print("cropping and masking images")
  r1 <- clipCropMask(image = image, aoi = aoi)
  
  
  # process data with shadows -----------------------------------------------
  print("processing data with shadows")
  name <- paste0(indices, "_",captureDate)
  print("....extracting values")
  r2 <- extractValuestoAreas(image = r1, plots = aoi, name)
  # convert to a list of data frames before applying summary functions
  r3 <- split(x = r2, f = r2$plotReference)
  
  ## histograms 
  print("....generating histograms")
  figs <- map(r3, ~generateHistogram(data = ., name = name))
  
  ## summary stats
  print("....generating statistics")
  stats <- map(r3,calculateStatistics)%>%
    bind_rows()
  
  # process data without shadows --------------------------------------------
  name2 <- paste0(indices, "_",captureDate,"_shadowRemoved")
  print("Processing data removing the shadows")
  thres1 <- removeshadow(image = r1, threshold = threshold)
  
  print("....extracting values")
  r2t <- extractValuestoAreas(image = thres1, plots = aoi, name = name2)
  # convert to list of data frames 
  r3t <- split(x = r2t, f = r2t$plotReference)
  ## histograms 
  print("....generating histograms")
  figs2 <- map(r3t, ~generateHistogram(data = ., name = name2))
  
  ## summary stats
  print("....generating stats")
  stats2 <- map(r3t,calculateStatistics) %>%
    bind_rows()
  names(stats2)[-1] <- paste0(names(stats2)[-1],"_shadowRemoved")
  
  allStats <- left_join(x = stats, y = stats2, by ="plotReference")
  
  return(list(
    histograms = figs,
    histogram_noShadow = figs2,
    stats = allStats
  ))
}
