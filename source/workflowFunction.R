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
