# image processing functions 
## RBG image to show the specific disturbances in area
## filter for shadowed elements on ndvi and ndre 
## area stats on NDVI and NDRE
### mean, std, histogram 



clipCropMask <- function(image, aoi){
  # remove all areas outside of of the plot areas
  # return: image 
  r1 <- terra::crop(image, aoi, mask = TRUE)
  return(r1)
}


greenBandMask <-function(image, layerName, threshold){
  # reassign values below a threshold to NA and return an image   
  # calculate the total number of values below the threshold 
  # return image 
  
  i2 <- terra::subset(image, subset = layerName)
  
  m <- c(0, threshold, NA,
         threshold, 1 , 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  rc1 <- classify(i2, rclmat, include.lowest=TRUE)
  names(rc1) <-"greenMask"
  return(rc1)
}

applyMask <- function(mask, image){
  m1 <- mask * image
  names(m1) <- names(image)
  return(m1)
}

calcIndicies <- function(image, redEdge, NIR, red){
  
  red <- terra::subset(image, subset = red)
  
  NIR <- terra::subset(image, subset = NIR)
  
  redEdge <- terra::subset(image, subset = redEdge)
  
  # NDRE
  ## (NIR – RedEdge)/(NIR + RedEdge)
  ndre <- (NIR - redEdge)/(NIR + redEdge) 
  names(ndre) <- "NDRE"
  # NDVI
  ## (NIR-Red)/(NIR+Red) 
  ndvi <- (NIR - red)/(NIR + red)
  names(ndvi) <- "NDVI"
  
  return(list(
    ndvi = ndvi,
    ndre = ndre
  ))
}

# 
# removeshadow <-function(image, threshold){
#   # reassign values below a threshold to NA and return an image   
#   # calculate the total number of values below the threshold 
#   # return image 
#   m <- c(0, threshold, NA)
#   rclmat <- matrix(m, ncol=3, byrow=TRUE)
#   rc1 <- classify(image, rclmat, include.lowest=TRUE)
# 
#   return(rc1)
# }

extractValuestoAreas <- function(image,plots, name){
  # spatial intersection between plots and images 
  # return: data frame of values associate with each plot 
  t1 <- terra::extract(x = image, y = plots)
  colnames(t1) <- c("plotReference",name)
  ###!!! will need to add some reference to the acutal plot name here but the
  ### current datasets does not have a meaningful value to apply. 
  # for(i in unique(t1$plotReference)){
  #   t1$plotReference[t1$plotReference == i] <- plots$name[i]
  # }
  
  return(t1)
}

### set to be ran with the purrr map function
generateHistogram <- function(data, name){
  # generate histograms based on data at each plot 
  ref <- data[1,1]
  
  fig <- plot_ly(x = data[,2], type = "histogram")%>%
      layout(title= paste0("Distribution of ", name , " values for plot ", ref), 
             xaxis = list(title = name), 
             yaxis = list(title = 'Number of observations'),
             plot_bgcolor = "#c7daec") 
  return(fig)
}
  
### set to be ran with the purrr map function
calculateStatistics <- function(data){
  # calculate the mean, median, std, and other statistics for each plot 
  vals <- data[,2]
  df <- data[1,]%>%
    mutate(mean = mean(vals, na.rm = TRUE),
           standardDev = sd(vals, na.rm = TRUE),
           median = median(vals, na.rm = TRUE))%>%
    select(1,3,4,5)
  return(df)
}


