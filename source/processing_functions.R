# image processing functions 
## RBG image to show the specific disturbances in area
## filter for shadowed elements on ndvi and ndre 
## area stats on NDVI and NDRE
### mean, std, histogram 

reworkPlotReference <- function(aoi){
  p1 <- st_cast(aoi, "POLYGON")%>%
    mutate(referenceID = 1:nrow(.))
  return(p1)
}

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

calcNDVI <- function(image, NIR, red){
  red <- terra::subset(image, subset = red)
  
  NIR <- terra::subset(image, subset = NIR)
  
  # NDVI
  ## (NIR-Red)/(NIR+Red) 
  ndvi <- (NIR - red)/(NIR + red)
  names(ndvi) <- "ndvi"
  
  return(ndvi)
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
  
  plots2 <- plots %>%
    select(name)%>%
    terra::vect()
  # plots2 <- plots2[1:10,] #testing
  df <- data.frame(matrix(nrow = nrow(plots), ncol = 2))
  colnames(df) <- c("plotReference",name)
  df$plotReference <- plots2$name
  dfs <- list() 
  
  for(i in seq_along(plots2$name)){
    print(i)
    p1 <- plots2[i, ]
    df2 <-  terra::extract(x = image, y = p1, ID = FALSE)
    df2$plotReference <- p1$name
    dfs[[i]] <- df2
  }  
  t1 <- bind_rows(dfs)%>%
    select("plotReference", `name`)
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
  # calculate the mean, median, std, and total masked for each plot
  vals <- data[,2]
  totalCells <- length(vals)
  totalMask <- length(vals[is.na(vals)])
  
  df <- data[1,]%>%
    mutate(mean = mean(vals, na.rm = TRUE),
           standardDev = sd(vals, na.rm = TRUE),
           median = median(vals, na.rm = TRUE),
           percentMasked = (length(vals[is.na(vals)])/length(vals))*100 )%>%
    select(1,3:ncol(.))
  return(df)
}


