# csu_taps
processing of high resolution drone images for the irrigation innovation consortium

## structure 

- **Data**: source location for images and spatial reference layer. Not included in the github due to file sizes

- **qgis** : supporting processing and visualization work

- **source** : holds workflow and processing functions. 


## expected workflow. 
1. All imagery and reference files are moved to data folder. Images should have data capture and standardized band names 
2. open `0_processImages.R`
  - Define file path to images
  - define file path to spatial reference layers (plots)
  - define dates of interest 
  
3. workflowFunctions
  - These container function just call processing_functions to generate a specific product. As of 09012023 `processSpec` is the active method. 
  
4. processingFunctings
  - Specific tasks that are called within the processing workflow functions. 
  
*note* : scripts and functions in the source folder shouldn't need to be edited directly outside of troubleshooting steps. 

