


library(terra)
library(raster)
library(tidyverse)
library(gdalraster)
library(sf)
library(plyr)
library(tictoc)

## read building footprints
bfprint <- st_read("clipped building footprints.gpkg")

## read uprns
uprn <-  st_read("uprn clipped.geojson")

## read postcodes
posc <- st_read("2d_3d.gpkg")

#### unzip tif dsm files


# get all the zip files
zipF <- list.files(path = ".", pattern = "*.zip", full.names = TRUE)

# unzip all your files
ldply(.data = zipF, .fun = unzip, exdir =".")

#merge raster layer
##dsm  navigate to where dsm tifs are and create a vrt
ff <- list.files(path =".", pattern = '\\.tif$', full.names = TRUE) 
v <- vrt(ff, "dsm.vrt")
#writeRaster(v, "_dsm.tif")


#### unzip tif dtm files

# get all the zip files
zipF <- list.files(path = ".", pattern = "*.zip", full.names = TRUE)

# unzip all your files
ldply(.data = zipF, .fun = unzip, exdir =".")

##dtm navigate to where dtm tifs are and create a vrt
ff1 <- list.files(path =".", pattern = '\\.tif$', full.names = TRUE) 
v1 <- vrt(ff1, "dtm.vrt")
#writeRaster(v1, "_dtm.tif")

## for v  which is dsm
## find extent of bbox to use for clips
ext <- st_bbox(v) %>%
  st_as_sfc() %>%
  st_as_sf()



### with respect to the clip [ ] clip and st_intersection works differently because
### [ ]clip returns the whole postcode area irrespective of the cut boundary of the extent box 
### whereas st_intersection returns the postcode area wrt the cut boundary
## from the extent box
## clip to dsm extent
### as a check write results from  [] clip and st_intersection clip and view in QGIS 
## precaution : this reduces duplicated uprns for every tile used as eventually
### all tiles will be merged 

#bfprint.clip <-  bfprint[ext,]
bfprint.clip  <- st_intersection(bfprint,ext)

## clip to postcode extent 
posc.st <- st_transform(posc, crs=st_crs(ext))
#posc.clip <-  posc.st[ext,]
posc.clip <- st_intersection(posc.st,ext)
#st_write(posc.clip, "squareclip.gpkg")

#t <- st_intersection(posc.st,ext)
 #st_write(t, "int.gpkg")

## clip to uprn extent 
#uprn.clip <-  uprn[ext,]
uprn.clip <- st_intersection(uprn,ext) 

tic()
uprn.pcode <- uprn.clip %>%
  st_transform(., crs = 3857) %>%
  dplyr::mutate(UPRN_X_3857 = unlist(map(.$geometry,1)),
         UPRN_Y_3857 = unlist(map(.$geometry,2))) %>%
  dplyr::rename("UPRN_X_27700"="X_COORDINATE", "UPRN_Y_27700"="Y_COORDINATE",
         "UPRN_X_4326"="LONGITUDE","UPRN_Y_4326"="LATITUDE") %>%
  .[,c(1,2,3,5,4,6,7,8)] %>%
  st_transform(., crs = st_crs(posc.clip)) %>%
  st_join(dplyr::select(posc.clip,Parent.Postcode,Child.Postcode)) %>%
  dplyr::rename("Parent Postcode"="Parent.Postcode","Child Postcode"="Child.Postcode")
toc()

## zonal statistics
#z <- extract(v, bfprint.clip)
#unlist(lapply(v, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))


bb <- bfprint.clip
#st_geometry(bb) <- st_collection_extract(x = st_geometry(bb), 
                                           #type = "POLYGON")#test <- as(test, "Spatial")
#bf <- as(bfprint.clip, "Spatial")
#bf <- sf:::as_Spatial(bfprint.clip$geom)
#as(sf_join, "Spatial")
bbbbb <- st_collection_extract(bb, "POLYGON")

#st_write(bfprint.clip, "svc.gpkg")
### zonal statistics for dsm 
tic()
dsm_mean <- terra::extract(v, vect(bbbbb), 'mean') %>%
  dplyr::rename("dsm_mean"="dsm")
toc()

tic()
dsm_median <- terra::extract(v, vect(bbbbb), 'median') %>%
  dplyr::rename("dsm_median"="dsm")
toc()

tic()
dsm_min <- terra::extract(v, vect(bbbbb), 'min') %>%
  dplyr::rename("dsm_min"="dsm")
toc()

tic()
dsm_max <- terra::extract(v, vect(bbbbb), 'max') %>%
  dplyr::rename("dsm_max"="dsm")
toc()

tic()
dsm_sd <- terra::extract(v, vect(bbbbb), 'sd') %>%
  dplyr::rename("dsm_sd"="dsm")
toc()

tic()
dsm_mode <- terra::extract(v, vect(bbbbb), 'modal') %>%
  dplyr::rename("dsm_mode"="dsm")
toc()

## merge all dsm statistics
tic()
dsm_all <- bbbbb[,1] %>%
  dplyr::rename("bfprint_ID"="ID") %>%
  cbind(dsm_mean,dsm_median=dsm_median[,2]) %>%
  cbind(dsm_min=dsm_min[,2]) %>%
  cbind(dsm_max=dsm_max[,2]) %>%
  cbind(dsm_sd=dsm_sd[,2]) %>%
  cbind(dsm_mode=dsm_mode[,2])
toc()

st_write(dsm_all, "dsm_all_TQ04.gpkg")

## for v1  which is dtm
## find extent of bbox to use for clips
#ext <- st_bbox(v1) %>%
  #st_as_sfc() %>%
  #st_as_sf()

## zonal statistics for dtm

tic()
dtm_mean <- terra::extract(v1, vect(bbbbb), 'mean') %>%
  dplyr::rename("dtm_mean"="dtm")
toc()

tic()
dtm_median <- terra::extract(v1, vect(bbbbb), 'median') %>%
  dplyr::rename("dtm_median"="dtm")
toc()

tic()
dtm_min <- terra::extract(v1, vect(bbbbb), 'min') %>%
  dplyr::rename("dtm_min"="dtm")
toc()

tic()
dtm_max <- terra::extract(v1, vect(bbbbb), 'max') %>%
  dplyr::rename("dtm_max"="dtm")
toc()

tic()
dtm_sd <- terra::extract(v1, vect(bbbbb), 'sd') %>%
  dplyr::rename("dtm_sd"="dtm")
toc()

tic()
dtm_mode <- terra::extract(v1, vect(bbbbb), 'modal') %>%
  dplyr::rename("dtm_mode"="dtm")
toc()
toc()

## merge all dtm zonal ststistics
tic()
dtm_all <- dtm_mean %>%
  cbind(dtm_median=dtm_median[,2]) %>%
  cbind(dtm_min=dtm_min[,2]) %>%
  cbind(dtm_max=dtm_max[,2]) %>%
  cbind(dtm_sd=dtm_sd[,2]) %>%
  cbind(dtm_mode=dtm_mode[,2])
toc()

st_write(dtm_all, "dtm_all_TQ04.gpkg")

## get height from dsm and dtm 
tic()
v2 <- v-v1
toc()

## zonal statistics for height
tic()
height_mean <- terra::extract(v2, vect(bbbbb), 'mean') %>%
  dplyr::rename("height_mean"="dsm")
toc()

tic()
height_median <- terra::extract(v2, vect(bbbbb), 'median') %>%
  dplyr::rename("height_median"="dsm")
toc()

tic()
height_min <- terra::extract(v2, vect(bbbbb), 'min') %>%
  dplyr::rename("height_min"="dsm")
toc()

tic()
height_max <- terra::extract(v2, vect(bbbbb), 'max') %>%
  dplyr::rename("height_max"="dsm")
toc()

tic()
height_sd <- terra::extract(v2, vect(bbbbb), 'sd') %>%
  dplyr::rename("height_sd"="dsm")
toc()

tic()
height_mode <- terra::extract(v2, vect(bbbbb), 'modal') %>%
  dplyr::rename("height_mode"="dsm")
toc()


## merge all height statistics
tic()
height_all <- height_mean %>%
  cbind(height_median=height_median[,2]) %>%
  cbind(height_min=height_min[,2]) %>%
  cbind(height_max=height_max[,2]) %>%
  cbind(height_sd=height_sd[,2]) %>%
  cbind(height_mode=height_mode[,2])
toc()

## merge statistics from dsm, dtm and height 
dtm_dsm_height <- dsm_all %>%
  cbind(dplyr::select(dtm_all, -c(ID))) %>%
  cbind(dplyr::select(height_all, -c(ID))) %>%
  dplyr::select(.,-c(ID))
  
## join statistics from dsm, dtm, height to uprn  
tic()
uprn_pcode_dsm_dtm_height <- st_join(uprn.pcode, dtm_dsm_height)
toc()


## write output to geopackage
st_write(uprn_pcode_dsm_dtm_height,"TQ04_tile.gpkg")
