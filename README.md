# CHE

The script below calculates uses a vector(point and polygon) and raster data types(lidar dsm and dtm)
to estimate six types of zonal statistics. Mean, Median, Mode, sd, maximum and minimum. 

Libraries used are sf, terra,tidyverse,tictoc


The script picks a downloaded lidar tile and extracts it to a folder. Creates a vrt of raster tiles which are computed to the extent of the  vector data polygon.

The polygon is then converted to spatial vector to compute the zonal statistics within the range of the polygon data. Height at ground level(dtm), height at canopy level, and the differences between both heights

## Quality Checks 

After clipping the polygon to the extent of the area, recheck the validity of resulting polygon geometries to ensure that all other collected geometries like points are removed.