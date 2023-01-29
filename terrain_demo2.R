library(terra)
library(hillshader)
library(rayshader)
library(raster)
library(rayrender)
library(magick)
library(rgl)

sm_tab1r <- rast("table.tif")
sources(sm_tab1r)
hasValues(sm_tab1r)
plot(sm_tab1r, main='Small Loess Table')

slope <- terrain(sm_tab1r, v='slope', unit="radians")
aspect <- terrain(sm_tab1r, v='aspect', unit="radians")
hill <- shade(slope, aspect, 40, 0)
plot(hill, col=grey(0:100/100), legend=FALSE, main='Small Loess Table')

localtif = raster::raster("table.tif")
table_mat = raster_to_matrix(localtif)

table_mat %>%
  # Create hillshade layer using
  # ray-tracing
  ray_shade() %>%
  # Add ambient shading
  add_shadow_2d(
    ambient_shade(
      heightmap = table_mat
    )
  )

table_mat %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(table_mat, texture = "desert", 
                           zscale=1, colorintensity = 5), alphalayer=0.3) %>%
  add_shadow(lamb_shade(table_mat,zscale = 6),0) %>%
  plot_map()

save <- getOption("rgl.useNULL")
options(rgl.useNULL=TRUE)

table_mat %>%
  sphere_shade(texture = "imhof2", colorintensity = 3.0) %>%
  add_shadow(ray_shade(table_mat, zscale = 20), 0.5) %>%
  add_shadow(ambient_shade(table_mat), 0) %>%
  plot_3d(table_mat, zscale=1)

widget<-rglwidget()
if (interactive())
  widget 

Sys.sleep(0.2)
render_snapshot()

table_mat %>%
  height_shade %>%
  add_overlay(sphere_shade(table_mat, texture = "desert", 
                           zscale=1, colorintensity = 3), alphalayer=0.5) %>%
  add_shadow(ray_shade(table_mat, zscale = 20), 0.5) %>%
  add_shadow(ambient_shade(table_mat), 0) %>%
  plot_3d(table_mat, zscale = 1.0, windowsize = c(1000, 800))
widget<-rglwidget()
if (interactive())
  widget 

Sys.sleep(0.2)
render_snapshot()
