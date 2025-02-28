# Demonstration of terrain analysis and visualization in R.
# For more advanced visualization with rayshader, see
# sample script on Canvas

#lines that start with # are comments, not code

#general package for geospatial work in R
library(terra)
#mapping package
library(tmap)
#linked packages for working with soil survey data
#not used in this Binder notebook version
# library(aqp)
# library(soilDB)
# library(sharpshootR)

#read in a DEM and make a quick plot
Mondeaux_dtm <- rast("Mondeaux_dtm.tif")
plot(Mondeaux_dtm)

#read in a vector file (Ice Age Trail route)
#and convert to same projection as DEM, then crop
#to match the DEM
IAT<-vect("IAT.shp")
IAT_proj<-project(IAT, Mondeaux_dtm)
e<-ext(Mondeaux_dtm)
IAT_projc<-crop(IAT_proj, e)
IAT_points<-as.points(IAT_projc)

#read in a canopy height layer (DSM-DTM) for the same area
#and make a quick plot

Mondeaux_CH <- rast("Mondeaux_CH.tif")
plot(Mondeaux_CH)

#clip out a new smaller DEM
e2<-c(698000,700000,5019200,5020500)
Mondeaux_dtm_sm<-crop(Mondeaux_dtm, e2)
plot(Mondeaux_dtm_sm)

#clip the canopy height layer and IAT shapefile as well
Mondeaux_CH_sm<-crop(Mondeaux_CH, ext(Mondeaux_dtm_sm))
plot(Mondeaux_CH_sm)
IAT_projc_sm<-crop(IAT_projc, ext(Mondeaux_dtm_sm))
IAT_points_sm<-as.points(IAT_projc_sm)

#make slope and aspect maps and a hillshade from the original
#and smaller DTM. Using qgistools for this gives you more
#control over the hillshade but won't work in an online
#notebook format
dtm_slope<-terrain(Mondeaux_dtm, "slope", unit="radian")
dtm_aspect<-terrain(Mondeaux_dtm, "aspect", unit="radian")
dtm_shade<-shade(dtm_slope, dtm_aspect, angle=30, direction=300)

dtm_slope_sm<-terrain(Mondeaux_dtm_sm, "slope", unit="radian")
dtm_aspect_sm<-terrain(Mondeaux_dtm_sm, "aspect", unit="radian")
dtm_shade_sm<-shade(dtm_slope_sm, dtm_aspect_sm, angle=30, direction=300)

#now plot the full area with elevation or slope shaded, and with Ice Age Trail
#as a line or as points created from the line
plot(dtm_shade, 
     main="Mondeaux area DTM",
     cex.main=1,
     font.main=1,
     col=grey(100:20/100), 
     legend=FALSE, 
     mar=c(3,3,3,7))
terrain_colors=colorRampPalette(c('#2c7bb6','#abd9e9','#ffffbf','#fdae61','#d7191c'))
plot(Mondeaux_dtm, 
     col=terrain_colors(8),
     plg=list(cex=1),
     mar=c(3,3,3,7),
     alpha=0.6,
     pax=list(ticks=FALSE, labels=FALSE),
     add=TRUE)
#to plot slope, comment out the above and uncomment this
# plot(dtm_slope, 
#      col=terrain_colors(8),
#      plg=list(cex=1),
#      mar=c(3,3,3,7),
#      alpha=0.6,
#      pax=list(ticks=FALSE, labels=FALSE),
#      add=TRUE)
#uncomment this to plot IAT as a line
# plot(IAT_projc, 
#      col="black",
#      lwd = 3.0,
#      add=TRUE)
plot(IAT_points, 
     col="black",
     cex = 0.5,
     add=TRUE)

#same but for small area DTM
plot(dtm_shade_sm, 
     main="Mondeaux area DTM",
     cex.main=1,
     font.main=1,
     col=grey(100:20/100), 
     legend=FALSE, 
     mar=c(3,3,3,7))
terrain_colors=colorRampPalette(c('#2c7bb6','#abd9e9','#ffffbf','#fdae61','#d7191c'))
plot(Mondeaux_dtm_sm, 
     col=terrain_colors(8),
     plg=list(cex=1),
     mar=c(3,3,3,7),
     alpha=0.6,
     pax=list(ticks=FALSE, labels=FALSE),
     add=TRUE)
#to plot slope, comment out the above and uncomment this
# plot(dtm_slope_sm, 
#      col=terrain_colors(8),
#      plg=list(cex=1),
#      mar=c(3,3,3,7),
#      alpha=0.6,
#      pax=list(ticks=FALSE, labels=FALSE),
#      add=TRUE)
#uncomment this to plot IAT as a line
# plot(IAT_projc_sm, 
#      col="black",
#      lwd = 3.0,
#      add=TRUE)
plot(IAT_points_sm, 
     col="black",
     cex = 0.5,
     add=TRUE)

#make a plot of canopy height over a hillshade for the
#smaller area
canopy_colors=colorRampPalette(c('#ffffcc','#c2e699','#78c679','#31a354','#006837'))

plot(dtm_shade_sm, 
     main="Mondeaux Area Canopy Height (m)",
     cex.main=1,
     font.main=1,
     col=grey(100:10/100), 
     legend=FALSE, 
     mar=c(3,3,3,7))
plot(Mondeaux_CH_sm, 
     col=canopy_colors(8),
     plg=list(cex=1),
     mar=c(3,3,3,7),
     alpha=0.8,
     pax=list(ticks=FALSE, labels=FALSE),
     add=TRUE)
plot(IAT_points_sm, 
     col="black",
     cex = 0.5,
     add=TRUE)


#create a dataframe recording canopy height along the Ice Age Trail
IAT_canopy<-extract(Mondeaux_CH, IAT_points, method="bilinear", xy=TRUE)

#also extract elevation along the Ice Age Trail, to the same dataframe
IAT_canopy$elevation<-extract(Mondeaux_dtm, IAT_points, method="bilinear")

#add distance column to IAT_canopy and calculate distance from x, y coordinates
IAT_canopy[,'distance']=NA
IAT_canopy$distance[1]=0
for (i in 2:824){
  IAT_step<-(((IAT_canopy$x[i]-IAT_canopy$x[i-1])^2)+
               ((IAT_canopy$y[i]-IAT_canopy$y[i-1])^2))^0.5
  IAT_canopy$distance[i]<-IAT_canopy$distance[i-1]+IAT_step
}

#make a simple plot of canopy height vs distance
plot(IAT_canopy$distance, IAT_canopy$Band_1, pch=19, type="l", xlab="Distance (m)", 
     ylab="Canopy Height (m)")
#make a simple plot of elevation vs distance
plot(IAT_canopy$distance, IAT_canopy$elevation$Mondeaux_dtm, pch=19, type="l", xlab="Distance (m)", 
     ylab="Elevation (m)")

#same for smaller area
#now create a dataframe recording canopy height along the Ice Age Trail
IAT_canopy_sm<-extract(Mondeaux_CH_sm, 
                       IAT_points_sm, 
                       method="bilinear", xy=TRUE)

#also extract elevation along the Ice Age Trail, to the same dataframe
IAT_canopy_sm$elevation<-extract(Mondeaux_dtm_sm, 
                                 IAT_points_sm, 
                                 method="bilinear")

#add distance column to IAT_canopy and calculate distance from x, y coordinates
IAT_canopy_sm[,'distance']=NA
IAT_canopy_sm$distance[1]=0
for (i in 2:370){
  IAT_step<-(((IAT_canopy_sm$x[i]-IAT_canopy_sm$x[i-1])^2)+
               ((IAT_canopy_sm$y[i]-IAT_canopy_sm$y[i-1])^2))^0.5
  IAT_canopy_sm$distance[i]<-IAT_canopy_sm$distance[i-1]+IAT_step
}

#make a simple plot of canopy height vs distance
plot(IAT_canopy_sm$distance, IAT_canopy_sm$Band_1, 
     pch=19, type="l", xlab="Distance (m)", 
     ylab="Canopy Height (m)")

#make a simple plot of elevation vs distance
plot(IAT_canopy_sm$distance, 
     IAT_canopy_sm$elevation$Mondeaux_dtm, 
     pch=19, type="l", xlab="Distance (m)", 
     ylab="Elevation (m)")

#The spatial queries to external databases that are in the code
#below do not seem to work within Binder. They are here to
#demonstrate how to do these, but this script just reads in
#the final shape file of IAT points with soil properties
#added

#use the smaller set of points along the Ice Age Trail
#to extract soil survey information. Start with mapunits,
#the basic unit of soil survey, used to label polygons.
#Each mapunit can have two or more components, which
#are series of similar soils.
# m <- SDA_spatialQuery(IAT_points_sm, what = 'mukey', byFeature = TRUE)

#now use mapunits to extract average properties for the
#components in each map unit
# m2<-get_SDA_property(property=c("om_r", "sandtotal_r"),bottom_depth=20, 
#                        method="Weighted Average",mukeys=m$mukey)
# #with match() and cbind() we can add properties to each
# #point along the trail route
# m$sandtotal_r[1:370]<-m2$sandtotal_r[match(m$mukey[1:370], m2$mukey)]
# m$om_r[1:370]<-m2$om_r[match(m$mukey[1:370], m2$mukey)]
# m_points_sm<-cbind(IAT_points_sm, m)

#make a quick plot of the IAT points shaded by %sand,
#then if they look okay, save as a shapefile
#plot(m_points_sm, "sandtotal_r", type="continuous")

#now make some plots with tmap, including points shaded
#by soil properties. Uses the smaller area but would
#work for the larger area with some modification of the
#above code

#reads in shapefile created with the above code
#only necessary in this Binder notebook version
m_points_sm<-vect("m_points_sm.shp")

#plot the trail with sand content symbols
tmap::tm_shape(dtm_shade_sm) +
  tmap::tm_raster(
    col.scale = tmap::tm_scale_continuous(
      values = "greys",
      midpoint = NA),
    col.legend = tmap::tm_legend(
      show=FALSE
    )
  ) +
  tm_shape(m_points_sm)+
  tm_symbols(size = 0.4,
             lwd = 0.5,
             fill = "sandtotal_r",
             fill.scale = tmap::tm_scale_continuous(
               values = 
                 c('#006837','#31a354','#78c679','#c2e699','#ffffcc')),
             fill.legend = tmap::tm_legend(
               "%Sand"
             ))+
  tmap::tm_graticules(
    labels.size = 0.6,
    n.x=3,
    n.y=3,
    lines=FALSE
  ) +
  tmap::tm_compass() +
  tmap::tm_layout(
    scale = 1.0
  )

#plot the trail with OM content symbols
tmap::tm_shape(dtm_shade_sm) +
  tmap::tm_raster(
    col.scale = tmap::tm_scale_continuous(
      values = "greys",
      midpoint = NA),
    col.legend = tmap::tm_legend(
      show=FALSE
    )
  ) +
  
  #   ) +
  tm_shape(m_points_sm)+
  tm_symbols(size = 0.4,
             lwd = 0.5,
             fill = "om_r",
             fill.scale = tmap::tm_scale_continuous(
               values = 
                 c('#ffffd4','#fed98e','#fe9929','#d95f0e','#993404')),
             fill.legend = tmap::tm_legend(
               "% OM (0-20cm)"
             ))+
  tmap::tm_graticules(
    labels.size = 0.6,
    n.x=3,
    n.y=3,
    lines=FALSE
  ) +
  tmap::tm_compass() +
  tmap::tm_layout(
    scale = 1.0
  )

#This section gets information on the profiles of all
#the components (distinct types of soil, roughly) along
#the IAT in this segment. Much of the code and commenting
#here is by Dylan Beaudette, NRCS
sql <- sprintf(
  "SELECT mukey, cokey, compname, compkind, comppct_r 
  FROM component 
  WHERE mukey IN %s 
  --AND majcompflag = 'Yes'
  AND compkind != 'Miscellaneous area'
  ", format_SQL_in_statement(as.integer(m3$mukey))
)
# send to SDA, result is a data.frame
s <- SDA_query(sql)
# get OSD morphology + extended summaries 
osd <- fetchOSD(unique(s$compname), extended = TRUE)
# check out results
str(osd, 1)
# convert horizon boundary distinctness -> vertical distance
# see manual page
osd$SPC$hzd <- hzDistinctnessCodeToOffset(
  osd$SPC$distinctness, 
  codes = c('very abrupt', 'abrubt', 'clear', 'gradual', 'diffuse')
)

#The information extracted in the above steps is in the form of a
#soil profile collection (SPC), an object in R designed for storing
#information on soil profiles: A group of profiles, each with a set
#of horizons, and properties assigned to horizons or to whole profiles

#The following lines use the SoilTaxonomyDendrogram function to create a 
#set of profile sketches including all profiles in the SPC
#The sketches are arranged using a dendrogram, a "tree" in which
#soils that have more similar classifications are placed closer to
#each other, on the same "branches" of the tree. Could also
#classify and arrange the soils by their properties

SoilTaxonomyDendrogram(
  spc = osd$SPC, 
  y.offset = 0.4, 
  rotationOrder = profile_id(osd$SPC)[order(osd$SPC$subgroup)],
  max.depth = 180,
  scaling.factor = 0.012, 
  cex.taxon.labels = 0.75,
  cex.id = 0.85,
  cex.names = 0.75,
  width = 0.3, 
  name.style = 'center-center', 
  plot.depth.axis = TRUE,
  axis.line.offset = -3.5,
  hz.distinctness.offset = 'hzd'
)


