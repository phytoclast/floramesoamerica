#writeRaster(dist, 'output/dist.tif')


#st_write(sfpoints2, ".", "output/sfpoints2", driver="ESRI Shapefile")

#how to make a sf polygon

p1 <- rbind(c(-180,-20), c(-140,56), c(10, 0), c(-140,-60), c(-180,-20))
hole <- rbind(c(-150,-20), c(-100,-10), c(-110,20), c(-150,-20))
p1 <- list(p1, hole)
p2 <- list(rbind(c(-10,0), c(140,60), c(160,0), c(140,-55), c(-10,0)))
p3 <- list(rbind(c(-125,0), c(0,60), c(40,5), c(15,-45), c(-125,0)))
pols <- st_sf(value = c(1,2,3),
              geometry = st_sfc(lapply(list(p1, p2, p3), st_polygon)))
r <- raster(pols, res = 1)
r <- fasterize(pols, r, field = "value", fun="sum")




#sppredict <- as_Spatial(sfpredict)
#Twlowres <- aggregate(Tw, fact=2, fun=mean)


#plot(Twlowres)
#res(Twlowres)
