# Map of field work places
#  by Juan Carlos Rocha
# juan.rocha@su.se
# Stockholm, 160411


require(ggmap)

# get map from Google
map <- get_map('Cienaga Grande de Santa Marta', zoom=10, maptype = 'roadmap', color='bw', source='google')
class(map)

# use a box
bbox <- attr(map, 'bb')
bbox <- bb2bbox(bbox)
# Modify manually bbox to get the range you want, check googlemaps for the right coords.
bbox2 <- c(-74.83926, 10.59559, -74.07983, 11.51344) # left, bottom, right, top 

# query google again with your coords
map <- get_map(bbox2, zoom=10, maptype = 'toner', color='bw', source='stamen')

# get point coordinates from Google
fieldSites <- c('Taganga', 'Tasajeras, Magdalena', 'Buenavista, Sitionuevo', 'Las Flores, Barranquilla')
PopSize <- c(3000, # 3000 ppl Taganga from wikipedia
             0, # No info for Tasajera
             0, # No info for buenavista
             0) # no info for las flores

coords <- geocode(fieldSites)

g <- ggmap(map) + geom_point(aes(x=lon, y=lat, colour='red', size=2), data=coords, alpha=0.5)

## Get Colombia also for context

Colombia <- get_map('Colombia', zoom=5, maptype = 'roadmap', color='bw', source='google')
c <- ggmap(Colombia)


# save.image(file='mapBEST.RData', safe=T)
