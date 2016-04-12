# Map of field work places
#  by Juan Carlos Rocha
# juan.rocha@su.se
# Stockholm, 160411


require(ggmap)

# get map from Google
map <- get_map('Cienaga Grande de Santa Marta', zoom=9, maptype = 'terrain', color='bw', source='osm')
class(map)

# get point coordinates from Google
fieldSites <- c('Taganga', 'Tasajeras, Magdalena', 'Buenavista, Sitionuevo', 'Las Flores, Barranquilla')
PopSize <- c(3000, # 3000 ppl Taganga from wikipedia
             0, # No info for Tasajera
             0, # No info for buenavista
             0) # no info for las flores

coords <- geocode(fieldSites)

ggmap(map) + geom_point(aes(x=lon, y=lat, colour='red', size=2), data=coords, alpha=0.5)
