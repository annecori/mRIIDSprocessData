

sierraleone <- sf::st_read("data/Geography/SierraLeone_shapefile/Sierra Leone.shp")
apply(sierraleone, 2, function(col) sum(!is.na(col)))
all.healthsites <- sf::st_read("data/Geography/facilities_shapefile/facilities.shp")
apply(all.healthsites, 2, function(col) sum(!is.na(col)))
