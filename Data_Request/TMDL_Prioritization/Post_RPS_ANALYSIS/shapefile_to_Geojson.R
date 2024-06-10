install.packages("geojsonsf")


shapefile_path <- '~/Documents/GitHub/Data_Request/TMDL_Prioritization/RPS Shiny App/HUC12_Shapefile/Watersheds_Area.shp'
huc12_poly <- st_read(shapefile_path, stringsAsFactors = FALSE)
huc12_poly_target=huc12_poly[huc12_poly$HUC_12%in%All_RPS_Results$HUC_12,]

library(geojsonsf)
geojson_data <- sf_geojson(huc12_poly_target)

library(rmapshaper)
county_json_simplified <- ms_simplify(geojson_data)


writeLines(county_json_simplified, "huc12_geojson.geojson")
