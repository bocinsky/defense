## code to prepare `DATASET` dataset goes here

##### National Park Spatial Polygon
meve <-
  "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Protected_Areas_Fee_Manager/FeatureServer/0/query" %>%
  httr::modify_url(
    query = list(
      f = "json",
      where = "Unit_Nm='Mesa Verde National Park'",
      returnGeometry = "true"
    )
  ) %>%
  sf::read_sf() %>%
  sf::st_transform("EPSG:26912")

meve %<>%
  sf::st_buffer(1000) %>%
  FedData::get_ned("meve2") %>%
  raster::projectRaster(res = c(30,30),
                        crs = sp::CRS("EPSG:26912")) %>%
  raster::crop(meve)


usethis::use_data(meve, overwrite = TRUE)
