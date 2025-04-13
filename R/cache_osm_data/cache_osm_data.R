cache_osm_data <- function(
  osm_path = getOption("global.osm_path")
) {
  if (!fs::dir_exists(osm_path)) {
    fs::dir_create(osm_path, recurse = TRUE)
  }
  Sys.setenv(OSMEXT_DOWNLOAD_DIRECTORY = osm_path)

  valencia_match = osmextract::oe_match("Valencia")

  valencia_pbf = osmextract::oe_download(
    file_url = valencia_match$url,
    file_size = valencia_match$file_size,
    download_directory = osm_path,
    provider = "geofabrik"
  )

  return(valencia_pbf)
}
