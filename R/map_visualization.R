library(httr)
library(jsonlite)
library(ggrepel)
library(sf)
library(leaflet)

# Define function to fetch water data
fetch_water_data <- function(domain, from_date, to_date, parameter = NULL) {
  base_url <- "http://www.oasi.ti.ch/web/rest/locations"
  params <- list(
    domain = domain,
    from = from_date,
    to = to_date
  )
  
  if (!is.null(parameter)) {
    params$parameter <- parameter
  }
  
  response <- GET(base_url, query = params)
  
  if (response$status_code == 200) {
    return(content(response, as = "text"))
  } else {
    print(paste("Failed to get data. Status code:", response$status_code, "Response:", content(response, as = "text")))
    return(NULL)
  }
}

# Example usage
data <- fetch_water_data(domain = "surfacewater", from_date = "2022-07-19", to_date = "2024-07-18", parameter = "Q")
if (!is.null(data)) {
  # Convert the JSON response to a data frame
  data <- fromJSON(data, flatten = TRUE)
  
  # Convert coordinates to numeric where appropriate
  data$coordinates.x <- as.numeric(data$coordinates.x)
  data$coordinates.y <- as.numeric(data$coordinates.y)
  data$coordinates.z <- as.numeric(data$coordinates.z)
  
  # Convert to an sf object with Swiss coordinate system (CH1903)
  data_sf <- st_as_sf(data, coords =  c("coordinates.x", "coordinates.y"), crs = 21781)
  
  # Transform to WGS84
  data_wgs84 <- st_transform(data_sf, crs = 4326)
  
  # Extract transformed coordinates
  data_wgs84$longitude <- st_coordinates(data_wgs84)[,1]
  data_wgs84$latitude <- st_coordinates(data_wgs84)[,2]
  
  # Create a leaflet map
  leaflet(data = data_wgs84) %>%
    addTiles() %>%
    addCircleMarkers(~longitude, ~latitude, popup = ~name, label = ~name, color = 'blue', radius = 5) #%>%
    #addMarkers(~longitude, ~latitude, label = ~name)
}
