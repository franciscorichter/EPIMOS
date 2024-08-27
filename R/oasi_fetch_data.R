#' Fetch Locations Data
#'
#' This function retrieves location data from the OASI API based on the specified domain.
#'
#' @param domain A character string representing the domain for which to fetch location data.
#' @return A data frame containing the locations data if the request is successful; otherwise, NULL.
#' @examples
#' \dontrun{
#' locations <- fetch_locations_data("example_domain")
#' print(locations)
#' }
#' @export
fetch_locations_data <- function(domain) {
  base_url <- "http://www.oasi.ti.ch/web/rest/locations"
  params <- list(domain = domain)
  
  response <- GET(base_url, query = params)
  
  if (response$status_code == 200) {
    return(fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE))
  } else {
    warning("Failed to get locations data. Status code:", response$status_code)
    return(NULL)
  }
}

#' Fetch Available Parameters for a Location
#'
#' This function retrieves the available parameters for a specified location from the OASI API.
#'
#' @param domain A character string representing the domain for which to fetch parameters.
#' @param location_code A character string representing the location code for which to fetch parameters.
#' @return A data frame containing the available parameters if the request is successful; otherwise, NULL.
#' @examples
#' \dontrun{
#' parameters <- fetch_parameters_data("example_domain", "location_code")
#' print(parameters)
#' }
#' @export
fetch_parameters_data <- function(domain, location_code) {
  base_url <- "http://www.oasi.ti.ch/web/rest/parameters"
  params <- list(domain = domain, location = location_code)
  
  response <- GET(base_url, query = params)
  
  if (response$status_code == 200) {
    return(fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE))
  } else {
    warning("Failed to get parameters data. Status code:", response$status_code)
    return(NULL)
  }
}

#' Fetch Time Series Data
#'
#' This function retrieves time series data for a specified parameter, location, and time range from the OASI API.
#'
#' @param domain A character string representing the domain for which to fetch data.
#' @param location_code A character string representing the location code for which to fetch data.
#' @param parameter A character string representing the parameter to fetch.
#' @param resolution A character string representing the resolution of the data (e.g., "daily", "hourly").
#' @param from_date A character string representing the start date for the data (in "YYYY-MM-DD" format).
#' @param to_date A character string representing the end date for the data (in "YYYY-MM-DD" format).
#' @return A character string containing the time series data in CSV format if the request is successful; otherwise, NULL.
#' @examples
#' \dontrun{
#' data <- fetch_time_series_data("example_domain", "location_code", "parameter", "daily", "2024-01-01", "2024-01-31")
#' print(data)
#' }
#' @export
fetch_time_series_data <- function(domain, location_code, parameter, resolution, from_date, to_date) {
  base_url <- "http://www.oasi.ti.ch/web/rest/measure/csv"
  params <- list(
    domain = domain,
    location = location_code,
    parameter = parameter,
    resolution = resolution,
    from = from_date,
    to = to_date
  )
  
  response <- GET(base_url, query = params)
  
  if (response$status_code == 200) {
    return(content(response, as = "text", encoding = "UTF-8"))
  } else {
    warning("Failed to get data. Status code:", response$status_code)
    return(NULL)
  }
}




#' Process Locations Data
#'
#' This function processes location data by separating locations based on the availability of the "Q" parameter.
#' It fetches the locations data, checks each location for the "Q" parameter, and categorizes the locations into
#' two groups: those with the "Q" parameter and those without. It then prints and returns the data for locations
#' with the "Q" parameter.
#'
#' @param domain A character string representing the domain for which to process locations data.
#' @return A data frame containing the locations that have the "Q" parameter available. If the data fetching fails, returns NULL.
#' @examples
#' \dontrun{
#' q_locations <- process_locations("example_domain")
#' print(q_locations)
#' }
#' @export
process_locations <- function(domain) {
  locations_data <- fetch_locations_data(domain = domain)
  
  # Initialize lists to hold locations data
  q_locations <- list()
  other_locations <- list()
  
  # Process each location to check for Q parameter availability
  if (!is.null(locations_data)) {
    for (i in seq_len(nrow(locations_data))) {
      location_code <- locations_data$code[i]
      location_name <- locations_data$name[i]
      
      # Fetch parameters data
      parameters_data <- fetch_parameters_data(domain = domain, location_code = location_code)
      
      if (!is.null(parameters_data)) {
        # Check if Q parameter is available
        if (any(parameters_data$code == "Q")) {
          q_locations <- append(q_locations, list(locations_data[i, ]))
        } else {
          other_locations <- append(other_locations, list(locations_data[i, ]))
        }
      }
    }
    
    # Convert lists to data frames
    q_locations_df <- bind_rows(q_locations)
    other_locations_df <- bind_rows(other_locations)
    
    # Display the separated data
    message("Locations with Q Parameter:")
    print(q_locations_df)
    
    message("Other Locations:")
    print(other_locations_df)
    
    return(q_locations_df)
  } else {
    message("Failed to fetch location data.")
    return(NULL)
  }
}

