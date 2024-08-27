library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)

# Define function to fetch locations data
fetch_locations_data <- function(domain) {
  base_url <- "http://www.oasi.ti.ch/web/rest/locations"
  params <- list(domain = domain)
  
  response <- GET(base_url, query = params)
  
  if (response$status_code == 200) {
    return(fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE))
  } else {
    print(paste("Failed to get locations data. Status code:", response$status_code, "Response:", content(response, as = "text")))
    return(NULL)
  }
}

# Define function to fetch available parameters for a location
fetch_parameters_data <- function(domain, location_code) {
  base_url <- "http://www.oasi.ti.ch/web/rest/parameters"
  params <- list(domain = domain, location = location_code)
  
  response <- GET(base_url, query = params)
  
  if (response$status_code == 200) {
    return(fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE))
  } else {
    print(paste("Failed to get parameters data. Status code:", response$status_code, "Response:", content(response, as = "text")))
    return(NULL)
  }
}

# Fetch all locations for surfacewater domain
locations_data <- fetch_locations_data(domain = "surfacewater")

# Initialize lists to hold locations data
q_locations <- list()
other_locations <- list()

# Process each location to check for Q parameter availability
if (!is.null(locations_data)) {
  for (i in 1:nrow(locations_data)) {
    location_code <- locations_data$code[i]
    location_name <- locations_data$name[i]
    
    # Fetch parameters data
    parameters_data <- fetch_parameters_data(domain = "surfacewater", location_code = location_code)
    
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
  
  # Print the separated data
  print("Locations with Q Parameter:")
  print(q_locations_df)
  
  print("Other Locations:")
  print(other_locations_df)
} else {
  print("Failed to fetch location data.")
}

# Define function to process and append data
process_and_append_data <- function(response_data, parameter, location_name, data_frame) {
  data_lines <- strsplit(response_data, "\n")[[1]]
  data_lines <- data_lines[!grepl("^#", data_lines)]
  
  if (length(data_lines) > 0) {
    data_clean <- paste(data_lines, collapse = "\n")
    data_df <- read.csv(text = data_clean, sep = ";", header = TRUE)
    
    if ("data" %in% names(data_df) && parameter %in% names(data_df)) {
      data_df$data <- as.Date(data_df$data, format="%d.%m.%Y %H:%M")
      data_df[[parameter]] <- as.numeric(data_df[[parameter]])
      data_df$Location <- location_name
      data_frame <- rbind(data_frame, data_df)
    }
  }
  return(data_frame)
}

# Initialize data frames to hold time series data
q_data <- data.frame()

# Fetch time series data for locations with Q parameter
for (i in 1:nrow(q_locations_df)) {
  location_code <- q_locations_df$code[i]
  location_name <- q_locations_df$name[i]
  
  # Fetch Q parameter data
  q_data_response <- fetch_time_series_data(domain = "surfacewater", location_code = location_code, parameter = "Q", resolution = "d", from_date = "2022-01-19", to_date = "2024-07-18")
  
  if (!is.null(q_data_response)) {
    q_data <- process_and_append_data(q_data_response, "Q", location_name, q_data)
  }
}

# Print the combined Q data
print("Combined Q Data:")
print(q_data)

# Plot the combined Q data
ggplot(q_data, aes(x = data, y = Q, color = Location)) +
  geom_line() +
  labs(title = "River Flow (Q) Over Time for Multiple Locations",
       x = "Date",
       y = "Flow (Q) [m3/s]") +
  theme_minimal()



library(ggplot2)

# Ensure the correct data frame is being used
print("Checking q_locations_df structure:")
print(str(q_locations_df))

# Create the plot
ggplot(q_locations_df) + 
  geom_point(aes(x = coordinates.x, y = coordinates.y), color = "blue") + 
  geom_text(aes(x = coordinates.x, y = coordinates.y, label = name), hjust = 0, vjust = 1) + 
  labs(title = "Locations Plot", x = "X Coordinates", y = "Y Coordinates") + 
  theme_minimal()

