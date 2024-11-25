library(shiny)
library(shinythemes)
library(shinycssloaders)
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(zoo)  # For linear interpolation
library(mgcv) # For GAM
library(shinyjs)

# --- Functions ---

# Fetch locations data
fetch_locations_data <- function(domain) {
  base_url <- "http://www.oasi.ti.ch/web/rest/locations"
  params <- list(domain = domain)
  response <- GET(base_url, query = params)
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
    if (!"name" %in% names(data) || !"code" %in% names(data)) {
      warning("Invalid data structure received from locations API.")
      return(NULL)
    }
    return(data)
  } else {
    warning("Failed to get locations data. Status code:", response$status_code)
    return(NULL)
  }
}

# Fetch time series data
fetch_time_series_data <- function(domain, location_code, parameter, resolution, from_date, to_date) {
  base_url <- "http://www.oasi.ti.ch/web/rest/measure/csv"
  params <- list(
    domain = domain,
    location = location_code,
    parameter = parameter,
    resolution = resolution,
    from = format(as.Date(from_date), "%Y-%m-%d"),
    to = format(as.Date(to_date), "%Y-%m-%d")
  )
  
  response <- GET(base_url, query = params)
  
  if (response$status_code == 200) {
    return(content(response, as = "text", encoding = "UTF-8"))
  } else {
    warning("Failed to get data. Status code:", response$status_code)
    return(NULL)
  }
}

# Process and append data
process_and_append_data <- function(response_data, parameter, location_name, data_frame) {
  data_lines <- strsplit(response_data, "\n")[[1]]
  data_lines <- data_lines[!grepl("^#", data_lines)]
  
  if (length(data_lines) > 0) {
    data_clean <- paste(data_lines, collapse = "\n")
    data_df <- read.csv(text = data_clean, sep = ";", header = TRUE)
    
    if ("data" %in% names(data_df) && parameter %in% names(data_df)) {
      data_df$data <- as.POSIXct(data_df$data, format="%d.%m.%Y %H:%M", tz = "UTC")
      if (all(is.na(data_df$data))) {
        warning("Date conversion failed. Check the date format.")
      }
      data_df[[parameter]] <- as.numeric(data_df[[parameter]])
      data_df$Location <- location_name
      data_frame <- rbind(data_frame, data_df)
    }
  }
  return(data_frame)
}

# Preprocess data (convert negative values to NA)
preprocess_data <- function(data_frame) {
  numeric_cols <- sapply(data_frame, is.numeric)
  data_frame[numeric_cols] <- lapply(data_frame[numeric_cols], function(x) ifelse(x < 0, NA, x))
  return(data_frame)
}

# Impute missing data based on selected method
impute_missing_data <- function(data_frame, method) {
  data_frame <- data_frame[order(data_frame$data), ]
  data_frame$time_num <- as.numeric(data_frame$data)
  
  if (method == "linear") {
    # Linear interpolation
    data_frame$Q_imputed <- zoo::na.approx(data_frame$Q, x = data_frame$time_num, na.rm = FALSE)
  } else if (method == "gam") {
    # Generalized Additive Model (GAM) imputation
    if (sum(!is.na(data_frame$Q)) > 2) {
      fit <- gam(Q ~ s(time_num), data = data_frame, na.action = na.exclude)
      data_frame$Q_imputed <- predict(fit, newdata = data_frame)
    } else {
      warning("Not enough data points for GAM imputation. No imputation applied.")
      data_frame$Q_imputed <- data_frame$Q
    }
  } else {
    stop("Unknown imputation method.")
  }
  return(data_frame)
}

# --- UI and server for the Shiny app ---

ui <- fluidPage(
  theme = shinytheme("flatly"),  # Modern UI theme
  useShinyjs(),
  
  # App title and header
  titlePanel(tags$div(
    img(src = "usi.png", height = "100px", style = "display: block; margin-left: auto; margin-right: auto;"),
    h1("OASI Environmental Data Viewer", align = "center")
  )),
  
  sidebarLayout(
    sidebarPanel(
      h4("Settings"),
      helpText("Use the options below to fetch and analyze environmental data."),
      
      selectInput("location", "Choose Location:", choices = NULL, selectize = TRUE),
      
      dateRangeInput("date_range", "Select Date Range:", 
                     start = "2021-01-01", 
                     end = Sys.Date(),
                     format = "yyyy-mm-dd"),
      
      hr(),
      h5("Data Handling"),
      selectInput("imputation_method", "Choose Imputation Method:", 
                  choices = list("Linear Interpolation" = "linear", 
                                 "Generalized Additive Model (GAM)" = "gam"),
                  selected = "linear"),
      
      hidden(actionButton("impute", "Impute Missing Data", class = "btn-success")),
      textOutput("missing_points"),
      hidden(downloadButton("download_imputed", "Download Imputed Data")),
      hr(),
      downloadButton("download_fetched", "Download Fetched Data"),
      br(), br(),
      
      # About explanation
      tags$div(
        h5("About"),
        p("This app allows you to visualize and process environmental data from OASI."),
        p("Developed in collaboration between USI and OASI to enable better data analysis and visualization."),
        p("For more information, visit ", a("OASI", href = "https://www.oasi.ti.ch", target = "_blank"), ".")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Visualization", 
                 withSpinner(plotOutput("data_plot"), color = "#0dc5c1")),
        tabPanel("Summary", 
                 h5("Data Summary"),
                 tableOutput("data_summary"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Initially hide buttons
  shinyjs::hide("impute")
  shinyjs::hide("download_imputed")
  shinyjs::hide("download_fetched")
  
  # Fetch locations and populate location dropdown
  locations_df <- fetch_locations_data("surfacewater")
  
  if (is.null(locations_df) || !"name" %in% names(locations_df)) {
    showModal(modalDialog(
      title = "Error",
      "Failed to fetch location data from the API.",
      easyClose = TRUE,
      footer = NULL
    ))
    return()
  } else {
    location_names <- locations_df$name[grepl("-", locations_df$name)]
    updateSelectInput(session, "location", choices = location_names)
  }
  
  # Reactive data fetching based on selected location and date range
  data_reactive <- reactive({
    req(input$location, input$date_range)
    location_code <- locations_df[locations_df$name == input$location, "code"]
    
    q_data_response <- fetch_time_series_data(
      domain = "surfacewater",
      location_code = location_code,
      parameter = "Q",
      resolution = "d",
      from_date = input$date_range[1],
      to_date = input$date_range[2]
    )
    
    if (is.null(q_data_response)) {
      showNotification("Failed to fetch time series data.", type = "error")
      return(NULL)
    }
    
    q_data <- process_and_append_data(q_data_response, "Q", input$location, data.frame())
    if (nrow(q_data) == 0) {
      showNotification("No data available for the selected parameters.", type = "warning")
      return(NULL)
    }
    
    q_data <- preprocess_data(q_data)
    q_data <- q_data[order(q_data$data), ]
    return(q_data)
  })
  
  # Generate and display the plot
  output$data_plot <- renderPlot({
    data <- data_reactive()
    req(data)
    
    ggplot(data, aes(x = data, y = Q)) +
      geom_point(color = "blue", na.rm = TRUE) +
      labs(title = paste("Q Time Series for Location:", input$location),
           x = "Date", y = "Q Values") +
      theme_minimal()
  })
  
  # Display data summary
  output$data_summary <- renderTable({
    data <- data_reactive()
    req(data)
    
    summary_data <- data.frame(
      "Location" = unique(data$Location),
      "Total Data Points" = nrow(data),
      "Start Date" = min(data$data, na.rm = TRUE),
      "End Date" = max(data$data, na.rm = TRUE),
      "Missing Points" = sum(is.na(data$Q))
    )
    return(summary_data)
  })
  
  # Monitor missing points and display impute button if necessary
  observe({
    data <- data_reactive()
    req(data)
    n_missing <- sum(is.na(data$Q))
    
    # Show number of missing points
    output$missing_points <- renderText({
      paste("Number of missing points:", n_missing)
    })
    
    # If missing data exists, show the imputation button
    if (n_missing > 0) {
      shinyjs::show("impute")
    } else {
      shinyjs::hide("impute")
    }
    
    # Show the fetched data download button
    if (!is.null(data) && nrow(data) > 0) {
      shinyjs::show("download_fetched")
    } else {
      shinyjs::hide("download_fetched")
    }
  })
  
  # Download fetched data
  output$download_fetched <- downloadHandler(
    filename = function() {
      paste("fetched_data_", gsub(" ", "_", input$location), ".csv", sep = "")
    },
    content = function(file) {
      data <- data_reactive()
      req(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Store the imputed data in a reactive variable
  imputed_data_reactive <- reactiveVal(NULL)
  
  # Perform imputation and update the plot
  observeEvent(input$impute, {
    data <- data_reactive()
    req(data)
    
    # Perform imputation using the selected method
    method <- input$imputation_method
    imputed_data <- impute_missing_data(data, method)
    imputed_data_reactive(imputed_data)  # Store the imputed data for download
    
    # Separate original and imputed data
    original_data <- imputed_data[!is.na(imputed_data$Q), ]  # Original non-missing data
    imputed_only <- imputed_data[is.na(imputed_data$Q), ]  # Missing values that were imputed
    
    # Plot original and imputed data
    output$data_plot <- renderPlot({
      ggplot() +
        geom_point(data = original_data, aes(x = data, y = Q, color = "Original"), size = 1, na.rm = TRUE) +
        geom_point(data = imputed_only, aes(x = data, y = Q_imputed, color = "Imputed"), size = 2, na.rm = TRUE) +
        labs(title = paste("Imputation for Location:", input$location),
             x = "Date", y = "Q Values") +
        scale_color_manual(values = c("Original" = "blue", "Imputed" = "red")) +
        theme_minimal()
    })
    
    # Show the download button after imputation
    shinyjs::show("download_imputed")
  })
  
  # Download imputed data
  output$download_imputed <- downloadHandler(
    filename = function() {
      paste("imputed_data_", gsub(" ", "_", input$location), ".csv", sep = "")
    },
    content = function(file) {
      imputed_data <- imputed_data_reactive()  # Get the imputed data
      req(imputed_data)
      write.csv(imputed_data, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


