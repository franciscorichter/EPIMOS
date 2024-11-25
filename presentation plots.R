library(ggplot2)

# Assuming `q_data` contains the original data and we have filtered it to Ticino - Bellinzona


ticino_data <- q_data %>% filter(Location == "Ticino - Bellinzona")

ticino_data = ticino_data[1060:nrow(ticino_data),]

ggplot(ticino_data, aes(x = data, y = Q)) +
  geom_point() +
  labs(title = "Time Series of Q Value (Ticino - Bellinzona)", x = "Date", y = "Q Value") +
  theme_minimal()



library(forecast)

# Convert the 'Q' data to a time series object (assuming daily frequency)
ticino_ts <- ts(ticino_data$Q, frequency = 365, start = c(2021, as.numeric(format(min(ticino_data$data), "%j"))))

# Fit an ARIMA model
arima_model <- auto.arima(ticino_ts)

# Get the fitted values from the model
ticino_data$fitted <- fitted(arima_model)

# Check model summary (optional, to see the model parameters)
summary(arima_model)

ggplot(ticino_data, aes(x = data)) +
  geom_point(aes(y = Q), color = "black", size = 1) +  # Original data
  geom_line(aes(y = fitted), color = "blue", linetype = "solid", size = 0.2) +  # Fitted values
  labs(title = "Original vs Fitted Q Value (Ticino - Bellinzona)", x = "Date", y = "Q Value") +
  theme_minimal()





# Generate predictions for the missing data
num_missing <- sum(is.na(ticino_data$Q))  # Count of missing values

# Use the ARIMA model to predict the missing values (extract the predictions)
imputed_values <- predict(arima_model, n.ahead = num_missing)$pred  # Use only the $pred part

# Assign the imputed values where Q is missing
ticino_data$imputed <- ifelse(is.na(ticino_data$Q), imputed_values, NA)
ticino_data$imputed <- as.numeric(ticino_data$imputed)

# Plot with imputed values (red) on top of original and fitted values
ggplot(ticino_data, aes(x = data)) +
  geom_point(aes(y = Q), color = "black", size = 1) +  # Original data
  geom_line(aes(y = fitted), color = "blue", linetype = "dashed", size = 0.2) +  # Fitted values
  geom_point(data = ticino_data[!is.na(ticino_data$imputed), ],  # Only plot non-NA imputed values
             aes(y = imputed), color = "red", size = 2) +  # Imputed values in red
  labs(title = "Original, Fitted, and Imputed Q Values (Ticino - Bellinzona)", x = "Date", y = "Q Value") +
  theme_minimal()

q_data$data <- as.Date(q_data$data)

# Filter for the selected locations and only 2024 data
selected_locations_2024 <- q_data %>%
  filter(Location %in% c("Ticino - Bellinzona", 
                         "Morobbia - Giubiasco", 
                         "Moesa - Lumino", 
                         "Riale di Gnosca - Gnosca") & 
           format(data, "%Y") == "2024")

# Create the plot
ggplot(selected_locations, aes(x = data)) +
  # Data points for Ticino - Bellinzona
  geom_point(data = selected_locations %>% filter(Location == "Ticino - Bellinzona"), 
             aes(y = Q), color = "black", size = 2) +  
  # Curve for Morobbia - Giubiasco
  geom_line(data = selected_locations %>% filter(Location == "Morobbia - Giubiasco"), 
            aes(y = Q, color = "Morobbia - Giubiasco"), size = 0.3) +  
  # Curve for Moesa - Lumino
  geom_line(data = selected_locations %>% filter(Location == "Moesa - Lumino"), 
            aes(y = Q, color = "Moesa - Lumino"), size = 0.3) +  
  # Curve for Riale di Gnosca - Gnosca
  geom_line(data = selected_locations %>% filter(Location == "Riale di Gnosca - Gnosca"), 
            aes(y = Q, color = "Riale di Gnosca - Gnosca"), size = 0.3) +  
  # Labels and Titles
  labs(title = "Ticino - Bellinzona Data with Curves for Other Locations",
       x = "Date", y = "Q Value") +
  # Custom colors for lines with a palette that ensures visibility
  scale_color_manual(values = c("Morobbia - Giubiasco" = "purple", 
                                "Moesa - Lumino" = "green", 
                                "Riale di Gnosca - Gnosca" = "red")) +  
  theme_minimal() +
  theme(legend.title = element_blank())  # Removes legend title








# Create the plot
ggplot(selected_locations, aes(x = data)) +
  # Data points for Ticino - Bellinzona
  geom_point(data = selected_locations %>% filter(Location == "Ticino - Bellinzona"), 
             aes(y = Q), color = "black", size = 2) +  
  
  geom_line(data = selected_locations %>% filter(Location == "Ticino - Bellinzona"), 
             aes(y = Q), color = "blue", size = 0.5) +
  # Curve for Morobbia - Giubiasco
  geom_line(data = selected_locations %>% filter(Location == "Morobbia - Giubiasco"), 
            aes(y = Q, color = "Morobbia - Giubiasco"), size = 0.3) +  
  # Curve for Moesa - Lumino
  geom_line(data = selected_locations %>% filter(Location == "Moesa - Lumino"), 
            aes(y = Q, color = "Moesa - Lumino"), size = 0.3) +  
  # Curve for Riale di Gnosca - Gnosca
  geom_line(data = selected_locations %>% filter(Location == "Riale di Gnosca - Gnosca"), 
            aes(y = Q, color = "Riale di Gnosca - Gnosca"), size = 0.3) +  
  # Labels and Titles
  labs(title = "Ticino - Bellinzona Data with Curves for Other Locations",
       x = "Date", y = "Q Value") +
  # Custom colors for lines with a palette that ensures visibility
  scale_color_manual(values = c("Morobbia - Giubiasco" = "purple", 
                                "Moesa - Lumino" = "green", 
                                "Riale di Gnosca - Gnosca" = "red")) +  
  theme_minimal() +
  theme(legend.title = element_blank())  # Removes legend title









# Create the plot
ggplot(selected_locations, aes(x = data)) +
  # Data points for Ticino - Bellinzona
  geom_point(data = ticino_bellinzona_data, 
             aes(y = Q), color = "black", size = 2) +  
  
  geom_line(data = ticino_bellinzona_data, 
            aes(y = Q), color = "blue", size = 0.5) +
  
  # Imputed values in red for Ticino - Bellinzona
  geom_point(data = ticino_bellinzona_data %>% filter(!is.na(imputed)), 
             aes(y = imputed), color = "red", size = 2) +
  
  # Curve for Morobbia - Giubiasco
  geom_line(data = selected_locations %>% filter(Location == "Morobbia - Giubiasco"), 
            aes(y = Q, color = "Morobbia - Giubiasco"), size = 0.3) +  
  
  # Curve for Moesa - Lumino
  geom_line(data = selected_locations %>% filter(Location == "Moesa - Lumino"), 
            aes(y = Q, color = "Moesa - Lumino"), size = 0.3) +  
  
  # Curve for Riale di Gnosca - Gnosca
  geom_line(data = selected_locations %>% filter(Location == "Riale di Gnosca - Gnosca"), 
            aes(y = Q, color = "Riale di Gnosca - Gnosca"), size = 0.3) +  
  
  # Labels and Titles
  labs(title = "Ticino - Bellinzona Data with Imputed Points and Curves for Other Locations",
       x = "Date", y = "Q Value") +
  
  # Custom colors for lines with a palette that ensures visibility
  scale_color_manual(values = c("Morobbia - Giubiasco" = "purple", 
                                "Moesa - Lumino" = "green", 
                                "Riale di Gnosca - Gnosca" = "red")) +  
  theme_minimal() +
  theme(legend.title = element_blank())  # Removes legend title





# Assign imputed values to the selected_locations dataset
selected_locations <- selected_locations %>%
  left_join(ticino_data %>% select(data, imputed), by = "data")


# Create the plot with imputed values in red for Ticino - Bellinzona and curves for other locations
ggplot(selected_locations, aes(x = data)) +
  # Data points for Ticino - Bellinzona
  geom_point(data = selected_locations %>% filter(Location == "Ticino - Bellinzona"), 
             aes(y = Q), color = "black", size = 2) +  
  
  geom_line(data = selected_locations %>% filter(Location == "Ticino - Bellinzona"), 
            aes(y = Q), color = "blue", size = 0.5) +
  
  # Imputed values in red for Ticino - Bellinzona
  geom_point(data = selected_locations %>% filter(Location == "Ticino - Bellinzona" & !is.na(imputed)), 
             aes(y = imputed), color = "red", size = 2) +
  
  # Curve for Morobbia - Giubiasco
  geom_line(data = selected_locations %>% filter(Location == "Morobbia - Giubiasco"), 
            aes(y = Q, color = "Morobbia - Giubiasco"), size = 0.3) +  
  
  # Curve for Moesa - Lumino
  geom_line(data = selected_locations %>% filter(Location == "Moesa - Lumino"), 
            aes(y = Q, color = "Moesa - Lumino"), size = 0.3) +  
  
  # Curve for Riale di Gnosca - Gnosca
  geom_line(data = selected_locations %>% filter(Location == "Riale di Gnosca - Gnosca"), 
            aes(y = Q, color = "Riale di Gnosca - Gnosca"), size = 0.3) +  
  
  # Labels and Titles
  labs(title = "Ticino - Bellinzona Data with Imputed Points and Curves for Other Locations",
       x = "Date", y = "Q Value") +
  
  # Custom colors for lines with a palette that ensures visibility
  scale_color_manual(values = c("Morobbia - Giubiasco" = "purple", 
                                "Moesa - Lumino" = "green", 
                                "Riale di Gnosca - Gnosca" = "red")) +  
  theme_minimal() +
  theme(legend.title = element_blank())  # Removes legend title

