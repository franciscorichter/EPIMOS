#' Train Predictive Models
#'
#' This function trains predictive models based on the provided data frame. It can use different methods such as `lm` or `glm`
#' to fit models for each column in the data frame. The trained models can then be used for imputation of missing data.
#'
#' @param data_frame A data frame containing the data to train the models on.
#' @param method A character string specifying the model type. Options are `"lm"` for linear models and `"glm"` for generalized linear models. Defaults to `"glm"`.
#' @param glm_family A family function to be used if `method` is set to `"glm"`. Defaults to `gaussian()`.
#' @return A list of trained models, one for each column in the data frame.
#' @examples
#' \dontrun{
#' models <- train_models(my_data, method = "glm", glm_family = gaussian())
#' }
#' @export
train_models <- function(data_frame, method = "glm", glm_family = gaussian()) {
  models <- list()
  
  for (i in seq_len(ncol(data_frame))) {
    if (!all(is.na(data_frame[, i]))) {
      fml <- as.formula(paste0("`", colnames(data_frame)[i], "` ~ ."))
      
      if (method == "lm") {
        model <- tryCatch({
          lm(fml, data = data_frame)
        }, error = function(e) {
          message("Model fitting failed for column: ", colnames(data_frame)[i])
          return(NULL)
        })
      } else if (method == "glm") {
        model <- tryCatch({
          glm(fml, data = data_frame, family = glm_family)
        }, error = function(e) {
          message("GLM fitting failed for column: ", colnames(data_frame)[i])
          return(NULL)
        })
      } else {
        stop("Unsupported method. Use 'lm' or 'glm'.")
      }
      
      models[[colnames(data_frame)[i]]] <- model
    }
  }
  
  return(models)
}


# Internal Function to process and append data
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

