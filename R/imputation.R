#' Impute Missing Data Using Trained Models
#'
#' This function imputes missing values in a data frame using pre-trained models.
#'
#' @param data_frame A data frame containing the data with missing values (`NA`).
#' @param models A list of pre-trained models corresponding to each column in the data frame.
#' @return A data frame with missing values imputed.
#' @examples
#' \dontrun{
#' imputed_data <- impute_missing_data_with_model(my_data, models)
#' }
#' @export
impute_missing_data_with_model <- function(data_frame, models) {
  imputed_data <- data_frame  # Initialize imputed data
  
  for (i in seq_len(ncol(data_frame))) {
    mss <- is.na(data_frame[, i])
    if (sum(mss) > 0 && !is.null(models[[colnames(data_frame)[i]]])) {
      message("Imputing missing data for column: ", colnames(data_frame)[i])
      
      model <- models[[colnames(data_frame)[i]]]
      imputed_data[mss, i] <- predict(model, newdata = data_frame[mss,])
    }
  }
  
  return(imputed_data)
}
