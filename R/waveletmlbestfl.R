#' @title Finds The Best Column In A Data Frame Containing Eight Metrics Values In The Columns
#'
#' @param input_df Data Frame Containing Eight Metrics Values In Different Columns
#' @import CEEMDANML
#' @return
#' \itemize{
#'   \item best_column_results: A list containing four data frames. Details can be found in description section.
#'   }
#' @export
#'
#' @examples
#' set.seed(123) # for reproducibility
#' Y <- rnorm(50, 25, 5)
#' model_1 <-CEEMDANML::carigaan(Y, ratio = 0.8, n_lag = 4)
#' model_2 <- CEEMDANML::carigas(Y, ratio = 0.8, n_lag = 4)
#' model_1_metrics_values <- model_1$Accuracy[,2]
#' model_2_metrics_values <- model_2$Accuracy[,2]
#' combined_results <- data.frame(cbind(model_1_metrics_values, model_2_metrics_values))
#' colnames(combined_results) <- c("model_1", "model_2")
#' best_model <- best_column(input_df = combined_results)
#' @references
#' \itemize{
#'   \item Paul, R. K., & Garai, S. (2021). Performance comparison of wavelets-based machine learning technique for forecasting agricultural commodity prices. Soft Computing, 25(20), 12857-12873.
#'   \item Paul, R. K., & Garai, S. (2022). Wavelets based artificial neural network technique for forecasting agricultural prices. Journal of the Indian Society for Probability and Statistics, 23(1), 47-61.
#'   \item Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
#'   \item Garai, S., Paul, R. K., Rakshit, D., Yeasin, M., Paul, A. K., Roy, H. S., Barman, S. & Manjunatha, B. (2023). An MRA Based MLR Model for Forecasting Indian Annual Rainfall Using Large Scale Climate Indices. International Journal of Environment and Climate Change, 13(5), 137-150.
#'   \item Garai, S. (2023). Package 'AllMetrics' Type Package Title Calculating Multiple Performance Metrics of a Prediction Model Version 0.1.0, Repository: https://cran.r-project.org/web/packages/AllMetrics/index.html.
#'   }
best_column <- function(input_df) {
  # Create a copy of the input_df to avoid modifying the original
  output_df <- input_df
  # Loop over the first five rows
  for (i in 1:5) {
    # Find the minimum value in the row
    min_value <- min(input_df[i, ])

    # Loop over each column in the row
    for (j in 1:ncol(input_df)) {
      # If the value is the minimum, replace it with 'MIN'
      if (input_df[i, j] == min_value) {
        output_df[i, j] <- 'MIN'
      }
      # Otherwise, replace it with 'NA'
      else {
        output_df[i, j] <- 'NA'
      }
    }
  }

  # Loop over the last three rows
  for (i in (nrow(input_df) - 2):nrow(input_df)) {
    # Find the maximum value in the row
    max_value <- max(input_df[i, ])

    # Loop over each column in the row
    for (j in 1:ncol(input_df)) {
      # If the value is the maximum, replace it with 'MAX'
      if (input_df[i, j] == max_value) {
        output_df[i, j] <- 'MAX'
      }
      # Otherwise, replace it with 'NA'
      else {
        output_df[i, j] <- 'NA'
      }
    }
  }
  metrics <- c("RMSE", "RRMSE", "MAE", "MAPE", "MASE", "NSE", "WI", "LME")
  rownames(output_df) <- metrics
  # Count the number of 'NA' values in each column
  na_counts <- apply(output_df, 2, function(x) sum(x == 'NA'))

  # Find the column(s) with the minimum number of 'NA' values
  min_NA_cols <- names(na_counts)[na_counts == min(na_counts)]

  # If there is more than one column with the same number of 'NA' values
  if (length(min_NA_cols) > 1) {
    # Get the number of 'NA' values for the first five rows in each column
    na_counts_5rows <- apply(output_df[1:5, min_NA_cols], 2, function(x) sum(x == 'NA'))

    # Find the column(s) with the least number of 'NA' values in the first five rows
    min_NA_cols_5rows <- min_NA_cols[na_counts_5rows == min(na_counts_5rows)]

    # Choose the first column from the list of columns with least 'NA' values in the first five rows
    min_NA_col <- min_NA_cols_5rows[1]
  } else {
    # If there is only one column with the minimum number of 'NA' values, choose that column
    min_NA_col <- min_NA_cols[1]
  }
  # Get the values from the input_df for the min_NA_col
  min_NA_values <- input_df[[min_NA_col]]
  BestColumn_metrics <- data.frame(cbind(min_NA_values))
  dim(BestColumn_metrics)
  colnames(BestColumn_metrics) <- min_NA_col
  rownames(BestColumn_metrics) <- metrics
  # Return the column name and its corresponding values
  best_column_results <- list(output_df = output_df, min_NA_col = min_NA_col, min_NA_values = min_NA_values, BestColumn_metrics= BestColumn_metrics)

  return(best_column_results)
}

#' @title Provides The Best Wavelet Filter-Level Combination For WARIGAAN Model
#'
#' @param df Data Frame Containing various time series data except in the the 1st column
#' @param col Mention the column number to be analysed except the 1st column
#' @param f_l filter and level of decomposition should be chosen in this format
#' @import WaveletML DescribeDF
#' @return
#' \itemize{
#'   \item WARIGAANbest_input_df: Data frame containing metrics values for WARIGAAN model different filter-level combinations
#'   \item WARIGAANbest_output_df: Data frame containing ‘NA’, ‘MIN’ or ‘MAX’ values as described earlier
#'   \item WARIGAANbest_FL: Best filter-level combination
#'   \item WARIGAANbest_FL_metrics_values: Corresponding metrics values of WARIGAANbest_FL
#'   \item WARIGAANbest: WARIGAANbest_FL with all metrics values
#'   }
#' @export
#'
#' @examples
#' # example_data
#' # Set the seed for reproducibility
#' set.seed(123)
#' # Define the values of N and CV
#' N <- 25
#' CV <- "(5-10)"
#' mu <- 20
#' # define named list with f_vals and l_vals
#' params <- list(f_vals = 'c18',
#'                l_vals = 2:3)
#' # Define the number of data points to generate for each combination of N and CV
#' n_data <- 3
#' cv_range <- as.numeric(strsplit(gsub("[()]", "", CV), "-")[[1]])
#' cv_values <- seq(cv_range[1], cv_range[2])
#' cv_sample <- sample(cv_values, n_data, replace = TRUE)
#' sd <- mu * (cv_sample/100)
#' data <- replicate(n_data, {
#'   d <- round(abs(rnorm(N, mean = mu, sd = sd)))
#'   d[d == 0] <- 1
#'   d
#' })
#' colnames(data) <- paste0("N_", N, "_CV_", CV, "_", 1:n_data)
#' # Add a column for sequential numbers
#' data_new <- data.frame(cbind("sl no" = 1:N, data))
#' # example
#' warigaan_best_model <- warigaan_best(df = data_new, col = 2, f_l = params)
#' warigaan_best_model
#' @references
#' \itemize{
#'   \item Aldrich, E. (2020). wavelets: Functions for Computing Wavelet Filters, Wavelet Transforms  and Multiresolution Analyses. Repository:https://cran.r-project.org/web/packages/wavelets/index.html.
#'   \item Aldrich, E. (2020). wavelets: Functions for Computing Wavelet Filters, Wavelet Transforms  and Multiresolution Analyses. Repository:https://cran.r-project.org/web/packages/wavelets/index.html.
#'   \item Paul, R. K., & Garai, S. (2021). Performance comparison of wavelets-based machine learning technique for forecasting agricultural commodity prices. Soft Computing, 25(20), 12857-12873.
#'   \item Paul, R. K., & Garai, S. (2022). Wavelets based artificial neural network technique for forecasting agricultural prices. Journal of the Indian Society for Probability and Statistics, 23(1), 47-61.
#'   \item Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
#'   \item Garai, S., Paul, R. K., Rakshit, D., Yeasin, M., Paul, A. K., Roy, H. S., Barman, S. & Manjunatha, B. (2023). An MRA Based MLR Model for Forecasting Indian Annual Rainfall Using Large Scale Climate Indices. International Journal of Environment and Climate Change, 13(5), 137-150.
#'   \item Garai, S. (2023). Package 'AllMetrics' Type Package Title Calculating Multiple Performance Metrics of a Prediction Model Version 0.1.0, Repository: https://cran.r-project.org/web/packages/AllMetrics/index.html.
#'   }
warigaan_best <- function(df, col = 2, f_l = list(f_vals = c('haar', 'c6', 'la8', 'bl14'),
                                                     l_vals = 2:floor(log2(length(Y))))){
  best_column <- NULL
  best_column <- function(input_df) {
    # Create a copy of the input_df to avoid modifying the original
    output_df <- input_df
    # Loop over the first five rows
    for (i in 1:5) {
      # Find the minimum value in the row
      min_value <- min(input_df[i, ])

      # Loop over each column in the row
      for (j in 1:ncol(input_df)) {
        # If the value is the minimum, replace it with 'MIN'
        if (input_df[i, j] == min_value) {
          output_df[i, j] <- 'MIN'
        }
        # Otherwise, replace it with 'NA'
        else {
          output_df[i, j] <- 'NA'
        }
      }
    }

    # Loop over the last three rows
    for (i in (nrow(input_df) - 2):nrow(input_df)) {
      # Find the maximum value in the row
      max_value <- max(input_df[i, ])

      # Loop over each column in the row
      for (j in 1:ncol(input_df)) {
        # If the value is the maximum, replace it with 'MAX'
        if (input_df[i, j] == max_value) {
          output_df[i, j] <- 'MAX'
        }
        # Otherwise, replace it with 'NA'
        else {
          output_df[i, j] <- 'NA'
        }
      }
    }
    metrics <- c("RMSE", "RRMSE", "MAE", "MAPE", "MASE", "NSE", "WI", "LME")
    rownames(output_df) <- metrics
    # Count the number of 'NA' values in each column
    na_counts <- apply(output_df, 2, function(x) sum(x == 'NA'))

    # Find the column(s) with the minimum number of 'NA' values
    min_NA_cols <- names(na_counts)[na_counts == min(na_counts)]

    # If there is more than one column with the same number of 'NA' values
    if (length(min_NA_cols) > 1) {
      # Get the number of 'NA' values for the first five rows in each column
      na_counts_5rows <- apply(output_df[1:5, min_NA_cols], 2, function(x) sum(x == 'NA'))

      # Find the column(s) with the least number of 'NA' values in the first five rows
      min_NA_cols_5rows <- min_NA_cols[na_counts_5rows == min(na_counts_5rows)]

      # Choose the first column from the list of columns with least 'NA' values in the first five rows
      min_NA_col <- min_NA_cols_5rows[1]
    } else {
      # If there is only one column with the minimum number of 'NA' values, choose that column
      min_NA_col <- min_NA_cols[1]
    }

    # Get the values from the input_df for the min_NA_col
    min_NA_values <- input_df[[min_NA_col]]
    BestColumn_metrics <- data.frame(cbind(min_NA_values))
    dim(BestColumn_metrics)
    colnames(BestColumn_metrics) <- min_NA_col
    rownames(BestColumn_metrics) <- metrics
    # Return the column name and its corresponding values
    best_column_results <- list(output_df = output_df, min_NA_col = min_NA_col, min_NA_values = min_NA_values, BestColumn_metrics= BestColumn_metrics)

    return(best_column_results)
  }
  stationarity <- suppressWarnings(DescribeDF::df_stationarity(df))
  # extract the first column name of the data frame
  col_name <- names(df)[col]
  # extract time series data
  Y <- df[[col_name]]

  # set wavelet and decomposition levels
  #f_vals <- c('haar', 'c6', 'la8', 'bl14')
  #l_vals <- c(2:floor(log2(length(Y))))

  # create empty dataframe with column names and number of rows
  n_rows <- 8
  n_cols <- length(f_l$f_vals) * length(f_l$l_vals)
  WARIGAAN_results <- data.frame(matrix(ncol = n_cols, nrow = n_rows))
  colnames(WARIGAAN_results) <- paste0(rep(f_l$f_vals, each = length(f_l$l_vals)), "_", rep(f_l$l_vals, length(f_l$f_vals)))

  # loop through f_vals and l_vals to populate columns
  for (f in f_l$f_vals) {
    for (l in f_l$l_vals) {
      Y <- df[[col_name]]

      # calculate WARIGAAN metrics values for every filter level combination
      warigaan_results_name <- paste0(f, "_", l)
      assign(warigaan_results_name,WaveletML::warigaan(Y = Y,
                                                       ratio = 0.9,
                                                       n_lag = as.numeric(stationarity$ADF[[col_name]][2]),
                                                       l = l,
                                                       f = f))
      WARIGAAN_f_l <- as.numeric(get(warigaan_results_name)$Accuracy[, 2])

      # add column to dataframe
      WARIGAAN_results[, paste0(f, "_", l)] <- WARIGAAN_f_l
    }
  }

  #find best column
  WARIGAAN_best_list <- best_column(WARIGAAN_results)
  return(list(WARIGAANbest_input_df = WARIGAAN_results,
              WARIGAANbest_output_df = WARIGAAN_best_list$output_df,
              WARIGAANbest_FL = WARIGAAN_best_list$min_NA_col,
              WARIGAANbest_FL_metrics_values = WARIGAAN_best_list$min_NA_values,
              WARIGAANbest = WARIGAAN_best_list$BestColumn_metrics))
}

#' @title Provides The Best Wavelet Filter-Level Combination For WARIGAS Model
#'
#' @param df Data Frame Containing various time series data except in the the 1st column
#' @param col Mention the column number to be analysed except the 1st column
#' @param f_l filter and level of decomposition should be chosen in this format
#' @import WaveletML DescribeDF
#' @return
#' \itemize{
#'   \item WARIGASbest_input_df: Data frame containing metrics values for WARIGAS model different filter-level combinations
#'   \item WARIGASbest_output_df: Data frame containing ‘NA’, ‘MIN’ or ‘MAX’ values as described earlier
#'   \item WARIGASbest_FL: Best filter-level combination
#'   \item WARIGASbest_FL_metrics_values: Corresponding metrics values of WARIGASbest_FL
#'   \item WARIGASbest: WARIGASbest_FL with all metrics values
#'   }
#' @export
#'
#' @examples
#' # example_data
#' # Set the seed for reproducibility
#' set.seed(123)
#' # Define the values of N and CV
#' N <- 25
#' CV <- "(5-10)"
#' mu <- 20
#' # define named list with f_vals and l_vals
#' params <- list(f_vals = 'c18',
#'                l_vals = 2:3)
#' # Define the number of data points to generate for each combination of N and CV
#' n_data <- 3
#' cv_range <- as.numeric(strsplit(gsub("[()]", "", CV), "-")[[1]])
#' cv_values <- seq(cv_range[1], cv_range[2])
#' cv_sample <- sample(cv_values, n_data, replace = TRUE)
#' sd <- mu * (cv_sample/100)
#' data <- replicate(n_data, {
#'   d <- round(abs(rnorm(N, mean = mu, sd = sd)))
#'   d[d == 0] <- 1
#'   d
#' })
#' colnames(data) <- paste0("N_", N, "_CV_", CV, "_", 1:n_data)
#' # Add a column for sequential numbers
#' data_new <- data.frame(cbind("sl no" = 1:N, data))
#' # example
#' warigas_best_model <- warigas_best(df = data_new, col = 2, f_l = params)
#' warigas_best_model
#' @references
#' \itemize{
#'   \item Aldrich, E. (2020). wavelets: Functions for Computing Wavelet Filters, Wavelet Transforms  and Multiresolution Analyses. Repository:https://cran.r-project.org/web/packages/wavelets/index.html.
#'   \item Paul, R. K., & Garai, S. (2021). Performance comparison of wavelets-based machine learning technique for forecasting agricultural commodity prices. Soft Computing, 25(20), 12857-12873.
#'   \item Paul, R. K., & Garai, S. (2022). Wavelets based artificial neural network technique for forecasting agricultural prices. Journal of the Indian Society for Probability and Statistics, 23(1), 47-61.
#'   \item Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
#'   \item Garai, S., Paul, R. K., Rakshit, D., Yeasin, M., Paul, A. K., Roy, H. S., Barman, S. & Manjunatha, B. (2023). An MRA Based MLR Model for Forecasting Indian Annual Rainfall Using Large Scale Climate Indices. International Journal of Environment and Climate Change, 13(5), 137-150.
#'   \item Garai, S. (2023). Package 'AllMetrics' Type Package Title Calculating Multiple Performance Metrics of a Prediction Model Version 0.1.0, Repository: https://cran.r-project.org/web/packages/AllMetrics/index.html.
#'   }

warigas_best <- function(df, col = 2, f_l = list(f_vals = c('haar', 'c6', 'la8', 'bl14'),
                                                 l_vals = 2:floor(log2(length(Y))))){
  best_column <- NULL
  best_column <- function(input_df) {
    # Create a copy of the input_df to avoid modifying the original
    output_df <- input_df
    # Loop over the first five rows
    for (i in 1:5) {
      # Find the minimum value in the row
      min_value <- min(input_df[i, ])

      # Loop over each column in the row
      for (j in 1:ncol(input_df)) {
        # If the value is the minimum, replace it with 'MIN'
        if (input_df[i, j] == min_value) {
          output_df[i, j] <- 'MIN'
        }
        # Otherwise, replace it with 'NA'
        else {
          output_df[i, j] <- 'NA'
        }
      }
    }

    # Loop over the last three rows
    for (i in (nrow(input_df) - 2):nrow(input_df)) {
      # Find the maximum value in the row
      max_value <- max(input_df[i, ])

      # Loop over each column in the row
      for (j in 1:ncol(input_df)) {
        # If the value is the maximum, replace it with 'MAX'
        if (input_df[i, j] == max_value) {
          output_df[i, j] <- 'MAX'
        }
        # Otherwise, replace it with 'NA'
        else {
          output_df[i, j] <- 'NA'
        }
      }
    }
    metrics <- c("RMSE", "RRMSE", "MAE", "MAPE", "MASE", "NSE", "WI", "LME")
    rownames(output_df) <- metrics
    # Count the number of 'NA' values in each column
    na_counts <- apply(output_df, 2, function(x) sum(x == 'NA'))

    # Find the column(s) with the minimum number of 'NA' values
    min_NA_cols <- names(na_counts)[na_counts == min(na_counts)]

    # If there is more than one column with the same number of 'NA' values
    if (length(min_NA_cols) > 1) {
      # Get the number of 'NA' values for the first five rows in each column
      na_counts_5rows <- apply(output_df[1:5, min_NA_cols], 2, function(x) sum(x == 'NA'))

      # Find the column(s) with the least number of 'NA' values in the first five rows
      min_NA_cols_5rows <- min_NA_cols[na_counts_5rows == min(na_counts_5rows)]

      # Choose the first column from the list of columns with least 'NA' values in the first five rows
      min_NA_col <- min_NA_cols_5rows[1]
    } else {
      # If there is only one column with the minimum number of 'NA' values, choose that column
      min_NA_col <- min_NA_cols[1]
    }

    # Get the values from the input_df for the min_NA_col
    min_NA_values <- input_df[[min_NA_col]]
    BestColumn_metrics <- data.frame(cbind(min_NA_values))
    dim(BestColumn_metrics)
    colnames(BestColumn_metrics) <- min_NA_col
    rownames(BestColumn_metrics) <- metrics
    # Return the column name and its corresponding values
    best_column_results <- list(output_df = output_df, min_NA_col = min_NA_col, min_NA_values = min_NA_values, BestColumn_metrics= BestColumn_metrics)

    return(best_column_results)
  }
  stationarity <- suppressWarnings(DescribeDF::df_stationarity(df))
  # extract the first column name of the data frame
  col_name <- names(df)[col]
  # extract time series data
  Y <- df[[col_name]]

  # set wavelet and decomposition levels
  #f_vals <- c('haar', 'c6', 'la8', 'bl14')
  #l_vals <- c(2:floor(log2(length(Y))))

  # create empty dataframe with column names and number of rows
  n_rows <- 8
  n_cols <- length(f_l$f_vals) * length(f_l$l_vals)
  WARIGAS_results <- data.frame(matrix(ncol = n_cols, nrow = n_rows))
  colnames(WARIGAS_results) <- paste0(rep(f_l$f_vals, each = length(f_l$l_vals)), "_", rep(f_l$l_vals, length(f_l$f_vals)))

  # loop through f_vals and l_vals to populate columns
  for (f in f_l$f_vals) {
    for (l in f_l$l_vals) {
      Y <- df[[col_name]]

      # calculate WARIGAS metrics values for every filter level combination
      warigas_results_name <- paste0(f, "_", l)
      assign(warigas_results_name,WaveletML::warigas(Y = Y,
                                                     ratio = 0.9,
                                                     n_lag = as.numeric(stationarity$ADF[[col_name]][2]),
                                                     l = l,
                                                     f = f))
      WARIGAS_f_l <- as.numeric(get(warigas_results_name)$Accuracy[, 2])

      # add column to dataframe
      WARIGAS_results[, paste0(f, "_", l)] <- WARIGAS_f_l
    }
  }

  #find best column
  WARIGAS_best_list <- best_column(WARIGAS_results)
  return(list(WARIGASbest_input_df = WARIGAS_results,
              WARIGASbest_output_df = WARIGAS_best_list$output_df,
              WARIGASbest_FL = WARIGAS_best_list$min_NA_col,
              WARIGASbest_FL_metrics_values = WARIGAS_best_list$min_NA_values,
              WARIGASbest = WARIGAS_best_list$BestColumn_metrics))
}



