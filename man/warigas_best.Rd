\name{warigas_best}
\alias{warigas_best}
\title{Provides The Best Wavelet Filter-Level Combination For WARIGAS Model}
\usage{
warigas_best(df, col = 2, f_l = list(f_vals = c('haar', 'c6', 'la8', 'bl14'),
                                                     l_vals = 2:floor(log2(length(Y)))))
}
\arguments{
\item{df}{Data Frame Containing various time series data except in the the 1st column}
\item{col}{Mention the column number to be analysed except the 1st column}
\item{f_l}{filter and level of decomposition should be chosen in this format}

}
\value{
\itemize{
  \item WARIGASbest_input_df: Data frame containing metrics values for WARIGAS model different filter-level combinations
  \item WARIGASbest_output_df: Data frame containing ‘NA’, ‘MIN’ or ‘MAX’ values as described earlier
  \item WARIGASbest_FL: Best filter-level combination
  \item WARIGASbest_FL_metrics_values: Corresponding metrics values of WARIGASbest_FL
  \item WARIGASbest: WARIGASbest_FL with all metrics values
  }
}
\description{
Four filters have been chosen namely ‘haar’, ‘c6’, ‘la8’, and ‘bl14’. (Kindly refer to 'wavelets' in 'CRAN' repository for more supported filters). Levels of decomposition are 2, 3, 4, etc. up to maximum decomposition level which is ceiling value of logarithm of length of the series base 2. For each combination ‘warigas’ are run (from ‘WaveletML’ package). Results are stored in ‘WARIGASbest_input_df’. First five metrics are expected to be minimum and last three metrics are expected to be maximum for a model to be considered good. Firstly, every metric value (among first five) is searched in every columns and minimum values are denoted as ‘MIN’ and other values are denoted as ‘NA’. Secondly, every metric (among last three) is searched in every columns and maximum values are denoted as ‘MAX’ and other values are denoted as ‘NA’. ‘WARIGASbest_output_df’ contains the similar number of rows (which is 8) and columns (which is number filter-level combinations) as of ‘WARIGASbest_input_df’. Values in ‘WARIGASbest_output_df’ are corresponding ‘NA’, ‘MIN’ or ‘MAX’. Finally, the column containing minimum number of ‘NA’ values is denoted as the best (‘WARIGASbest_FL). In special case, if two columns having equal 'NA', it has been checked among these two columns which one is having least 'NA' in first five rows and has been inferred as the best. ‘WARIGASbest_FL_metrics_values’ are the corresponding metrics values. ‘WARIGASbest’ is the data frame (dimension: 1*8) containing different metrics of the best filter-level combination. More details about 'AllMetrics' can be found in Garai et al. (2023) <doi:10.13140/RG.2.2.11977.42087>.
}
\examples{
# example_data
# Set the seed for reproducibility
set.seed(123)
# Define the values of N and CV
N <- 25
CV <- "(5-10)"
mu <- 20
# define named list with f_vals and l_vals
params <- list(f_vals = 'c18',
               l_vals = 2:3)
# Define the number of data points to generate for each combination of N and CV
n_data <- 3
cv_range <- as.numeric(strsplit(gsub("[()]", "", CV), "-")[[1]])
cv_values <- seq(cv_range[1], cv_range[2])
cv_sample <- sample(cv_values, n_data, replace = TRUE)
sd <- mu * (cv_sample/100)
data <- replicate(n_data, {
  d <- round(abs(rnorm(N, mean = mu, sd = sd)))
  d[d == 0] <- 1
  d
})
colnames(data) <- paste0("N_", N, "_CV_", CV, "_", 1:n_data)
# Add a column for sequential numbers
data_new <- data.frame(cbind("sl no" = 1:N, data))
# example
warigas_best_model <- warigas_best(df = data_new, col = 2, f_l = params)
warigas_best_model
}
\references{
\itemize{
  \item Aldrich, E. (2020). wavelets: Functions for Computing Wavelet Filters, Wavelet Transforms  and Multiresolution Analyses. Repository:https://cran.r-project.org/web/packages/wavelets/index.html.
  \item Paul, R. K., & Garai, S. (2021). Performance comparison of wavelets-based machine learning technique for forecasting agricultural commodity prices. Soft Computing, 25(20), 12857-12873.
  \item Paul, R. K., & Garai, S. (2022). Wavelets based artificial neural network technique for forecasting agricultural prices. Journal of the Indian Society for Probability and Statistics, 23(1), 47-61.
   \item Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
  \item Garai, S., Paul, R. K., Rakshit, D., Yeasin, M., Paul, A. K., Roy, H. S., Barman, S. & Manjunatha, B. (2023). An MRA Based MLR Model for Forecasting Indian Annual Rainfall Using Large Scale Climate Indices. International Journal of Environment and Climate Change, 13(5), 137-150.
  \item Garai, S. Paul, R. K., Yeasin, M.(2023). Package 'WaveletML' Type Package Title Wavelet Decomposition Based Hybrid Machine Learning Models, Repository: https://cran.r-project.org/web/packages/WaveletML/index.html.
  }
}
