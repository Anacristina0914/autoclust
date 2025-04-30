#' Transform Autoantibody Data for Line Plot
#'
#' Prepares a long-format data frame showing the percentage frequency of positive autoantibody results
#' per subgroup, suitable for plotting with `ggplot2`.
#'
#' @param df A data frame containing the autoantibody data and subgroup information.
#' @param autoantibody_columns A character vector of column names or an integer vector of column positions
#' corresponding to the autoantibody variables to include.
#' @param subgroup_column A character string or integer indicating the subgroup column to group by.
#' Defaults to `"Clustering"`.
#'
#' @return A long-format data frame with the following columns:
#' \describe{
#'   \item{Subgroup column}{The grouping variable (e.g., Clustering).}
#'   \item{Autoantibody}{The name of the autoantibody.}
#'   \item{Frequency}{The percentage of positive results within each subgroup.}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Extracts the specified autoantibody columns and the subgroup column.
#'   \item Converts the autoantibody binary values to numeric (if needed).
#'   \item Calculates the sum of positives per subgroup.
#'   \item Divides by the subgroup size to obtain the percentage positivity.
#'   \item Returns the data in a melted long format suitable for plotting.
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage
#' df <- your_data_frame
#' autoantibody_vars <- c("anti-dsDNA", "anti-Sm", "anti-RNP")
#' freq_long <- transform_data_for_line_plot(df, autoantibody_columns = autoantibody_vars)
#'
#' library(ggplot2)
#' ggplot(freq_long, aes(x = Autoantibody, y = Frequency, group = Clustering, color = Clustering)) +
#'   geom_line() + geom_point()
#' }
#'
#' @seealso [reshape2::melt()], [dplyr::group_by()], [dplyr::mutate()]
#'
#' @export
transform_data_for_line_plot <- function(df, autoantibody_columns,
                                         subgroup_column = "Clustering"){

  aa_result_list <- extract_aa_columns(data_frame = df,
                                       autoantibody_columns = autoantibody_columns,
                                       subgroup_column = subgroup_column)

  # Extract data, aa_colnames and subgroup_colname
  aa_data <- aa_result_list[[1]]
  aa_columns <- aa_result_list[[2]]
  subgroup_column <- aa_result_list[[3]]

  subroup_column_name <- colnames(aa_data)[subgroup_column]

  #Convert binary columns to numeric
  aa_data <- aa_data %>%
    mutate(across(all_of(colnames(aa_data)[aa_columns]), ~ as.numeric(as.character(.))))

  # Groupped sums per subgroup
  grouped_sums <- aa_data %>%
    group_by(.data[[subroup_column_name]]) %>%
    summarise(across(, sum))

  # Group sizes
  group_sizes <- aa_data %>%
    group_by(.data[[subroup_column_name]]) %>%
    summarise(group_size = n())

  # Divide the sum of 1's by the number of individuals per group and multiply by 100 for percentage
  group_freq <- grouped_sums %>%
    left_join(group_sizes, by = subroup_column_name) %>%
    mutate(across(all_of(colnames(aa_data)[aa_columns]), ~ . / group_size *100))

  # Remove cluster information no longer needed
  group_freq$group_size <- NULL
  # Change format for plotting
  freq_long <- melt(group_freq, id.vars = subroup_column_name,
                    variable.name = "Autoantibody",
                    value.name = "Frequency")
  return(freq_long)

}
