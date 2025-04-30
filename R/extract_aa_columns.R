#' Extract Autoantibody and Subgroup Columns
#'
#' Extracts a subset of a data frame containing specified autoantibody columns and a subgroup column.
#' Supports input as either column names or column positions.
#'
#' @param data_frame A data frame containing autoantibody measurements and subgroup information.
#' @param autoantibody_columns A character vector of column names or an integer vector of column positions
#' corresponding to the autoantibody variables to extract.
#' @param subgroup_column A character string or integer indicating the subgroup column to extract.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{aa_data}{A data frame containing only the selected autoantibody columns and the subgroup column.}
#'   \item{antibody_cols}{The column positions (indices) of the selected autoantibody columns.}
#'   \item{subgroup_col_number}{The column position (index) of the selected subgroup column.}
#' }
#'
#' @details
#' This function checks the type of `autoantibody_columns` and `subgroup_column`:
#' \itemize{
#'   \item If character, it matches them to the column names.
#'   \item If integer, it uses them directly as positions.
#' }
#' If the input is neither character nor integer, an error is thrown.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' df <- your_data_frame
#' autoantibody_vars <- c("anti-dsDNA", "anti-Sm", "anti-RNP")
#' result <- extract_aa_columns(df, autoantibody_columns = autoantibody_vars,
#' subgroup_column = "Clustering")
#' head(result[[1]]) # View extracted data
#' }
#'
#' @seealso [base::subset()]
#'
#' @export
extract_aa_columns <- function(data_frame, autoantibody_columns,
                               subgroup_column){
  if (is(autoantibody_columns, "character")){
    antibody_cols <- which(autoantibody_columns %in% colnames(data_frame))
  } else if(is(autoantibody_columns, "integer")){
    antibody_cols <- autoantibody_columns
  } else {
    stop("autoantibody_columns must contain names or positions of columns")
  }

  if (is(subgroup_column,"character")){
    subgroup_col_number <- which(colnames(data_frame) == subgroup_column)
  } else if(is(subgroup_column, "integer")){
    subgroup_col_number <- subgroup_column
  } else {
    stop("subgroup_column must contain names or positions of columns")
  }


  aa_data <- subset(data_frame, select = c(antibody_cols, subgroup_col_number))
  return(list(aa_data, antibody_cols, subgroup_col_number))
}
