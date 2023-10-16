#' Check Common Columns Across DataFrames
#'
#' This function accepts a list of dataframes and checks for common columns
#' across them. It returns a dataframe indicating the presence of variables
#' in each dataframe.
#'
#' @param dfs A list of dataframes to check for common columns.
#'
#' @return A dataframe with rows representing variables (columns) and
#'         columns representing the names of the dataframes in the list.
#'         An "X" indicates the presence of a variable in the corresponding
#'         dataframe, while a blank entry indicates absence.
#'         #' @export
check_common_columns <- function(dfs) {

  # Get the names of the dataframes
  df_names <- names(dfs)

  # Get all unique columns across the dataframes
  all_columns <- unique(unlist(lapply(dfs, colnames)))

  # Create a new dataframe to store the results
  result_df <- matrix("", nrow = length(all_columns), ncol = length(dfs),
                      dimnames = list(all_columns, df_names))

  # Fill the new dataframe with indications of presence or absence of variables
  for(i in seq_along(dfs)) {
    df <- dfs[[i]]
    df_name <- df_names[i]
    common_columns <- colnames(df)
    result_df[common_columns, df_name] <- "X"
  }

  # Convert the resulting matrix to a dataframe
  result_df <- as.data.frame(result_df)

  # Return the resulting dataframe
  return(result_df)
}
