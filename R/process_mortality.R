##' Process Mortality Function
#'
#' This function processes a given mortality dataset based on specified metadata json file,
#' and optional parameters for handling reference year, and maternal mortality cases.
#'
#' @param data A data.table object containing the data to be processed.
#' @param metadata_address A string specifying the path to a JSON file containing metadata for processing the data.
#' @param set_reference_year (Optional) A numeric or string value specifying a reference year to be added to the data.
#' @param var_maternal_mortality_cases (Optional) A string specifying the name of the column in the data to be used for
#'        identifying maternal mortality cases.
#'
#' @details
#' The function consists of various helper functions that handle specific processing tasks:
#' - `load_json_as_list`: Loads a JSON file and returns its content as a list.
#' - `clean_string`: Cleans and standardizes character strings.
#' - `check_actual_type`: Checks and returns the actual type of a categorical variable.
#' - `process_date`: Processes and formats date values.
#'
#' The main function reads the metadata from the specified JSON file, iterates through each variable in the metadata,
#' and processes the data based on the metadata specifications.
#'
#' For each variable in the metadata, the function performs the following tasks:
#' - Checks if the variable exists in the data, skips the variable if not found.
#' - Checks and adjusts the data type of the variable based on the metadata and actual data.
#' - Processes character, date, time, numeric, categorical numeric, and categorical character-to-character variables based on specified rules and formats.
#' - Optionally creates a new variable 'maternal_mortality_cases' based on specified criteria.
#' - Optionally adds a 'reference_year' variable with a specified value.
#'
#' New with this version:
#' - Added processing for the 'categorical_character_to_character' type, which maps character values to their corresponding labels.
#'
#' @return
#' Returns the processed data as a data.table object.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(jsonlite)
#'
#' # Assume `mortality_data` is your data and `metadata.json` is your metadata file path address
#' processed_data <- process_mortality(mortality_data, "metadata.json", set_reference_year = 2020,
#'                                      var_maternal_mortality_cases = "icd_code_column")
#' }
#'
#' @seealso
#' \code{\link[jsonlite]{fromJSON}}, \code{\link[stringi]{stri_trans_general}},
#' \code{\link[base]{as.Date}}, \code{\link[dplyr]{na_if}}
#' @export
process_mortality <-  function(data,
                               metadata_address,
                               set_reference_year = NULL,
                               var_maternal_mortality_cases = NULL) {
  load_json_as_list <- function(json_path) {
    json_content <-
      jsonlite::fromJSON(json_path, simplifyVector = FALSE)
    return(json_content)
  }
  clean_string <- function(string) {
    cleaned_string <- gsub("[[:punct:]&&[^(),.]]", "", string)
    cleaned_string <- stringi::stri_trans_general(cleaned_string, "Latin-ASCII")
    cleaned_string <- tools::toTitleCase(cleaned_string)
    return(cleaned_string)
  }

  check_actual_type <- function(var_data, var_type) {
    is_all_numeric <-
      all(!is.na(as.numeric(as.character(
        na.omit(var_data)
      ))))

    # If the data is already marked as categorical_numeric and all values are numeric
    if (var_type == "categorical_numeric" && is_all_numeric) {
      return("categorical_numeric")
    } else {
      # If there's any non-numeric value, treat as categorical_character
      return("categorical_character")
    }
  }

  process_date <- function(date_column) {
    if (length(date_column) == 0) {
      stop("date_column is empty")
    }

    # Initialize a vector to store the formatted dates
    formatted_dates_final <-
      rep(NA_character_, length(date_column))

    # Check the data type of the date column
    if (inherits(date_column, "POSIXct")) {
      # If the date column is POSIXct, format directly to the desired date string
      formatted_dates_final <- format(date_column, "%d/%m/%Y")
      return(formatted_dates_final)
    } else {
      first_value <-
        as.character(na.omit(date_column)[1])  # Remove NA values and get the first value

      # Check if the date format is "dd/mm/yyyy"
      if (grepl("^\\d{2}/\\d{2}/\\d{4}$", first_value)) {
        # If the date format is already correct, just return the date column as it is
        return(as.character(date_column))
      } else {
        non_na_indices <- which(!is.na(date_column))
        formatted_dates <-
          sub(" UTC", "", as.character(date_column[non_na_indices]))
        date_objects <-
          as.Date(formatted_dates, format = "%Y-%m-%d")

        if (any(is.na(date_objects))) {
          if (!is.na(first_value) && nchar(first_value) > 8) {
            # If character count is large, assume it's an SPSS date and process accordingly
            formatted_dates_final[non_na_indices] <-
              format(as.POSIXct(
                as.numeric(date_column[non_na_indices]),
                origin = "1582-10-14",
                tz = "GMT"
              ),
              "%d/%m/%Y")
          } else {
            # If character count is small, assume it's an Excel date and process accordingly
            formatted_dates_final[non_na_indices] <-
              format(as.Date(as.numeric(date_column[non_na_indices]), origin = "1899-12-30"),
                     "%d/%m/%Y")
          }
        } else {
          formatted_dates_final[non_na_indices] <-
            format(date_objects, "%d/%m/%Y")
        }
      }
    }

    return(formatted_dates_final)
  }


  metadata <- load_json_as_list(metadata_address)

  for (var_name in names(metadata)) {
    var_data <- data[[var_name]]

    # Check if the column exists in the data
    if (!var_name %in% names(data)) {
      warning(paste0(
        "Variable ",
        var_name,
        " from metadata not found in data. Skipping..."
      ))
      next # Skip this iteration of the loop
    }


    # Extract the type for this variable from metadata
    var_type <- metadata[[var_name]]$type

    # Get the actual type of the variable from data, if it's a categorical variable
    if (var_type %in% c("categorical_numeric", "categorical_character")) {
      actual_type <- check_actual_type(var_data, var_type)
      # If the actual type and metadata type don't match, use the actual type
      if (var_type != actual_type) {
        var_type <- actual_type
      }
    }

    # For character variables
    if (!is.null(var_type) && var_type == "character") {
      data[, (var_name) := dplyr::na_if(.SD[[var_name]], ""), .SDcols = var_name]
      data[, (var_name) := dplyr::na_if(.SD[[var_name]], NA), .SDcols = var_name]

      data[, (var_name) := as.character(.SD[[var_name]]), .SDcols = var_name]
    }

    # For date variables
    if (!is.null(var_type) && var_type == "date") {
      # First preprocess the date using the process_date function
      data[, (var_name) := process_date(.SD[[var_name]]), .SDcols = var_name]

      # Then format it according to the provided format
      if (!is.null(metadata[[var_name]]$format)) {
        date_format <- metadata[[var_name]]$format
        data[, (var_name) := as.Date(.SD[[var_name]], format = date_format), .SDcols = var_name]
      } else {
        next
      }
    }

    # For time variables
    if (!is.null(var_type) && var_type == "time") {
      # Replace empty strings with NA
      data[, (var_name) := ifelse(.SD[[var_name]] == "", NA, .SD[[var_name]]), .SDcols = var_name]
      # Convert to time using hms::as_hms()
      #data[, (var_name) := hms::as_hms(.SD[[var_name]]), .SDcols = var_name]
      data[, (var_name) := as.character(.SD[[var_name]]), .SDcols = var_name]
    }

    # For numeric variables
    if (!is.null(var_type) && var_type == "numeric") {
      # If a missing value(s) is specified in the metadata, replace it with NA
      if (!is.null(metadata[[var_name]]$missing)) {
        missing_vals <- metadata[[var_name]]$missing
        if (is.list(missing_vals)) {
          # If multiple missing values are provided
          for (missing_val in missing_vals) {
            set(data,
                which(data[[var_name]] == missing_val),
                var_name,
                NA_real_)
          }
        } else {
          # If a single missing value is provided
          set(data,
              which(data[[var_name]] == missing_vals),
              var_name,
              NA_real_)
        }
      }
      data[, (var_name) := as.numeric(.SD[[var_name]]), .SDcols = var_name]
    }


    # For categorical_character variables
    if (!is.null(var_type) &&
        var_type == "categorical_character") {
      data[, (var_name) := dplyr::na_if(.SD[[var_name]], ""), .SDcols = var_name]
      data[, (var_name) := dplyr::na_if(.SD[[var_name]], NA), .SDcols = var_name]

      # Convert to factor first
      data[, (var_name) := factor(.SD[[var_name]]), .SDcols = var_name]
      # Clean each unique level
      cleaned_levels <-
        sapply(levels(data[[var_name]]), clean_string)
      # Update the levels of the factor with cleaned levels
      levels(data[[var_name]]) <- cleaned_levels

      # Optionally merge identical levels (if any) after cleaning
      data[, (var_name) := factor(.SD[[var_name]]), .SDcols = var_name]
    }


    # For categorical_numeric variables
    if (!is.null(var_type) && var_type == "categorical_numeric") {
      # If labels are provided
      if (!is.null(metadata[[var_name]]$labels)) {
        labels <- metadata[[var_name]]$labels
        cleaned_labels <- sapply(names(labels), clean_string)
        names(labels) <- cleaned_labels  # Update the names of the labels with cleaned strings

        all_levels <- na.omit(unique(var_data))
        missing_labels <- setdiff(all_levels, names(labels))

        # Add missing labels
        for (ml in missing_labels) {
          labels[ml] <- paste0("sin etiqueta para el valor ", ml)
        }

        # Sort levels and labels by the order of appearance in data
        ordered_levels <- all_levels[order(match(all_levels, names(labels)))]
        ordered_labels <- labels[ordered_levels]

        data[, (var_name) := factor(.SD[[var_name]], levels = ordered_levels, labels = ordered_labels), .SDcols = var_name]

        # Set label for NA values if applicable
        if ("NA" %in% names(labels)) {
          levels(data[[var_name]])[is.na(levels(data[[var_name]]))] <- labels[["NA"]]
        }
      } else {
        data[, (var_name) := factor(.SD[[var_name]]), .SDcols = var_name]
      }
    }


    # For categorical_character_to_character
    if (!is.null(var_type) && var_type == "categorical_character_to_character") {
      # If labels are provided
      if (!is.null(metadata[[var_name]]$labels)) {
        labels <- metadata[[var_name]]$labels
        cleaned_labels <- sapply(names(labels), clean_string)
        names(labels) <- cleaned_labels  # Update the names of the labels with cleaned strings

        all_original_levels <- na.omit(unique(var_data))
        all_cleaned_levels <- sapply(all_original_levels, clean_string)

        missing_labels <- setdiff(all_cleaned_levels, names(labels))

        # Add missing labels
        for (ml in missing_labels) {
          labels[ml] <- paste0("sin etiqueta para el valor ", ml)
        }

        # Sort levels and labels by the order of appearance in data
        ordered_levels <- all_cleaned_levels[order(match(all_cleaned_levels, names(labels)))]
        ordered_labels <- labels[ordered_levels]

        levels_mapping <- setNames(all_original_levels, all_cleaned_levels)
        cleaned_var_data <- factor(var_data, levels = all_original_levels, labels = all_cleaned_levels)

        data[, (var_name) := factor(cleaned_var_data, levels = ordered_levels, labels = ordered_labels)]

        # Set label for NA values if applicable
        if ("NA" %in% names(labels)) {
          levels(data[[var_name]])[is.na(levels(data[[var_name]]))] <- labels[["NA"]]
        }
      } else {
        data[, (var_name) := factor(.SD[[var_name]]), .SDcols = var_name]
      }
    }


    if (!is.null(var_maternal_mortality_cases)) {
      data[, maternal_mortality_cases := grepl("^A34", get(var_maternal_mortality_cases)) |
             grepl("^O", get(var_maternal_mortality_cases))]
    }

    if (!is.null(set_reference_year)) {
      if (!"reference_year" %in% names(data) ||
          is.null(data[["reference_year"]])) {
        data[, ("reference_year") := set_reference_year]
      }
    }
  }

  return(data)

}
