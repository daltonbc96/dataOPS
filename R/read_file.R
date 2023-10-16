#' Read File Function
#'
#' This function reads files from a specified path and processes them into a data.table object.
#' Supported file extensions include CSV, TXT, DBF, SAV, XLS, and XLSX.
#'
#' @param file_path A string specifying the path to the file to be read.
#' @param sheet (Optional) A numeric or character value specifying the sheet to be read (only applicable for XLS and XLSX files).
#' @param csv_encoding (Optional) A string specifying the encoding of the CSV file. Default is "UTF-8".
#'
#' @details
#' The function first identifies the file extension and then reads the file accordingly using the appropriate
#' R package and function. It handles missing values by replacing a set of predefined strings (e.g., "NA", "NULL") with NA.
#' Post-reading processing includes replacing certain characters, trimming whitespace, and standardizing column names.
#'
#' The file reading is handled by different packages based on the file extension:
#' - CSV and TXT files are read using \code{data.table::fread}.
#' - DBF files are read using \code{foreign::read.dbf}.
#' - SAV files are read using \code{foreign::read.spss}.
#' - XLS and XLSX files are read using \code{readxl::read_excel}.
#'
#' @return
#' Returns the data read from the file as a data.table object with processed column names and character data.
#'
#' @examples
#' \dontrun{
#' # Assume `file_path` is the path to your file
#' data <- read_file(file_path, sheet = 1)
#' }
#'
#' @seealso
#' \code{\link[data.table]{fread}}, \code{\link[foreign]{read.dbf}}, \code{\link[foreign]{read.spss}}, \code{\link[readxl]{read_excel}}
#' @export
read_file <- function(file_path, sheet = NULL, csv_encoding = "UTF-8") {

  extension <- tools::file_ext(file_path)
  na_values <- c("NA", "NULL", "", "NaN", "#N/A", "#REF!", "#NAME?", "#DIV/0!", "#NUM!", "#VALUE!", "#NULL!", "<NA>")


  if (extension == "txt" | extension == "TXT" |extension == "csv" | extension == "CSV") {
    df <- data.table::fread(file_path, stringsAsFactors = F, na.strings = na_values, colClasses = "character", encoding = csv_encoding)

  } else if (extension == "dbf" | extension == "DBF") {
    df <- foreign::read.dbf(file_path, as.is = T)
    df <- data.table::as.data.table(df)

  } else if (extension == "sav" | extension == "SAV") {
    df <- foreign::read.spss(file_path, to.data.frame = T, use.value.labels = F)
    df <- data.table::as.data.table(df)

  } else if (extension == "xls" | extension == "XLS" | extension == "xlsx" | extension == "XLSX") {
    df <- readxl::read_excel(file_path, sheet = sheet)
    df <- data.table::as.data.table(df)

  } else {
    stop("Unsupported file extension")
  }

  # Processamento centralizado
  df[ , (names(df)) := lapply(.SD, function(col) {
    col[col %in% na_values] <- NA_character_
    return(col)
  })]

  names(df) <- tolower(gsub("[^a-zA-Z0-9_]", "", iconv(names(df), to = "ASCII//TRANSLIT")))
  cols_to_update <- names(df)[sapply(head(df), is.character)]

  df[, (cols_to_update) := lapply(.SD, function(x) {
    x <- stringr::str_replace_all(x, "\t", "")
    stringr::str_trim(x)
  }), .SDcols = cols_to_update]

  return(df)
}

