# dataOPS

Data Operations (OPS) in Health in the Americas (OPS/PAHO).

This is a package that contains health data processing routines for the Americas.

## How to Install

``` r
# install.packages("devtools")
devtools::install_github("daltonbc96/dataOPS")
```

## Process Mortality Data

This function processes a given mortality dataset based on specified metadata json file, and optional parameters for handling reference year, and maternal mortality cases.
```r
library(dataOPS)
library(data.table)
library(jsonlite)

# Assume `mortality_data` is your data and `metadata.json` is the address of the metadata file.
processed_data <- process_mortality(mortality_data, "path/metadata.json", set_reference_year = 2020,
                                    var_maternal_mortality_cases = "icd_code_column")
```
## Read Files
This function reads files from a specified path and processes them into a data.table object.
Supported file extensions include CSV, TXT, DBF, SAV, XLS, and XLSX.

``` r
# Assume `file_path` is the path to your file
data <- read_file(file_path, sheet = 1)
```
