# Ensure required packages are installed before proceeding
required_packages_1 <- c("pingr", "conflicted", "pacman")
new_packages_1 <- required_packages_1[!(required_packages_1 %in% 
                                        installed.packages()[, "Package"])]
if (length(new_packages_1)) install.packages(new_packages_1)

library(conflicted)
library(pingr)
library(pacman)

# Function to check internet connectivity
check_internet <- function() {
  if (!requireNamespace("pingr", quietly = TRUE)) {
    message("The 'pingr' package is required but not installed. ",
            "Skipping internet connectivity check.")
    internet_available <- FALSE
  } else {
    internet_available <- pingr::is_online()
  }
  return(internet_available)
}

# Initialize internet_available as NULL
internet_available <- NULL

# Check internet connectivity
internet_available <- check_internet()  # Ensure global assignment

# Set default CRAN repository
if (is.null(getOption("repos"))) {
  options(repos = c(CRAN = "https://cran.rstudio.com/"))
}

# Proceed only if there is an active internet connection
if (internet_available) {
  pacman::p_load(
    ggplot2, dplyr, VIM, caret, corrplot, openxlsx, jsonlite, readr,
    readxl, data.table
  )
  suppressMessages(suppressWarnings(
    pacman::p_load(
      ggplot2, dplyr, VIM, caret, corrplot, openxlsx, jsonlite, readr,
      readxl, data.table
    )
  ))
  caret_dependencies <- c("recipes", "listenv")
  missing_deps <- caret_dependencies[
    !sapply(caret_dependencies, requireNamespace, quietly = TRUE)
  ]
  if (length(missing_deps) > 0) {
    message(
      "Installing missing caret dependencies: ",
      paste(missing_deps, collapse = ", ")
    )
  }
} else {
  message(
    "No internet connection available. Please check your internet ",
    "connection and try again."
  )
  if (requireNamespace("caret", quietly = TRUE)) {
    caret_dependencies <- c("recipes", "listenv")
    missing_deps <- caret_dependencies[
      !sapply(caret_dependencies, requireNamespace, quietly = TRUE)
    ]
    if (length(missing_deps) > 0) {
      message(
        "Installing missing caret dependencies: ",
        paste(missing_deps, collapse = ", ")
      )
    }
  }
}


if (requireNamespace("caret", quietly = TRUE)) {
  if (requireNamespace("purrr", quietly = TRUE)) {
    if (requireNamespace("dplyr", quietly = TRUE)) {
      conflict_prefer("filter", "dplyr", quiet = TRUE)
    }
  }
}

required_packages <- c(
  "ggplot2", "dplyr", "VIM", "caret", "corrplot", "openxlsx", "jsonlite",
  "readr", "readxl", "conflicted", "data.table"
)
if (internet_available) {
  suppressMessages(suppressWarnings(
    pacman::p_load(char = required_packages, install = TRUE)
  ))
} else {
  missing_pkgs <- required_packages[
    !sapply(required_packages, requireNamespace, quietly = TRUE)
  ]
  if (length(missing_pkgs) > 0) {
    message(
      "The following required packages are missing but cannot be installed due to no internet: ",
      paste(missing_pkgs, collapse = ", ")
    )
  }
}

if (!requireNamespace("conflicted", quietly = TRUE)) {
  install.packages("conflicted")
}

if (requireNamespace("conflicted", quietly = TRUE)) {
  if (requireNamespace("caret", quietly = TRUE)) {
    conflict_prefer("lift", "caret", quiet = TRUE)
  }
  if (requireNamespace("purrr", quietly = TRUE)) {
    conflict_prefer("flatten", "purrr", quiet = TRUE)
  }
  if (requireNamespace("dplyr", quietly = TRUE)) {
    conflict_prefer("filter", "dplyr", quiet = TRUE)
  }
}

# Function: log_message
# Logs messages related to dataset processing in a log file.
# Parameters:
#   dataset_name: Name of the dataset being processed
#   message: Message to log
# Returns: None
log_message <- function(dataset_name, message) {
  log_filename <- file.path(getwd(), paste0("processing_log_", Sys.Date(), ".txt"))
  write(paste(Sys.time(), "-", dataset_name, "-", message, "\n"),
        file = log_filename, append = TRUE)
}

# Function to load dataset based on file extension
load_dataset <- function(filepath) {
  ext <- tools::file_ext(filepath)
  tryCatch({
    switch(
      ext,
      "csv"  = read.csv(filepath, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
      "tsv"  = read.delim(filepath, stringsAsFactors = FALSE),
      "xlsx" = {
        sheets <- tryCatch(openxlsx::getSheetNames(filepath), error = function(e) NULL)
        if (is.null(sheets)) {
          log_message(basename(filepath), "Error: Cannot read XLSX sheets.")
          return(NULL)
        }
        read.xlsx(filepath, sheet = 1)
      },
      "xls"  = read_excel(filepath),
      "json" = fromJSON(filepath),
      "rds"  = readRDS(filepath),
      {
        log_message(basename(filepath), paste("Unsupported file format:", ext))
        stop("Error: Unsupported file format - ", ext)
      }
    )
  }, error = function(e) {
    log_message(basename(filepath), paste("Error loading file:", e$message))
    return(NULL)
  })
}

# Function to get mode of a categorical column
get_mode <- function(x) {
  unique_x <- unique(x[!is.na(x)])  # Exclude NA values before finding unique elements
  if (length(unique_x) == 0) 
    return(NA)
  
  unique_x[which.max(tabulate(match(x, unique_x)))]
}


# Function to inspect dataset structure and log results
inspect_structure <- function(data, dataset_name) {
  struct_info <- capture.output(str(data))
  log_message(dataset_name, "Dataset Structure:")
  write(paste(struct_info, collapse = "\n"), file = paste0("processing_log_", Sys.Date(), ".txt"), append = TRUE)
}

# Function to check for outliers using dynamic method selection
check_outliers <- function(data, dataset_name, iqr_threshold = 1.5, 
                           zscore_threshold = 3) {
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  outlier_rows <- list()
  outlier_summary <- list()  # To store summary info

  num_rows <- nrow(data)
  num_cols <- ncol(data)

  method <- ifelse(num_rows <= 1500 & num_cols <= 15, "IQR", "Z-score")

  for (col in numeric_cols) {
    if (all(is.na(data[[col]]))) next  # Skip columns with all NAs

    col_data <- data[[col]]
    col_data_no_na <- col_data[!is.na(col_data)] # Data without NAs for calc

    outlier_indices <- integer(0)

    if (method == "IQR") {
      q1 <- quantile(col_data_no_na, 0.25)
      q3 <- quantile(col_data_no_na, 0.75)
      iqr_value <- q3 - q1
      lower_bound <- q1 - iqr_threshold * iqr_value
      upper_bound <- q3 + iqr_threshold * iqr_value
      outlier_indices <- which(na.omit(col_data) < lower_bound | 
                               na.omit(col_data) > upper_bound) # Check non-NA
    } else if (method == "Z-score") {
      sd_val <- sd(col_data_no_na)
      if (sd_val == 0) {
        log_message(dataset_name, paste("Skipping Z-score outlier detection for", 
                                        col, ": No variance."))
        next
      }
      z_scores <- (col_data - mean(col_data_no_na)) / sd_val # Manual zscore
      outlier_indices <- which(abs(z_scores) > zscore_threshold)
    }

    if (length(outlier_indices) > 0) {
      outlier_rows[[col]] <- data[outlier_indices, ] # Store entire rows
      outlier_summary[[col]] <- length(outlier_indices)
    }
  }

  log_message(dataset_name, paste0("Outlier detection method used: ", method))
  log_message(dataset_name, "Outlier Check Results:")

  if (length(outlier_summary) == 0) {
    log_message(dataset_name, "No significant outliers detected.")
  } else {
    for (col in names(outlier_summary)) {
      log_message(dataset_name, paste("Column: ", col, " - Outlier Count: ", 
                                      outlier_summary[[col]]))
    }
  }

  return(outlier_rows) # Return outlier data
}

# Generate Visuals
save_visuals <- function(data, dataset_name) {
  log_message(dataset_name, "Generating visualizations.")

  if (!any(sapply(data, is.numeric))) {
    log_message(dataset_name,
                "Skipping PDF generation: No numeric columns available.")
    return()
  }

  numeric_cols <- names(data)[sapply(data, is.numeric)]

  tryCatch({
    pdf_filename <- paste0(dataset_name, "_visualizations.pdf")
    pdf(pdf_filename)
    on.exit(dev.off())

    log_message(dataset_name,
                paste("Generating visualizations PDF:", pdf_filename))
    par(mfrow = c(1, 1))
    aggr(data, col = c("navyblue", "red"), numbers = TRUE, sortVars = TRUE,
         labels = names(data), cex.axis = 0.7, gap = 3,
         main = "Missing Data Heatmap")

    if (length(numeric_cols) > 1) {
      correlation_matrix <- cor(data[numeric_cols], use = "complete.obs")
      corrplot(correlation_matrix, method = "color", tl.cex = 0.7,
               number.cex = 0.7, main = "Correlation Matrix")
    log_message(dataset_name, "Correlation matrix calculated.")
  } else {
    correlation_matrix <- NULL
  }

    log_message(dataset_name, "PDF visualizations saved.")
  }, error = function(e) {
    log_message(dataset_name, paste("Error generating PDF:", e$message))
  })
}

# Function to process a single dataset
process_dataset <- function(file) {
  cat("\nProcessing:", file, "\n")
  data <- load_dataset(file)
  if (is.null(data)) {
    log_message(basename(file), "Skipping due to loading errors.")
    return(NULL)
  }

  dataset_name <- tools::file_path_sans_ext(basename(file))
  log_message(dataset_name, "Processing started.")

  # Identify numeric and categorical columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  categorical_cols <- names(data)[sapply(data, is.character) |
                                  sapply(data, is.factor)]

  # Imputation
  for (col in numeric_cols) {
    if (any(is.na(data[[col]]))) {
      data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
    }
  }
  for (col in categorical_cols) {
    data[[col]][is.na(data[[col]])] <- get_mode(data[[col]])
  }

  # Data summary
  summary_stats <- capture.output(summary(data))
  log_message(dataset_name, paste("Summary Statistics:\n", paste(summary_stats, collapse = "\n")))
  missing_counts <- colSums(is.na(data))
  duplicate_count <- sum(duplicated(data))

  log_message(dataset_name, "Summary Statistics:")
  log_message(dataset_name, paste0("Missing Values: ", 
                                   toString(missing_counts[missing_counts > 0])))
  log_message(dataset_name, paste0("Duplicate Records: ", duplicate_count))

  # Outlier detection
  check_outliers(data, dataset_name)

  # Ensure save_visuals is called
  save_visuals(data, dataset_name)

  # Ensure column names are clean for Excel
  names(data) <- make.names(names(data), unique = TRUE)

  # Generate timestamp for versioning
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Ensure data.table is loaded
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required but not installed.")
  }

  # Write CSV efficiently with proper encoding
  data.table::fwrite(data, file = paste0(dataset_name, "_cleaned_", timestamp, ".csv"),
                     row.names = FALSE, bom = TRUE, quote = FALSE, sep = ",")
  log_message(dataset_name, "Processing completed. Cleaned file saved.")
}

library(parallel)

# Improved parallel processing setup
process_all_datasets <- function(directory) {
  files <- list.files(
    directory, full.names = TRUE,
    pattern = "\\.(csv|tsv|xlsx|xls|json|rds)$"
  )
  files <- files[!grepl(
    "_cleaned_\\d{8}_\\d{6}\\.", files, ignore.case = TRUE
  )] # Exclude already processed files

  num_cores <- max(1, detectCores() - 1)

  if (.Platform$OS.type == "windows") {
    cl <- makeCluster(num_cores, type = "PSOCK")
    on.exit({
      stopCluster(cl)
    })

    clusterEvalQ(cl, {
      library(data.table)
      library(openxlsx)
      library(readxl)
      library(jsonlite)
      library(caret)
      library(corrplot)
      library(VIM)
    })

    clusterExport(
      cl, varlist = c(
        "process_dataset", "log_message", "load_dataset",
        "get_mode", "check_outliers", "save_visuals"
      ), envir = environment()
    )

    parLapply(cl, files, function(file) {
      tryCatch({
        process_dataset(file)
      }, error = function(e) {
        log_message(basename(file), paste("Error processing file:", e$message))
        return(NULL)
      })
    })

    stopCluster(cl)
  } else {
    mclapply(files, function(file) {
      tryCatch({
        process_dataset(file)
      }, error = function(e) {
        log_message(basename(file), paste("Error processing file:", e$message))
        return(NULL)
      })
    }, mc.cores = num_cores)
  }

  cat("\nProcessing complete for all files in", directory, "\n")
}

# Clear temporary files to prevent clutter
unlink(list.files(tempdir(), full.names = TRUE), recursive = TRUE)

# data_processor.r

# ---- How to Invoke this Script ----
#    set working directory first
#    Source the script:
#    In an R script: source("path/to/data_processor.r")
#    In an RMarkdown file:
#    ```{r}
#    source("path/to/data_processor.r")
#    process_all_datasets("path/to/your_data_directory") # Or set working directory first
#    ```
#    In R console: source("path/to/data_processor.r")
#    process_all_datasets("path/to/your_data_directory") # Or set working directory first
#    The script is designed to process datasets in a specified directory, clean the data, and generate summary statistics and visualizations. It uses a variety of R packages for data manipulation, imputation, outlier detection, and visualization.

#  The script defines two main functions: process_dataset and process_all_datasets. The process_dataset function loads a single dataset, performs data cleaning operations (e.g., imputation, outlier detection), generates summary statistics and visualizations, and saves the cleaned data and results to files. The process_all_datasets function processes all datasets in a specified directory by calling the process_dataset function for each file.
#  The script supports various file formats, including CSV, TSV, XLSX, XLS, JSON, and RDS. It automatically detects the file format based on the extension and loads the data accordingly.
#  The data cleaning steps include imputing missing values with the median for numeric columns and the mode for categorical columns, detecting outliers using the IQR method, and generating a correlation matrix for numeric columns.
#  The script also generates visualizations, such as a missing data heatmap, a correlation matrix plot, and summary statistics for each dataset. The visualizations are saved in a PDF file, and the summary statistics are saved in a text file.

#  To use the script, you need to specify the directory containing your datasets in the process_all_datasets function call. You can also customize the data cleaning and visualization steps based on your specific requirements.
#  After running the script, you should see cleaned datasets, summary statistics, and visualizations saved in the specified directory. You can then use these cleaned datasets for further analysis or modeling tasks.
#  By following this approach, you can automate the data cleaning process for multiple datasets, save time, and ensure consistency in data quality checks and transformations across different datasets.

#  I hope this helps! Let me know if you have any questions.