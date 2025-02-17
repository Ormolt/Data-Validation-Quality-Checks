# 📌 Data Validation and Quality Check Script

## 🚀 Overview
This **R script** automates the process of **data validation, quality checks, and preprocessing**. It ensures datasets are loaded correctly, detects **missing values**, identifies **outliers**, and generates **summary statistics and visualizations**. The script supports multiple file formats and includes **parallel processing** capabilities for efficient execution.

## ✨ Features
- ✅ **Automatic Package Installation**: Ensures required R packages are installed and loaded.
- 🌍 **Internet Connectivity Check**: Determines if required packages can be installed.
- 📂 **File Format Support**: Supports `CSV`, `TSV`, `XLSX`, `XLS`, `JSON`, and `RDS` formats.
- 🛠 **Data Cleaning**: Handles **missing values** and **outliers**.
- 📊 **Visualization Generation**: Creates **missing data heatmaps** and **correlation matrices**.
- ⚡ **Parallel Processing**: Processes multiple datasets concurrently.
- 📝 **Logging Mechanism**: Records processing details in a log file.

## 📦 Required Packages
The script ensures the following R packages are installed and loaded:
```r
c("ggplot2", "dplyr", "VIM", "caret", "corrplot", "openxlsx", "jsonlite", "readr", "readxl", "data.table", "conflicted", "pacman")
```

## 🔧 Functions

### 1️⃣ **check_internet()**
🔍 Checks if an internet connection is available using the `pingr` package.

### 2️⃣ **log_message(dataset_name, message)**
📝 Logs messages related to dataset processing.
📁 Saves logs in a text file with a timestamp.

### 3️⃣ **load_dataset(filepath)**
📂 Loads datasets based on file extensions.
❌ Logs errors for unsupported file formats.

### 4️⃣ **get_mode(x)**
📊 Determines the mode of a categorical column.

### 5️⃣ **inspect_structure(data, dataset_name)**
🔍 Captures and logs dataset structure.

### 6️⃣ **check_outliers(data, dataset_name, iqr_threshold, zscore_threshold)**
⚠️ Detects outliers using either the **IQR** or **Z-score** method.
🔄 Selects method based on dataset size.

### 7️⃣ **save_visuals(data, dataset_name)**
📊 Generates visualizations, including **missing data heatmaps** and **correlation matrices**.
📁 Saves plots in a **PDF file**.

### 8️⃣ **process_dataset(file)**
📂 Loads and cleans a **single dataset**.
📝 Logs processing steps and saves a cleaned version.

### 9️⃣ **process_all_datasets(directory)**
⚡ Processes all datasets in a **specified directory**.
🔄 Uses **parallel processing** for efficiency.

## 🔍 Use Cases
This script can be applied in various scenarios, such as:
- 📊 **Data Science & Machine Learning**: Ensuring high-quality datasets before model training.
- 🏥 **Healthcare Analytics**: Cleaning patient records and identifying missing data.
- 🏛 **Financial Analysis**: Detecting anomalies in transaction data.
- 📈 **Business Intelligence**: Automating data validation for dashboards and reports.


## ▶️ Execution Instructions

### 1️⃣ **Set the working directory**:
```r
setwd("path/to/your_data_directory")
```

### 2️⃣ **Source the script**:
```r
source("path/to/data_cleaning_setup.R")
```

### 3️⃣ **Run the processing function**:
```r
process_all_datasets("path/to/your_data_directory")
```

## 📌 Expected Output
✅ **Cleaned datasets**: Stored with a timestamp suffix.
✅ **Log file**: Contains details of processing steps.
✅ **Visualization PDF**: Includes **missing data heatmaps** and **correlation matrices**.

## ⚠️ Notes
- 🔹 Ensure **all required packages** are installed before execution.
- 🔹 The script **excludes already processed files** to avoid duplication.
- 🔹 **Parallel processing** optimizes performance on multi-core systems.

## 🤝 Contributing
Contributions are welcome! If you'd like to improve this script:
1. Fork the repository.
2. Create a new branch (`feature-branch-name`).
3. Make your changes and commit them.
4. Submit a pull request for review.

## 📜 License
This project is licensed under the **MIT License**.

## 📬 Contact
For issues, suggestions, or questions, please reach out via:
- 📧 Email: `ormolt88@gmail.com`
- 🏠 GitHub Issues: [Open an issue](https://github.com/Ormolt/Data-Validation-Quality-Checks/issues)

By using this script, **data validation and quality checks are automated**, ensuring efficient and reliable preprocessing for further analysis. 🚀
