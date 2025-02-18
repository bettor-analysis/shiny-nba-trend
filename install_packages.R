
# Run this before the app to make sure all packages are installed
# before running trendNBAapp.R Shiny App


# install_packages.R

# List of required packages
required_packages <- c(
  "shiny",
  "shinythemes",
  "reactable",
  "htmltools",
  "ggplot2",
  "dplyr",
  "hoopR",
  "tidyverse",
  "plotly",
  "shinycssloaders",
  "openxlsx"
)

# Install any missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:\n")
  print(missing_packages)
  install.packages(missing_packages, dependencies = TRUE)
} else {
  cat("All required packages are already installed.\n")
}

# Confirm installation
cat("Package installation complete. You can now run the app.")


