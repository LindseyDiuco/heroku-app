# Install required dependencies, packages, and libraries.

# Core Shiny functionality
install.packages('shiny')
install.packages('shinydashboard')
install.packages('shinyWidgets')

# Data manipulation and analysis
install.packages('dplyr')
install.packages('tidyverse')     # Includes ggplot2, readr, and other tidyverse packages
install.packages('tidymodels')    # Modeling packages
install.packages('broom')         # Tidying model output
install.packages('magrittr')      # Pipes

# Visualization
install.packages('viridis')       # Color scales
install.packages('UpSetR')        # UpSet plots
install.packages('shapviz')       # SHAP visualizations
install.packages('leaflet')       # Interactive maps
install.packages('survminer')     # Survival analysis visualizations

# Geospatial data
# install.packages('sf')            # Geospatial data
# install.packages('rgeos')
# install.packages('rnaturalearth') # Natural Earth map data
# install.packages('rnaturalearthdata') # Natural Earth map data

# Statistical and survival analysis
install.packages('boot')          # Bootstrap methods
install.packages('survival')      # Survival analysis
install.packages('fitdistrplus')  # Distribution fitting
install.packages('binom')         # Binomial distribution

# Time series and forecasting
install.packages('forecast')      # Time series forecasting
install.packages('TTR')

# Date and time manipulation
install.packages('lubridate')     # Date and time manipulation

# Reporting and documentation
install.packages('knitr')         # Dynamic report generation
install.packages('kableExtra')    # Enhanced table formatting
install.packages('rmarkdown')     # Dynamic documents
install.packages('tinytex')       # Lightweight TeX distribution

# SHAP explanations
install.packages('kernelshap')    # SHAP explanations

# Utility
install.packages('glue')          # String interpolation
