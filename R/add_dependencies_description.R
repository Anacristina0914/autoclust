# list dependencies
pkg_list = c("cluster","dplyr",
"Rtsne","ggplot2","umap",
"anocva","readxl","dplyr",
"ggpubr","naniar", "lubridate",
"survival","ISLR","caret","qqman",
"scatterplot3d","magrittr",
"knitr","ggridges","viridis",
"ggpubr","GGally","reshape2",
"plotly","heatmaply","ggcorrplot",
"umap","anocva","readxl",
"lavaan","mediation","factoextra",
"pheatmap", "factoextra")

# Add them to DESCRIPTION
for (pkg in pkg_list){
  usethis::use_package(pkg)
}

# Create Rd files for function with roxy comments
roxygen2::roxygenise()
