requiredPackages <- c("ggplot2", "ggrepel", 
                      "dplyr",  "RSQLite",
                      "mice",   "VIM",
                      "XML",  "implied",
                      "visdat",  "UpSetR",
                      "tidyr", "DMwR",
                      "esquisse", "heplots",
                      "ggrepel", "missForest",
                      "robustbase", "missMDA",
                      "e1071","knn",
                      "tabplot",  "ggcorrplot",
                      "inspectdf","ggbiplot",
                      "devtools", "purrr",
                      "xml2", "magrittr",
                      "viridis", "tibble",
                      "pander", "ggsoccer")



install.packages("ggsoccer")

for (pac in requiredPackages) {
  if(!require(pac,  character.only=TRUE)){
    install.packages(pac, repos="http://cran.rstudio.com")
    library(pac,  character.only=TRUE)
  } 
}
rm(pac)
rm(requiredPackages)

#devtools::install_github("FCrSTATS/SBpitch")

#install_github("vqv/ggbiplot")