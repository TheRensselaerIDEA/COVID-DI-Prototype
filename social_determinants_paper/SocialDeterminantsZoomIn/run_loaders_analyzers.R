# title: "run_loaders_analzyers"
# author: Ross DeVito
#
# Run current app's Loaders and Analyzers so that their respective
#   data frames and analysis functions are in global environment
#
# As of september 18, these are:
#   data.frames:
#       cdc.data
#       chr.data.2019
#       chr.namemap.2019
#       geo.namemap
#
#   analysis functions:
#       pca.func
#       km.func
#       km.wssplot
#       kendall.func

# save current directory so can return to it after running Loaders/Analyzers
current.dir <- getwd()

# set working directory to current apps init
path.to.current.app <- file.path("..", "init")
setwd(file.path(path.to.current.app))

source("Librarian.R")
source("Loader_CHR2019.R")
source("Loader_CDC.R")
source("Loader_GEO.R")
source("Analyzer_PCA.R")
source("Clustering_Lib.R")
source("Analyzer_Correlation.R")
source("Theme.R") # for styles for plots needed in run_app_analysis, could be optional

# revert to original working directory
setwd(current.dir)
