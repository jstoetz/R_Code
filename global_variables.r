##Global Variables

# <----- 0. Clear Memory and Restart R ----->
gc()
rm(list = ls())
.rs.restartR()


# <------ 1. Install packages (if required) ----->
#install.packages(
#  c(
#    'dplyr',
#    'devtools',
#    'data.table',
#    'stringr',
#    'rvest',
#    'readr',
#    'filesstrings',
#    'data.table',
#    'reticulate',
#   'TTR',
#   'QuantTools',
#   'stringi',
#   'lubridate',
#   'BatchGetSymbols',
#   'tidyquant',
#   'quantmod',
#   'tidyr',
#   'tidyverse',
#   'ggplot2',
#   'purrr',
#   'DescTools',
#   'xlxs',
#   'rmarkdown',
#   'forecast',
#   'zoo',
#   'purrr',
#   'reshape2',
#   'rmarkdown',
#   'markdown',
#   'knitr',
#   'bookdown',
#   'xts',
#   'downloader',
#   'kableExtra'
#  )
#)

#<----- 2. Load required libraries ----->
library(dplyr)
library(devtools)
library(data.table)
library(stringr)
library(rvest)
library(readr)
library(filesstrings)
library(data.table)
library(reticulate)
library(TTR)
library(QuantTools)
library(stringi)
library(lubridate)
library(BatchGetSymbols)
library(tidyquant)
library(quantmod)
library(tidyr)
library(ggplot2)
library(purrr)
library(DescTools)
library(xls)
library(rmarkdown)
library(forecast)
library(zoo)
library(purrr)
library(reshape2)
library(rmarkdown)
library(markdown)
library(knitr)
library(bookdown)
library(xts)
library(downloader)
library(kableExtra)
library(RcppAlgos)
library(gtools)

#<----- 4. Set working directory and file paths ----->

# - where data, function, variable and temnplate files live
root.directory      <- "C:/Data_Files/"
if(getwd() != root.directory){setwd(root.directory)}
if(!dir.exists(root.directory)){dir.create(root.directory)}

# - Keep tenplate (R, Py and C++) files in C:/Data Files
template.directory <- paste0(root.directory,"Template_Directory/")
if(!dir.exists(template.directory)){dir.create(template.directory)}

# - Store R functions
function.directory <- paste0(root.directory,"Function_Directory/")
if(!dir.exists(function.directory)){dir.create(function.directory)}

# - Store R functions
variable.directory <- paste0(root.directory,"Variable_Directory/")
if(!dir.exists(variable.directory)){dir.create(variable.directory)}

#<----- 5.  Load functions from function.directory, templates from template.directory and variables from variable.directory ----->
lapply(list.files(function.directory,full.names = T),function(x)(source(x)))
lapply(list.files(template.directory,full.names = T),function(x)(source(x)))
lapply(list.files(variable.directory,full.names = T),function(x)(source(x)))

#<------ 5. Change/update required paramaters ----->

# - Date and Version Control Variables
p.year                <- 2021   #enter current year
p.current.version     <- 1.05   #enter the current version number - would like to run weekly
p.prior.version       <- p.current.version - .01
