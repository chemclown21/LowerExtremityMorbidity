#=======================================
#LE Trauma Vulnerability Project
#Step 3: Purge Columns You Don't Want
#Code written by Vitto Resnick, 10/09/25
#=======================================

#========= UTILITIES =========
rm(list = ls(all.names = TRUE))

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(cobalt)
library(WeightIt)
library(survey)

#========= LOADING DATA =========
# Read in your file (assume you named it classified_data.csv)
files_directory = '/Users/vresnick/Documents/GitHub/LowerExtremityMorbidity'
setwd(files_directory)
outdir = "outputs"

dt <- readRDS("outputs/2_decoded_Pre_Post_251009.rds")


