################################################################################
rm(list = ls())
################################################################################
# this script was written on debian bullseye with TLS protocol version 1.2.
# the webpage 'https://theclergydatabase.org.uk' does not support TLS v. 1.2 or higher.
# If open-ssl is used for protocol encrytion the SECLEVEL parameter might (temporary) be set 
# from 2 to 1. On debian (and most likely on other UNIX architectures) this might be found in the corresponding
# config file, see: /etc/ssl/openssl.cnf [from command line or 'system' command in R: cat /etc/ssl/openssl.cnf | grep DEFAULT@SECLEVEL"]
################################################################################
library(RCurl)
library(XML)
library(data.table)
library(checkmate)
################################################################################
path.script <- rstudioapi::getSourceEditorContext()$path  # .../src/R/main.R
path.base <- dirname(path.script)  # .../src/R/
path.base <- dirname(path.base)  # .../src/
path.base <- dirname(path.base)  # .../
path.data <- paste0(path.base, .Platform$file.sep, "data")  # .../data/
path.log <- paste0(path.base, .Platform$file.sep, "log")  # .../logs
## load created function(s)
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "functions.R"))
## assemble and write data
assemble.data(idx.seq = 3:5000, path.data = path.data, path.log = path.log, logging = TRUE, write.file = TRUE, file.name = "data-english-church")
data.web <- fread(paste0(path.data, .Platform$file.sep, "data-english-church.csv"),
                  stringsAsFactors = TRUE)