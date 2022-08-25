################################################################################
rm(list = ls())
################################################################################
# this script was written on debian bullseye with TLS protocol version 1.2.
# the webpage 'https://theclergydatabase.org.uk' does not support TLS v. 1.2 or higher.
# If open-ssl is used for protocol encrytion the SECLEVEL parameter might (temporary) be set 
# from 2 to 1. On debian (and most likely on other UNIX architectures) this might be found in the corresponding
# config file, see: /etc/ssl/openssl.cnf [from command line or 'system' command in R: cat /etc/ssl/openssl.cnf | grep DEFAULT@SECLEVEL"]
################################################################################
library(data.table)
library(ggplot2)
library(sf)
################################################################################
path.script <- rstudioapi::getSourceEditorContext()$path  # .../src/R/main.R
path.base <- dirname(path.script)  # .../src/R/
path.base <- dirname(path.base)  # .../src/
path.base <- dirname(path.base)  # .../
path.data <- paste0(path.base, .Platform$file.sep, "data")  # .../data/
default_theme <- theme_minimal() +
  theme(text = element_text(family = "Decima WE", size = 15)) +
  theme(panel.grid.major = element_line(color = "grey", size = 0.3)) +
  theme(axis.line = element_line(color = "black", size = 0.4))
################################################################################
data <- fread(paste0(path.data, .Platform$file.sep, "final_data.csv"))
data <- data[Population != 0]

ggplot(data) +
  geom_point(aes(x = time, y = Appointments, color = as.factor(C_ID), alpha = 0.4)) +
  default_theme +
  theme(legend.position = "none") +
  labs(title = "Number of church appointments", subtitle = "(colored by C_ID)")
  
my_sf <- st_as_sf(data, c("longitude", "latitude"))
ggplot()

