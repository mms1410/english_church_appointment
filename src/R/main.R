################################################################################
rm(list = ls())
set.seed(123)
################################################################################
# this script was written on debian bullseye with TLS protocol version 1.2.
# the webpage 'https://theclergydatabase.org.uk' does not support TLS v. 1.2 or higher.
# If open-ssl is used for protocol encrytion the SECLEVEL parameter might (temporary) be set 
# from 2 to 1 for a quick workaround. On debian (and most likely on other UNIX architectures) this might be found in the corresponding
# config file, see: /etc/ssl/openssl.cnf [from command line or 'system' command in R: cat /etc/ssl/openssl.cnf | grep DEFAULT@SECLEVEL"]
################################################################################
library(data.table)
library(ggplot2)
#library(patchwork)
#library(sf)
#library(tmap)
library(maps)
library(viridis)
################################################################################
path.script <- rstudioapi::getSourceEditorContext()$path  # .../src/R/main.R
path.base <- dirname(path.script)  # .../src/R/
path.base <- dirname(path.base)  # .../src/
path.base <- dirname(path.base)  # .../
path.data <- paste0(path.base, .Platform$file.sep, "data")  # .../data/
path.assets <- paste0(path.base, .Platform$file.sep, "assets")
default_theme <- theme_minimal() +
  theme(text = element_text(family = "Bookman", size = 15)) +
  theme(panel.grid.major = element_line(color = "grey", size = 0.3)) +
  theme(axis.line = element_line(color = "black", size = 0.4))
UK <- subset(map_data("world"), region == "UK")
################################################################################
## load data
data <- fread(paste0(path.data, .Platform$file.sep, "final_data.csv"))
data <- data[Population != 0]
data.agg <- data[ , .(Population = sum(Population), Appointments = sum(Appointments)), by = time]
data.agg[, `:=`(lnPop = log(Population), lnApp = log(Appointments))]
## plot church appointments
ggplot(data) +
  geom_point(aes(x = time, y = Appointments, color = as.factor(C_ID), alpha = 0.4)) +
  default_theme +
  theme(legend.position = "none") +
  labs(title = "Number of church appointments", subtitle = "(colored by C_ID)")
ggsave(paste0(path.assets, .Platform$file.sep, "scatterplot_church-appointments.pdf"))
## plot population
ggplot(data) +
  geom_point(aes(x = time, y = Population, color = as.factor(C_ID), alpha = 0.4)) +
  default_theme +
  theme(legend.position = "none") +
  labs(title = "Population", subtitle = "(colored by C_ID)")
ggsave(paste0(path.assets, .Platform$file.sep, "scatterplot_population.pdf"))
## plot population variation
ggplot() +
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.4) +
  geom_point(data = data, aes(x = longitude, y = latitude, color = Population)) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() +
  labs(title = "Variation in Population")
ggsave(paste0(path.assets, .Platform$file.sep, "variation_population.pdf"))
## plot cced variation
ggplot() +
  geom_polygon(data = UK, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.4) +
  geom_point(data = data, aes(x = longitude, y = latitude, fill = C_ID)) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() +
  theme(legend.position = "none") +
  labs(title = "Variation in CCEd_ID")
ggsave(paste0(path.assets, .Platform$file.sep, "variation_cced.pdf"))
## plot appointment vs. population
ggplot(data = data.agg) +
  geom_point(aes(x = log(Population), y = log(Appointments))) +
  geom_line(aes(x = log(Population), y = log(Appointments))) +
  labs(title = "Scatterplot (log-)Appointments vs. (log-)Population") +
  default_theme
ggsave(paste0(path.assets, .Platform$file.sep, "scatterplot_app-vs-pop.pdf"))
################################################################################
idx.train <- sample(seq(nrow(data)), floor(nrow(data) * 0.75))
data.train <- data[idx.train]
data.test <- data[-idx.train]
model.linear <- lm(formula = Population ~ time + C_ID + latitude + longitude + Appointments, data = data.train)
model.linear.pred <- predict(object =  data.test)
################################################################################