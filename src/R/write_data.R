################################################################################
rm(list = ls())
################################################################################
library(RCurl)
library(XML)
library(data.table)
library(xts)
library(checkmate)
library(haven)
################################################################################
path.script <- rstudioapi::getSourceEditorContext()$path  # .../src/R/main.R
path.base <- dirname(path.script)  # .../src/R/
path.base <- dirname(path.base)  # .../src/
path.base <- dirname(path.base)  # .../
path.data <- paste0(path.base, .Platform$file.sep, "data")  # .../data/
path.log <- paste0(path.base, .Platform$file.sep, "log")  # .../logs/
## load created function(s)
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "functions.R"))
## assemble and write data
assemble.data(idx.seq = 3:5000, log.name = "log_3-5000", path.data = path.data, path.log = path.log,
              logging = TRUE, write.file = TRUE, file.name = "data-english-church-01")
assemble.data(idx.seq = 5001:10000, log.name = "log_5000-10000", path.data = path.data, path.log = path.log,
              logging = TRUE, write.file = TRUE, file.name = "data-english-church-02")
assemble.data(idx.seq = 10001:15000, log.name = "log_10001-15000", path.data = path.data, path.log = path.log,
              logging = TRUE, write.file = TRUE, file.name = "data-english-church-03")
assemble.data(idx.seq = 15001:20000, log.name = "log_15001-20000", path.data = path.data, path.log = path.log,
              logging = TRUE, write.file = TRUE, file.name = "data-english-church-04")
assemble.data(idx.seq = 20001:25000, log.name = "log_20001-25000", path.data = path.data, path.log = path.log,
              logging = TRUE, write.file = TRUE, file.name = "data-english-church-05")
assemble.data(idx.seq = 25001:30000, log.name = "log_25001-30000", path.data = path.data, path.log = path.log,
              logging = TRUE, write.file = TRUE, file.name = "data-english-church-06")
assemble.data(idx.seq = 30001:35000, log.name = "log_30001-35000", path.data = path.data, path.log = path.log,
              logging = TRUE, write.file = TRUE, file.name = "data-english-church-07")
assemble.data(idx.seq = 35001:40000, log.name = "log_35001-40000", path.data = path.data, path.log = path.log,
              logging = TRUE, write.file = TRUE, file.name = "data-english-church-08")
assemble.data(idx.seq = 40001:45000, log.name = "log_40001-45000", path.data = path.data, path.log = path.log,
              logging = TRUE, write.file = TRUE, file.name = "data-english-church-09")
assemble.data(idx.seq = 45001:50000, log.name = "log_45001-50000", path.data = path.data, path.log = path.log,
              logging = TRUE, write.file = TRUE, file.name = "data-english-church-10")

################################################################################
## read data
data.web <- rbind(
  fread(paste0(path.data, .Platform$file.sep, "data-english-church-01.csv")),
  fread(paste0(path.data, .Platform$file.sep, "data-english-church-02.csv")),
  fread(paste0(path.data, .Platform$file.sep, "data-english-church-04.csv")),
  fread(paste0(path.data, .Platform$file.sep, "data-english-church-05.csv"))
)
data.population <- as.data.table(haven::read_dta(paste0(path.data, .Platform$file.sep, "Population_year.dta")))
## filter data
data.web <- data.web[Year >= 1525 & Year <= 1850]
data.population <- data.population[year >= 1525 & year <= 1850]
## transform numeric year into ordered factor of binned years
year.breaks <- seq(from = 1525, to =  1850, by = 5)

data.web$Year <- cut(data.web$Year, breaks = year.breaks, include.lowest = TRUE, ordered_result = TRUE)
data.population$year <- cut(data.population$year, breaks = year.breaks, include.lowest = TRUE, ordered_result = TRUE)
data.population[data.population == 0] <- NA
##
years.rounded <- unlist(regmatches(levels(data.web$Year),
                                   gregexpr(text = levels(data.web$Year),
                                            pattern = "\\d{4}(?!\\,)",
                                            perl = TRUE)))
levels(data.web$Year) <- years.rounded
levels(data.population$year) <- years.rounded
## aggregate data.merge
data.web <- data.web[, .(Appointments = .N), by = .(CCEd_ID, Year)]
## set key used to join
setkeyv(data.web, c("CCEd_ID", "Year"))
setkeyv(data.population, c("cced_id", "year"))
data.merge <-data.population[data.web, nomatch = 0]
## factor Year to numeric Year
data.merge$year <- as.numeric(levels(data.merge$year))[data.merge$year]
## create final data
data.final <- data.table(cbind(
  time = data.merge$year,
  C_ID = data.merge$C_ID,
  latitude = data.merge$latitude,
  longitude = data.merge$longitude,
  Appointments = data.merge$Appointments,
  Population = data.merge$Population,
  lnPop = log(data.merge$Population),
  lnApp = log(data.merge$Appointments)
))
# write final data
fwrite(data.final, file = paste0(path.data, .Platform$file.sep, "final_data.csv"))
