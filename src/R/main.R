################################################################################
rm(list = ls())
################################################################################


path.script <- rstudioapi::getSourceEditorContext()$path  # .../src/R/main.R
path.data <- dirname(path.script)  # .../src/R/
path.data <- dirname(path.data)  # .../src/
path.data <- dirname(path.data)  # .../
path.data <- paste0(path.data, .Platform$file.sep, "data")  # .../data/
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "01_scrape_data"))

url.base <- "https://theclergydatabase.org.uk/jsp/locations/DisplayLocation.jsp?locKey="
for (key in seq(3,400)) {
  key <- 3
  url <- paste0(url.base, key)
}

