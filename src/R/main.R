################################################################################
rm(list = ls())
################################################################################
path.script <- rstudioapi::getSourceEditorContext()$path  # .../src/R/main.R
path.base <- dirname(path.script)  # .../src/R/
pathbase <- dirname(path.base)  # .../src/
pathbase <- dirname(path.base)  # .../
path.data <- paste0(path.base, .Platform$file.sep, "data")  # .../data/
path.logs <- paste0(path.base, .Platform$file.sep, "logs")  # .../logs
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "01_scrape_data.R"))

url.base <- "https://theclergydatabase.org.uk/jsp/locations/DisplayLocation.jsp?locKey="
for (key in seq(200,300)) {
  url <- paste0(url.base, key)
  tryCatch({
    scrape.html(url = url, path.data = path.data)
  },
  error = function(e) {
    message("Unexpecter error occured.\n")
    print(e)
  },
  warning = function(w) {
    print(w)
  })
}
## key=14 => two histories
# 150, 193
file.extension <-"csv"
key <- 193
