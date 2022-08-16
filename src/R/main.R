################################################################################
rm(list = ls())
################################################################################
# this script was written on debian bullseye with TLS protocol version 1.2.
# the webpage 'https://theclergydatabase.org.uk' does not support TLS v. 1.2 or higher.
# If open-ssl is used for protocol encrytion the SECLEVEL parameter might (temporary) be set 
# from 2 to 1. On debian (and most likely on other UNIX architectures) this might be found in the corresponding
# config file, see: /etc/ssl/openssl.cnf [from command line or 'system' command in R: cat /etc/ssl/openssl.cnf | grep DEFAULT@SECLEVEL"]
################################################################################
################################################################################
path.script <- rstudioapi::getSourceEditorContext()$path  # .../src/R/main.R
path.base <- dirname(path.script)  # .../src/R/
pathbase <- dirname(path.base)  # .../src/
pathbase <- dirname(path.base)  # .../
path.data <- paste0(path.base, .Platform$file.sep, "data")  # .../data/
path.logs <- paste0(path.base, .Platform$file.sep, "logs")  # .../logs
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
              .Platform$file.sep,
              "01_scrape.R"))

url.base <- "https://theclergydatabase.org.uk/jsp/locations/DisplayLocation.jsp?locKey="
tbl.merge <- NULL
for (key in seq(430,445)) {  # 50000, 440, 452
  url <- paste0(url.base, key)
  tryCatch({
    tbl <- scrape.html(url = url, write.file = FALSE)
    tbl.merge <- data.table(rbind(tbl.merge, tbl))
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