################################################################################
# this script was written on debian bullseye with TLS protocol version 1.2.
# the webpage 'https://theclergydatabase.org.uk' does not support TLS v. 1.2 or higher.
# If open-ssl is used for protocol encrytion the SECLEVEL parameter might (temporary) be set 
# from 2 to 1. On debian (and most likely on other UNIX architectures) this might be found in the corresponding
# config file, see: /etc/ssl/openssl.cnf [from command line or 'system' command in R: cat /etc/ssl/openssl.cnf | grep DEFAULT@SECLEVEL"]
################################################################################
################################################################################
library(RCurl)
library(XML)
library(data.table)
library(checkmate)
##
scrape.html <- function(url, path.data, file.extension = "csv", ...) {
  #'
  #'
  #' @param url url of website
  #' @param path.data folder in which data is written
  #' @param file.extension format of data to write
  #'
  #'
  ## some small checks
  assertCharacter(url)
  assertDirectoryExists(path.data)
  assertCharacter(path.data)
  assertChoice(format, choices = c(".csv", "csv"))
  ## get webpage raw data
  page <- RCurl::getURL(url)
  page.trim <- gsub(pattern = "[[:space:]]", x = page, replacement = "", perl = TRUE)
  tbls <- XML::readHTMLTable(page)
  ## get missing information not contained in tables but in summary list
  pattern.summary <- "<h3>Summary</h3>.*?</ul>"
  smry.list <- regmatches(page.trim, gregexpr(pattern = pattern.summary, text = page.trim))[[1]]
  pattern.items <- "(?<=<li><label>).*?(?=</li>)"
  smry.list <- regmatches(smry.list, gregexpr(pattern = pattern.items, text = smry.list, perl = TRUE))[[1]]
  smry.list <- strsplit(smry.list, split = "</label>")
  smry.list <- lapply(smry.list, function(x) gsub(pattern = "&nbsp|;|:", replacement = "", x))
  smry.names <- sapply(smry.list, "[[", 1)
  smry.names <- gsub(pattern  = "\\(", replacement = " \\(", x = smry.names)  # note: '(' is meta character
  smry.data <- sapply(smry.list, "[[", 2)
  tbl.smry <- as.data.table(matrix(smry.data, nrow = 1))
  colnames(tbl.smry) <- smry.names
  ## get missing information not contained as data cells but in tag references
  location <- regmatches(page.trim, gregexpr(pattern = "(?<=Location:).*?(?=</h1>)", text = page.trim, perl = TRUE))[[1]]
  location.name <- regmatches(location, gregexpr(pattern = "(?<=:).*$", text = location, perl = TRUE))[[1]]
  location.name <- data.table(Location_Name = location.name)
  location.type <- regmatches(location, gregexpr(pattern = "^.*?(?=:)", text = location, perl = TRUE))[[1]]
  location.type <- data.table(Location_Type = location.type)
  person.id <- regmatches(page.trim, gregexpr(pattern = "(?<=PersonID=)[[:digit:]]*|notgiven,", text  = page.trim, perl = TRUE))[[1]]
  person.id <- data.table(Person_ID = person.id)
  cced.id <- regmatches(page.trim, gregexpr(pattern = "(?<=<dfn>).*?(?=</dfn>)", perl = TRUE, text = page.trim))[[1]]
  cced.id <- data.table(CCEd_ID = cced.id)
  # county_	diocesejurisdiction__	diocesegeographic__	parish	cced_id	names	personid	year	type	office	full	inlocation_
  ### create final data.table
  tbl <- as.data.table(cbind(tbls[[1]], tbls[[2]]))
  tbl.names <- names(tbl)
  tbl.names <- gsub(pattern = "[[:space:]]", replacement = "_", x = tbl.names, perl = TRUE)
  colnames(tbl) <- tbl.names
  tbl <- tbl[, .(CCE_Region, Archdeaconry, Name_as_Recorded, Year, Type, Office)]
  tbl <- as.data.table(cbind(
    tbl.smry,
    location.type,
    location.name,
    cced.id,
    tbl[, .(Name_as_Recorded, Year, Type, Office)]
  ))
  tbl.names <- names(tbl)
  tbl.names <- gsub(pattern = "[[:space:]]", replacement = "_", x = tbl.names, perl = TRUE)
  names(tbl) <- tbl.names
  ## write
  file.extension <- gsub(pattern = "\\.", replacement = "", x = file.extension)  # ensure no dot prefix contained
  filename <- paste0(path.data, .Platform$file.sep, "cced_id_", as.character(cced.id), ".", file.extension)
  fwrite(x = tbl, file = filename )
}