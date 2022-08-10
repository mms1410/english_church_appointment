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
scrape.html <- function(url, path.data, file.extension = "csv", verbose = TRUE, logging = FALSE, ...) {
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
  assertChoice(file.extension, choices = c(".csv", "csv"))
  ## get webpage raw data
  page <- RCurl::getURL(url)
  page.trim <- gsub(pattern = "[[:space:]]", x = page, replacement = "", perl = TRUE)
  tbls <- XML::readHTMLTable(page)
  ##
  if (!grepl(pattern = "<li><h3>Evidence</h3>", x = page.trim)) {
    if (verbose) {
      warning(paste0("URL ", url, " does not contain 3rd level 'Evidence' list entry.\n"))
      #cat(paste0(paste0(rep("=", 78), collapse = ""), "\n"))
      #cat(paste0("URL ", url, " does not contain 3rd level 'Evidence' list entry.\n"))
      #cat(paste0(paste0(rep("=", 78), collapse = ""), "\n"))
    }
    return(NULL)
  }
  if( !grepl(pattern = "<li><h3>Summary</h3>", x = page.trim)) {
    if (verbose) {
      warning(paste0("URL ", url, " does not contain 3rd level 'Summary' list entry.\n"))
      #cat(paste0(paste0(rep("=", 78), collapse = ""), "\n"))
      #cat(paste0("URL ", url, " does not contain 3rd level 'Summary' list entry.\n"))
      #cat(paste0(paste0(rep("=", 78), collapse = ""), "\n"))
    }
    return(NULL)
  }
  if (!grepl(pattern = "<li><h3>History</h3>", x = page.trim)) {
    if (verbose) {
      warning(paste0("URL ", url, " does not contain 3rd level 'History' list entry.\n"))
      #cat(paste0(paste0(rep("=", 78), collapse = ""), "\n"))
      #cat(paste0("URL ", url, " does not contain 3rd level 'History' list entry.\n"))
      #cat(paste0(paste0(rep("=", 78), collapse = ""), "\n"))
    }
    return(NULL)
  }
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
  colnames(tbl.smry) <- gsub(pattern = "[[:space:]]", replacement = "_", x = colnames(tbl.smry), perl = TRUE)
  tbl.smry <- tbl.smry[, .(County, `Diocese_(Jurisdiction)`, `Diocese_(Jurisdiction)`)]
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
  ### create final data.table
  ## note: tbl.history not nec. one row
  tbl.history <- as.data.table(tbls[[1]])
  colnames(tbl.history) <- gsub(pattern = "[[:space:]]", replacement = "_", x = colnames(tbl.history), perl = TRUE)
  tbl.history <- tbl.history[, .(CCE_Region)]
  tbl.history <- unique(tbl.history)
  tbl.evidence <- as.data.table(tbls[[2]])
  colnames(tbl.evidence) <- gsub(pattern = "[[:space:]]", replacement = "_", x = colnames(tbl.evidence), perl = TRUE)
  tbl.evidence <- tbl.evidence[, .(Name_as_Recorded, Year, Type, Office)]
  ### merge all together
  tbl <- as.data.table(cbind(
    tbl.smry,
    location.type,
    location.name,
    cced.id,
    tbl.history,
    tbl.evidence
  ))
  ### write
  file.extension <- gsub(pattern = "\\.", replacement = "", x = file.extension)  # ensure no dot prefix contained
  filename <- paste0(path.data, .Platform$file.sep, "cced_id_", as.character(cced.id), ".", file.extension)
  fwrite(x = tbl, file = filename )
  if (verbose) {
    cat(paste0(paste0(rep("=", 78), collapse = ""), "\n"))
    cat(paste0("Write data to ", filename, "\n"))
    cat(paste0(paste0(rep("=", 78), collapse = ""), "\n")) 
  }
}
