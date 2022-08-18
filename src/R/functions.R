################################################################################
#
################################################################################
library(RCurl)
library(XML)
library(data.table)
library(checkmate)
##
scrape.html <- function(url) {
  #'
  #'
  #' @param url url of website
  #'
  ## some small checks
  assertCharacter(url)
  loc.key <- regmatches(url, regexpr(pattern = "[0-9]*$", text  = url, perl = TRUE))
  ## get webpage raw data
  page <- RCurl::getURL(url)
  page.trim <- gsub(pattern = "[[:space:]]", x = page, replacement = "", perl = TRUE)
  ## check connection
  if (grepl(pattern = "httpstatus500", x = page.trim, ignore.case = TRUE)) {
    msg <- paste0("URL ", url, " request resulted in 500 Internal Server Error.\n")
    warning(msg)
  }
  tbls <- XML::readHTMLTable(page)
  ## check tables
  if (!grepl(pattern = "<li><h3>Evidence</h3>", x = page.trim)) {
    msg <- paste0("URL ", url, " does not contain 3rd level 'Evidence' list entry.\n")
    warning(msg)
  }
  if( !grepl(pattern = "<li><h3>Summary</h3>", x = page.trim)) {
    msg <- paste0("URL ", url, " does not contain 3rd level 'Summary' list entry.\n")
    warning(msg)
  }
  if (!grepl(pattern = "<li><h3>History</h3>", x = page.trim)) {
    msg <- paste0("URL ", url, " does not contain 3rd level 'History' list entry.\n")
    warning(msg)
  }
  ## check County and Diocese
  if (!grepl(pattern = "<li><label>County:", x = page.trim)) {
    msg <- paste0("URL ", url, " does not contain County list istem.\n")
    warning(msg)
  }
  if (!grepl(pattern = "<li><label>Diocese.*?(Jurisdiction)", x = page.trim)) {
    msg <- paste0("URL ", url, " does not contain Diocese (Jurisdiction) list istem.\n")
    waring(msg)
  }
  if(!grepl(pattern = "<li><label>Diocese.*?(Geographic)", x = page.trim)) {
    msg <- paste0("URL ", url, " does not contain Diocese (Geographic) list istem.\n")
    warning(msg)
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
  tbl.smry <- tbl.smry[, .(County, `Diocese_(Jurisdiction)`, `Diocese_(Geographic)`)]
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
  ## tidy up
  cols.char <- names(lapply(X = tbl, FUN = typeof) == "character")
  tbl[, (cols.char) := lapply(X = .SD, FUN = function(column){gsub(x = column, pattern = "[[:space:]]{2,}", replacement = "", perl = TRUE)}), .SDcols = cols.char]
  ## return data.table
  tbl
}
assemble.data <- function(idx.seq, url.base = "https://theclergydatabase.org.uk/jsp/locations/DisplayLocation.jsp?locKey=", file.name = NULL, file.extension = "csv", path.data = NULL,   path.log = NULL, write.file = FALSE, verbose = TRUE, logging = FALSE) {
  #'
  #' @param idx.seq
  #' @param url.base
  #' @param file.name
  #' @param file.extension
  #' @param path.data
  #' @param path.log
  #' @param write.file
  #' @param verbose
  #' @param logging
  #' 
  # TODO:
  #  file.name file extension overlap
  assertFlag(verbose)
  assertFlag(logging)
  assertFlag(write.file)
  if (write.file) {
    assertChoice(file.extension, choices = c(".csv", "csv"))
    assertCharacter(path.data)
    assertDirectoryExists(path.data)
  }
  if (logging) {
    assertDirectoryExists(path.log)
  }
  if (write.file) {
    assertCharacter(file.name, any.missing = FALSE, len = 1)
  }
  if (!grepl(pattern = "^\\.", x = file.extension)) file.extension <- paste0(".", file.extension)
  tbl.merge <- NULL
  if (logging) {
    file.log <- gsub(x = Sys.time(), pattern = "[[:space:]]|:", replacement = "-", perl = TRUE)
    file.log <- paste0(path.log, .Platform$file.sep, "log_", file.log, ".txt")
    system(command = paste0("touch ", file.log))
  }
  for (loc.key in idx.seq) {
    url <- paste0(url.base, loc.key)
    tryCatch({
      tbl <- scrape.html(url = url)
      tbl.merge <- data.table(rbind(tbl.merge, tbl))
      if (verbose) cat(paste0("Appended data table with ", nrow(tbl), "\t", "new rows at loc key ", loc.key, "\n"))
      if (logging) cat(paste0("Appended data table with ", nrow(tbl), "\t", "new rows at loc key ", loc.key, "\n"), file = file.log, append = TRUE)
    },
    error = function(e) {
      msg <- paste0("Unexpected error occured at loc.key ", loc.key, ": ", e$message)
      message(msg)
      if (logging) cat(msg, file = file.log, append = TRUE)
    },
    warning = function(w) {
       msg <- paste0(w$message)
       message(msg)
       if (logging) cat(msg, file = file.log, append = TRUE)
    })
  }
  if (write.file) {
    fwrite(x = tbl.merge, file = paste0(path.data, .Platform$file.sep, file.name, file.extension))
    if (logging) cat(paste0("Data written to: ", path.data, .Platform$file.sep, file.name, file.extension, "\n"), file = file.log, append = TRUE)
    if (verbose) cat(paste0("Data written to: ", path.data, .Platform$file.sep, file.name, file.extension, "\n"))
  } else {
    return(tbl)
  }
}
