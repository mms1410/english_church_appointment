################################################################################
#
#
################################################################################
rm(list = ls())
#
library(data.table)
library(checkmate)
#
path.data <- rstudioapi::getSourceEditorContext()$path
path.data <- dirname(path.data) ## R
path.data <- dirname(path.data) ## src
path.data <- dirname(path.data) ## 'home'
path.data <- paste0(path.data, .Platform$file.sep, "data")
 # 
files.merge <- function(path.data, file.ending = "csv", archive = TRUE, ...) {
 #'
 #' @param path.folder
 #' @param file.ending
 #'
 #'
 #' ToDo:
 #' FileEnding
 assertCharacter(path.data)
 assertCharacter(file.ending)
 assertFlag(archive)
 assertChoice(ending, choices = c("csv", ".csv"))
 ##
 file.pattern = paste0(file.ending, "$")
 tbl.final <- data.table(County = factor(),
                          Diocese_Juristiction = factor(),
                          Diocese_Geographic = factor(),
                          Location_Type = factor(),
                          Location_Name = factor(),
                          CCEd_ID = integer(),
                          Name = factor(),
                          Person_ID = integer(),
                          Year = integer(),
                          Type = factor(),
                          Office = factor())
  
  optional <- list(...)
  path.archive <- optional[["path.archives"]]
  path.turds <- paste0(path.data, .Platform$file.sep, "turds")
  if (!file.exists(path.turds)) {
    dir.create(path.turds)
  }
  if (is.null(path.archive)) {
    path.archive <- paste0(path.data, .Platform$file.sep, "archive")
  }
  if (archive) {
    if (!file.exists(path.archive)) {
      dir.create(path.archive)
    }
  }
  ##
 for (file in list.files(path.data, full.names = TRUE, pattern = file.pattern)) {
   tbl <- data.table::fread(file, stringsAsFactors = TRUE)
   if (archive) {
     file.copy(from = file, to = path.archive)
   }
   if (all(colnames(tbl) == colnames(tbl.final))) {
     tbl.final <- rbindlist(list(tbl,tbl.final))
     file.remove(from = file)
   } else {
       ## move to 
     file.copy(from = file, to  = path.turds)
     file.remove(from = file)
     }
   }
  if (length(list.files(path.turds))) {
    file.remove(from = path.turds)
  }
}
