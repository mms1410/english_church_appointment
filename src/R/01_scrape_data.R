################################################################################
# this script was written on debian bullseye with TLS protocol version 1.2.
# the webpage 'https://theclergydatabase.org.uk' does not support TLS v. 1.2 or higher.
# If open-ssl is used for protocol encrytion the SECLEVEL parameter might (temporary) be set 
# from 2 to 1. On debian (and most likely on other UNIX architectures) this might be found in the corresponding
# config file, see: /etc/ssl/openssl.cnf [from command line or 'system' command in R: cat /etc/ssl/openssl.cnf | grep DEFAULT@SECLEVEL"]
################################################################################
# clear environment
rm(list = ls())
################################################################################
library(RCurl)
library(data.table)
library(checkmate)

table.names <- c("CCE Region", "NAME AS RECORD")
url.base <- "https://theclergydatabase.org.uk/jsp/locations/DisplayLocation.jsp?locKey="
pages <- seq(2, 250000)
pages <- paste0(url.base, pages)
url.scrape <- pages[1]


path.data <- rstudioapi::getSourceEditorContext()$path
path.data <- dirname(path.data) ## R
path.data <- dirname(path.data) ## src
path.data <- dirname(path.data) ## 'home'
path.data <- paste0(path.data, .Platform$file.sep, "data")

if (!file.exists(path.data)) {
  dir.create(path.data)
}

pattern.data = "(?<=<td>)(?:.|\n)*?(?=<\\/td>)"
pattern.threads = "(?<=<th>)(?:.|\n)*?(?=<\\/th>)"
pattern.table= "(<table[^>]*>(?:.|\n)*?<\\/table>)"


html.to.tbl <- function(html.tbl) {
  #
  html.tbl.colnames <- regmatches(html.tbl,gregexpr(pattern = pattern.threads, text = html.tbl, perl = TRUE))[[1]]
  html.tbl.ncol <- length(html.tbl.colnames)
  ##
  html.tbl.list <- as.list(html.tbl.colnames)
  names(html.tbl.list) <- html.tbl.colnames
  ##
  html.tbl.vec <- regmatches(html.tbl, gregexpr(pattern = pattern.data, text = html.tbl, perl = TRUE))[[1]]
  html.tbl.nrow <- length(html.tbl.vec) / html.tbl.ncol
  html.tbl <- as.data.table(matrix(html.tbl.vec, nrow = html.tbl.nrow, ncol = html.tbl.ncol, byrow = TRUE))
  colnames(html.tbl) <- html.tbl.colnames
  ##
  html.tbl
}

## in the following remove white space and ignore upper and lower case
table.names <- gsub(pattern = "[[:space:]]", replacement = "", x = table.names, perl = TRUE)
## get html content of webpage
page <- RCurl::getURLContent(url.scrape)
page <- gsub(pattern = "[[:space:]]", replacement = "", x = page, perl = TRUE)
##
location <- regmatches(page, gregexpr(pattern = "(?<=Location:).*?(?=</h1>)", text = page, perl = TRUE))[[1]]
location.name <- regmatches(location, gregexpr(pattern = "(?<=:).*$", text = location, perl = TRUE))[[1]]
location.type <- regmatches(location, gregexpr(pattern = "^.*?(?=:)", text = location, perl = TRUE))[[1]]
location.type

## get summary
pattern.summary <- "<h3>Summary</h3>.*?</ul>"
smry.list <- regmatches(page, gregexpr(pattern = pattern.summary, text = page))[[1]]
pattern.items <- "(?<=<li><label>).*?(?=</li>)"
smry.list <- regmatches(smry.list, gregexpr(pattern = pattern.items, text = smry.list, perl = TRUE))[[1]]
smry.list <- strsplit(smry.list, split = "</label>")
smry.list <- lapply(smry.list, function(x) gsub(pattern = "&nbsp|;|:", replacement = "", x))
##
smry.names <- sapply(smry.list, "[[", 1)
smry.data <- sapply(smry.list, "[[", 2)
##
tbl.smry <- as.data.table(matrix(smry.data, nrow = 1))
colnames(tbl.smry) <- smry.names
## get html tables
html.tbls <- regmatches(page, gregexpr(pattern = pattern.table, text = page))[[1]]
html.tbl.hist <- html.tbls[1]
html.tbl.evid <- html.tbls[2]
## 
tbl.hist <- html.to.tbl(html.tbl.hist)
tbl.evid <- html.to.tbl(html.tbl.evid)
##
tbl.evid$NameasRecorded  <- gsub(pattern = "<.*?>", "", tbl.evid$NameasRecorded)
tbl.evid$Type <- gsub(pattern = "<.*?>", replacement = "", x = tbl.evid$Type)
## person and cce id
person.id <- regmatches(html.tbl.evid, gregexpr(pattern = "(?<=PersonID=)[[:digit:]]*|notgiven,", text  = html.tbl.evid, perl = TRUE))[[1]]  
tbl.evid[, PersonID := list(person.id)]
cced.id <- regmatches(page, gregexpr(pattern = "(?<=<dfn>).*?(?=</dfn>)", perl = TRUE, text = page))[[1]]
tbl.evid[, CCEdID := list(cced.id)]

tbl.write <- as.data.table(cbind(County = tbl.smry$County,
                    Diocese_Juristiction = tbl.smry$`Diocese(Jurisdiction)`,
                    Diocese_Geographic = tbl.smry$`Diocese(Geographic)`,
                    Location_Type = location.type,
                    Location_Name = location.name,
                    CCEd_ID = tbl.evid$CCEdID,
                    Name = tbl.evid$NameasRecorded,
                    Person_ID = tbl.evid$PersonID,
                    Year = tbl.evid$Year,
                    Type = tbl.evid$Type,
                    Office = tbl.evid$Office))
fwrite(tbl.write, file = paste0(path.data, .Platform$file.sep, "cced_id_", cced.id))
# county_	diocesejurisdiction__	diocesegeographic__	parish	cced_id	names	personid	year	type	office	full	inlocation_
## summary -> in Location / Contains
# evidence record:
# first thread header cells
# a row in an html table defined by <tr>
# header cells <th>
# data cell <td>
# group header content <thead>
# body content of html table <tbody>  