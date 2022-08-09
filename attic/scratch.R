# county_	diocesejurisdiction__	diocesegeographic__	parish	cced_id	names	personid	year	type	office	full	inlocation_
## summary -> in Location / Contains
# evidence record:
# first thread header cells
# a row in an html table defined by <tr>
# header cells <th>
# data cell <td>
# group header content <thead>
# body content of html table <tbody>  
################################################################################
pattern.table <- "(<table[^>]*>(?:.|\n)*?<\\/table>)"
gregexpr(pattern = pattern.table, text = page)
html.tbls <- regmatches(page, gregexpr(pattern = pattern, text = page))[[1]]
html.tbl1 <- html.tbls[1]
html.tbl2 <- html.tbls[2]

pattern.threads <- "(?<=<th>)(?:.|\n)*?(?=<\\/th>)"  # get column names (header cells)
tbl.colnames <- regmatches(html.tbl2,gregexpr(pattern = pattern.threads, text = html.tbl2, perl = TRUE))[[1]]
tbl.ncol <- length(tbl.colnames)

tbl.list <- as.list(tbl.colnames)
names(tbl.list) <- tbl.colnames

pattern.data <- "(?<=<td>)(?:.|\n)*?(?=<\\/td>)"  #  get data cells
gregexpr(pattern = pattern.data, text = html.tbl2, perl = TRUE)
data.vec <- regmatches(html.tbl2, gregexpr(pattern = pattern.data, text = html.tbl2, perl = TRUE))[[1]]

tbl.data <- as.data.table(matrix(data.vec, nrow = length(data.vec), ncol = tbl.ncol, byrow = TRUE))
colnames(tbl.data) <- tbl.colnames

(rstudioapi::getSourceEditorContext()$path)

nameasrecorded <- tbl2$NameasRecorded

names <- unlist(regmatches(nameasrecorded, gregexpr(pattern = "(?<=>).*?(?=<)", text = nameasrecorded, perl = TRUE)))
ids <- unlist(regmatches(nameasrecorded,gregexpr(pattern = "(?<=PersonID=)[[:digit:]]*", text = nameasrecorded, perl = TRUE)))
################################################################################

## in the following remove white space and ignore upper and lower case
table.names <- gsub(pattern = "[[:space:]]", replacement = "", x = table.names, perl = TRUE)
## get html content of webpage
page <- RCurl::getURLContent(url.scrape)
page <- gsub(pattern = "[[:space:]]", replacement = "", x = page, perl = TRUE)
##
location <- regmatches(page, gregexpr(pattern = "(?<=Location:).*?(?=</h1>)", text = page, perl = TRUE))[[1]]
location.name <- regmatches(location, gregexpr(pattern = "(?<=:).*$", text = location, perl = TRUE))[[1]]
location.type <- regmatches(location, gregexpr(pattern = "^.*?(?=:)", text = location, perl = TRUE))[[1]]
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
################################################################################
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

dots <- list(archive.path = paste0(path.data, .Platform$file.sep, "archive"))


list.files(path.data, full.names = TRUE)
file.pattern

for (file in list.files(path.data, full.names = TRUE, pattern = file.pattern)) {
  cat(paste0(paste0(rep("=", 78), collapse = ""), "\n"))
  cat(paste0(file, "\n"))
  cat(paste0(paste0(rep("=", 78), collapse = ""), "\n"))
}
length(list.files(path.turds))

       