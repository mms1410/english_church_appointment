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