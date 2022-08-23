# code description

## R/functions.R

The script 'functions.R' contains the functions used for webscraping the necessary data.
`scrape.html` takes a string representing a url as input and returns a `data.table` object consisting of scraped data.
First the webpages html content is parsed using R's curl interface written in the package `Rcurl`. Since `scrape.html` uses regular expressions, white spaces might at some occasions be unwanted, therefore the variable `page.trim` contains the parsed html page with removed whitespaces. Note that here the flag perl must be set to TRUE since the regex pattern contains perl syntax that would otherwise not be interpreted appropriately.
The webpages requested in this project typically  contains the following pattern:
There are three major parts, 'Evidence', 'Summary' and 'History'. The data in the 'Evidence' and 'History' section are embedded in an html tabel while the 'Summary' data is simply written as list items.
Not all webpages contain the necessary data, therefore some checks are done to assure certain list items do exists using the fact that list items in html are encoded with the '<li>' tag. Inspection the html code one can see that 'Evidence', 'Summary' and 'History' are actually represented as third level headers on the webpages encoded in the '<h3>' tag.
Data contained in the 'Summary' section have to be scraped individually and are refrenced in the 'tbl.smry' variable.
The regular expressions used to  get the necessary data contains two patterns worthwile to mention: First when I want t find data contained in html tags I want to perform a non greedy search since otherwise closing html tags of the last found "html-tag-section" will be used. The syntax for this is a '?' operator after any quantifier, here the '*' operator which qunatifies to none or arbitrary many times. The '.' character encodes all alphanumerical symbols including whitespaces and special symbols like exclamation marks.
The second important syntax used here are 'lookahead' and 'lookbehind' symbols ecoded in "(?='some_text') and "(?<='some_text')".
Lookaheads (Lookbehinds) assert certain letters after (before) the actual text of interest and are excluded from the match.
Note that Lookaheads and Lookbehinds are Perl syntax, so the appropriate flag must be set to true, further note that the special characters '^' and '$' encode the beginning or end of a text
