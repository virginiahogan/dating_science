# load packages
library(RCurl)
library(XML)
require(stringr)
require(devtools)

#in.auth <- inOAuth()
#devtools::install_github("rstudio/leaflet")
library(dplyr)
library(rvest)
library(ggmap)
library(leaflet)
library(RColorBrewer)


googleURLs <- function(search) {
  # read in page contents
#  search="bart simpson san francisco"
  search_str = str_replace_all(search, " ", "+")
  u <- paste0("http://www.google.com/search?aq=f&gcx=w&sourceid=chrome&ie=UTF-8&q=", search_str)

  html <- getURL(u)
  class(html)

  # parse HTML into tree structure
  doc <- htmlParse(html)

  # extract url nodes using XPath. Originally I had used "//a[@href][@class='l']" until the google code change.
  attrs <- xpathApply(doc, "//h3//a[@href]", xmlAttrs)

  # extract urls
  links <- sapply(attrs, function(x) x[[1]])

  # free doc from memory
  free(doc)

  # ensure urls start with "http" to avoid google references to the search page
  links <- grep("http", links, fixed = TRUE, value=TRUE)
  links = sapply(links, function(l) str_replace_all(l, "/url\\?q=", ""))
  links = as.character(links)

  links=sapply(links, function(l) strsplit(l[1], split="&sa")[[1]][1])
  links=sapply(links, function(l) strsplit(l[1], split="%3")[[1]][1])

  links = as.character(links)

  return(links)
}

urls=googleURLs("mike san francisco")

getURLTexts = function(search) {
  str_vec=c()
  names_str = c()
  urls=googleURLs(search)
  for(u in urls) {
    print(u)
    html_page=NULL
    html_page=try(read_html(paste0(u, ".html")))

    if(is.null(html_page)) {
      print(".html didn't work")
      html_page=try(read_html(u))

    }


    if(!is.null(html_page)) {
      doc=htmlTreeParse(html_page, useInternal=TRUE)
      doc = unlist(xpathApply(doc, '//p', xmlValue))

      # Replace all \n by spaces
      doc = gsub('\\n', ' ', doc)

      # Join all the elements of the character vector into a single
      # character string, separated by spaces
      doc = paste(doc, collapse = ' ')
      str_vec = c(str_vec, doc)
      current_name = u
      if(str_detect(u, "twitter")) {
        current_name="twitter"
      }
      if(str_detect(u, "linkedin")) {
        current_name="linkedin"
      }
      if(str_detect(u, "github")) {
        current_name="github"
      }
      if(str_detect(u, "facebook")) {
        current_name="facebook"
      }
      if(str_detect(u, "quora")) {
        current_name="quora"
      }
      if(str_detect(u, "google")) {
        current_name="google"
      }
      names_str=c(names_str, current_name)

    }

  }
  names(str_vec)=names_str
  return(str_vec)
}
search="bart simpson san francisco"
text = getURLTexts(search)



head(results)

url= urls[1]
doc.html = htmlTreeParse(paste0(url, ".html"), useInternal = TRUE)

# Extract all the paragraphs (HTML tag is p, starting at
# the root of the document). Unlist flattens the list to
# create a character vector.


## how many times they tweet about taylor swift/their mom
## where their github page ranks



first[[1]]
first[[2]]
