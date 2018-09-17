devtools::install_github("jayjacobs/tldextract")
library("tldextract")
library(plyr)
library("jsonlite")
library("rjson")
getwd()

#url = "https://es.wikipedia.org/w/api.php?action=query&list=exturlusage&euquery=*.colmex.mx/&eulimit=500&format=json"


result <- fromJSON(file = "eswikilinks.json")
print(result)
colmexlinks <- result$query$exturlusage
colmexlinksdf <- as.data.frame(result)
print(colmexlinksdf)

colmexlinks <- read.table(file = 'colmex_links.csv', header = TRUE, sep=",", fill=TRUE, encoding = "UTF-8")
View(colmexlinks)

host <- parse_url(colmexlinks$copia_links[1])$hostname
domain.info <- tldextract(host)
View(domain.info)

colmexurls <- list()

for (i in 1:length(colmexlinks$links)){
  host <- parse_url(colmexlinks$copia_links[i])$hostname
  colmexurls[[i]] <- host
}


freqlinks <- table(unlist(colmexurls))


write.csv(freqlinks, file = "freqlinkscolmex.csv", row.names=FALSE)



colmexlinks$url_base <- unlist(colmexurls)
clinks$links <- NULL
clinks <- rename(colmexlinks, c("copia_links"="link_completo", "Text.2.2"="articulo_wikipedia"))
clinks <- clinks[,c(3,2,1)]
View(clinks)
clinksdf <- data.frame(clinks)
View(clinksdf)
write.csv(clinksdf, file = "colmexlinks.csv", row.names=FALSE)

freqarticles <- sort(table(clinksdf$articulo_wikipedia),decreasing=T)
View(freqarticles)
domain.info <- tldextract(parse_url(colmexlinks$copia_links[1]))