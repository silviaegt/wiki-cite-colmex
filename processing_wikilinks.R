install.packages("curl")


setwd("C:/Users/silvi/Dropbox/eswiki/code/raw_data/")
devtools::install_github("jayjacobs/tldextract")
# library("tldextract")
library(plyr)
R.version
colmexlinks <- read.table(file = 'colmexlinks_raw.csv', header = TRUE, sep=",", fill=TRUE)
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