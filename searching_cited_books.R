getwd()
#install.packages("XML")
require(XML)
library("methods")


# set working directory
# here you add were your data is (remember to change \ from Windows paths)
#setwd("here/goes/your/path")
setwd("/Users/segutierrez/Dropbox/eswiki/code/raw_data")

#1. Read your tsv with all citations with identifiers extracted from the most recent version of your language Wikipedia 
#Available here: https://figshare.com/articles/Wikipedia_Scholarly_Article_Citations/1299540/10
# (in this example we look at Spanish Wiki)
eswiki <- read.table(file = 'eswiki.tsv', sep = '\t', header = TRUE, fill=TRUE, encoding = "UTF-8")

#2. To get a glimpse of the first rows and thus of how data is ordered:

#head(eswiki)

#3. To get a summary of your data
#sumeswiki <- summary(eswiki)
#here we get, for instance, that the pages with the biggest number of references are: 
#Evolución bilógica(461), Infiriendo transferencia genética horizontal (252), and Historia social de los virus (229)

############Let's start digging into our data#################


#1. Retrieve all types of ids
types <- sort(table(eswiki$type),decreasing=T)
View(types)
barplot(types)
#Compare with: https://medium.com/freely-sharing-the-sum-of-all-knowledge/what-are-the-ten-most-cited-sources-on-wikipedia-lets-ask-the-data-34071478785a
#note how isbns are the more dominant! 


#2. Filter rows that have "isbn"; thus all book-citations
libros <- eswiki[ which(eswiki$type=='isbn'), ]



#2.1 Convert "libros" into a dataframe
library("tibble")
libros_df <- as_data_frame(libros)
#View(libros)
#summary(libros)

#2.2 Create a subset with all the books that cite the "Historia Mínima de México" el El Colegio de México
library("dplyr")
historia_minima = c(9681211391,9789681211394,9788415832010, 9789681211387, 9789680101672)
hm_citations <- libros_df[which(libros_df$id %in% historia_minima),]

View(hm_citations)
#Notes: only one out of the 5 different isbns of the same book has been used: "9681211391" (2004 edition)



#3. Explore all books from the University Press
libroscolmex <- read.csv(file = 'isbncolmex.csv', sep = ',', header = TRUE, fill=TRUE, encoding = "UTF-8")
isbncolmex <- libroscolmex$ISBN
colmex_citations <- libros_df[which(libros_df$id %in% isbncolmex),]

colmexcite_df <- as_data_frame(colmex_citations)

wikipages_list <- as.character(colmexcite_df$page_title)
wikipages <- table(wikipages_list)

citedisbn_list <- as.character(colmexcite_df$id)
citedisbn <- table(citedisbn_list)
View(citedisbn)

# Get a filtered version of the books that do have citations on Wikipedia
cited_books <- libroscolmex[which(libroscolmex$ISBN %in% citedisbn_list),]


citedcenter <- table(cited_books$DEPARTAMENTO)
citedyear <- table(cited_books$A.d1.O.EDICI.d3.N)

View(citedyear)

#3.1 Export different results into a csv
#write.csv(libros, file = "eslibros.csv", row.names=FALSE)

#write.csv(hm_citations, file = "hm_citations.csv", row.names=FALSE)

#write.csv(colmexcite_df, file = "colmexcitations.csv", row.names=FALSE)
write.csv(cited_books, file = "cited_books.csv", row.names=FALSE)

pagwiki_citations <- table(colmex_citations$page_title)


pagwiki_citations
#4. Retrieve the most frequent isbns and get titles from OCLC (work in prgress)
freqbooks <- sort(table(libros$id),decreasing=T)
View(freqbooks)
twenty <- data.frame(freqbooks[1:20])
class(data.frame(twenty))
twenty[1,1]
dim(twenty)
length(twenty$Var1)
for (i in 1:3){
  isbn = twenty[i,1]
  url = sprintf("http://classify.oclc.org/classify2/Classify?isbn=%s&summary=true", isbn)
  data <- xmlTreeParse(url)
  xml_data <- xmlToList(data)
  works = xml_data[[8]]
  print(works[[1]])
  
}


library(rvest)
work <- read_html("http://classify.oclc.org/classify2/Classify?isbn=3822847445&summary=true")




#data <- xmlParse()
# load packages
library("XML")

data <- xmlTreeParse(url)

# convertimos a lista
xml_data <- xmlToList(data)

# hay 8 elementos en la lista
length(xml_data)


# el ... mmm 8 ? que corresponde a works?
works = xml_data[[8]]

# y de ahí quieres: author, holding, hyr y title

# author
works[[1]]

# holding
works[[4]]

#hyr
works[[5]]

#title
works[[10]]

#http://classify.oclc.org/classify2/Classify?isbn=0940228475&summary=truev

barplot(freqbooks)






