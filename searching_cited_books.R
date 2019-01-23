getwd()
#install.packages("XML")
require(XML)
library("methods")
library("dplyr")
library("tidyr")


# set working directory
# here you add were your data is (remember to change \ from Windows paths)
setwd("/home/luba/Dropbox/eswiki/code/raw_data")
#setwd("/Users/segutierrez/Dropbox/eswiki/code/raw_data")
#getwd()

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

#2.2 Create a subset with all the books that cite one book you know about as a test
#Here you can see my test with "Historia Mínima de México" el El Colegio de México
historia_minima = c(9681211391,9789681211394,9788415832010, 9789681211387, 9789680101672)
hm_citations <- libros_df[which(libros_df$id %in% historia_minima),]
View(hm_citations)
#Notes: only one out of the 5 different isbns of the same book has been used: "9681211391" (2004 edition)


#######################################################
####3. Extract all ISBNs from your University Press####
#######################################################
# A little pre-processing was necessary for my data (did it with openrefine)
#registroscolmex <- separate(registroscolmexraw, ISBN, into = c("ISBN", "Notas"), sep = "\\s")


library(readr)
registroscolmex <- read_csv("delColmex_limpio.csv", 
                            col_types = cols(ISBN = col_character()))
View(registroscolmex)

## This is to subset where my real information is
registroscolmexisbn <- subset(registroscolmex, (!is.na(registroscolmex$ISBN)))
colmexissn <- subset(registroscolmex, (!is.na(registroscolmex$ISSN)))

#This is a list of registers without ISBN, ISSN
registroscolmexsinisbn <- subset(registroscolmex, (is.na(registroscolmex$ISBN)) & (is.na(registroscolmex$ISSN)))
titulos_sin_isbn <- sort(table(registroscolmexsinisbn$title), decreasing = T)
write.csv(registroscolmexsinisbn, file = "registroscolmex_sin-isbn-issn.csv", row.names=FALSE)

length(registroscolmexisbn$ISBN)
isbncolmexfreq <- as.data.frame(sort(table(registroscolmex$ISBN), decreasing = T))
colnames(isbncolmexfreq) <- c("ISBN", "Frecuencia")
write.csv(isbncolmexfreq, file = "isbncolmexfreq.csv", row.names=FALSE)

############################################################
###4. Compare your ISBNs with the ones cited in Wikipedia###
###########################################################
#Now that I have my list of unique ISBNs
isbncolmex <- isbncolmexfreq$ISBN

#I will see if they are in the df of cited books in Wikipedia
colmexcite_df <-  as_data_frame(libros_df[which(libros_df$id %in% isbncolmex),])
View(colmexcite_df)
#Number of unique different websites that cite our books
length(unique(colmexcite_df$page_title)) #63
#Number of unique different books cited in Wikipedia
length(unique(colmexcite_df$id)) #38


wikipages_list <- as.character(colmexcite_df$page_title)
wikipagesfreq <- sort(table(wikipages_list), decreasing=T)
View(wikipagesfreq)

citedisbn_list <- as.character(colmexcite_df$id)
citedisbnfreq <- as.data.frame(sort(table(citedisbn_list), decreasing=T))

registroscolmex_sin_repetir <- registroscolmexisbn[!duplicated(registroscolmexisbn$ISBN),]

citedisbn_joined <- inner_join(citedisbnfreq , registroscolmex_sin_repetir, by=c("citedisbn_list"="ISBN"))
citedisbn_joined[is.na(citedisbn_joined)] = 0
length(citedisbn_joined$freq)
citedisbn <- data.frame(citedisbn_joined$citedisbn_list, citedisbn_joined$title, citedisbn_joined$Autor, citedisbn_joined$Freq)
names(citedisbn) <- c("ISBN", "Título", "Autor", "No. de Citas")
sort(table(citedisbn$Autor), decreasing = T)

write.csv(citedisbn, file = "citedisbn.csv", row.names=FALSE)



#############################################################################


# Get a filtered version of the books that do have citations on Wikipedia
cited_books <- registroscolmex[which(registroscolmex$ISBN %in% citedisbn_list),]


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






