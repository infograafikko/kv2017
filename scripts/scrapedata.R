library(rvest)
library(xml2)
library(readr)

setwd("~/Documents/D3js/Kuntavaalit2017/data")
kuntanumerot <- read.csv("kuntanumerot.csv", header = FALSE, sep="\t", colClasses = c(rep("character", 1)))

kokomaa <- NULL

#The scraping part
for(i in 1:nrow(kuntanumerot)) {
  
kunta <- kuntanumerot[i,1]

URLtext <- paste0("http://tulospalvelu.vaalit.fi/KV-2017/fi/",kunta)
URL <- read_html(URLtext)

firstCol <- URL %>% html_nodes(xpath='//*[@id="litistyvaTauluPuo"]/table') %>% html_nodes(xpath = '//td[1]/text()') %>% iconv("UTF-8", "UTF-8")
firstCol <- firstCol[2:length(firstCol)]

secondCol <- URL %>% html_nodes(xpath='//*[@id="litistyvaTauluPuo"]/table') %>% html_nodes(xpath = '//td[2]/div[2]/text()') %>% iconv("UTF-8", "UTF-8")
secondCol <- as.numeric(gsub(",",".",secondCol))

alue <- URL %>% html_nodes(xpath='//*[@id="alku"]') %>% html_text

kunta <- as.data.frame(cbind(firstCol,secondCol, alue))

kokomaa <- rbind(kokomaa, kunta)

}

#Tidy data
kokomaaEdit <- kokomaa
colnames(kokomaaEdit) <- c("puolue", "aanipct", "alue")

kokomaaEdit <- kokomaaEdit[ ! kokomaaEdit$puolue %in% c("Yhteensä", "Mitättömät äänet"), ]

#Change column order
kokomaaEdit <- kokomaaEdit[, c(3,1,2)]
kokomaaEdit <- spread(kokomaaEdit, puolue, aanipct)

#Change to numeric
cols <- colnames(kokomaaEdit)
for (i in 2:length(cols)) {
  selectCol <- cols[i]
  kokomaaEdit[selectCol] <- as.numeric(levels(kokomaaEdit[[selectCol]]))[kokomaaEdit[[selectCol]]]
}

#Change NA to 0
kokomaaEdit[is.na(kokomaaEdit)] <- 0

kokomaaEdit <- kokomaaEdit[, c(1,3:9,19,2,10:18,20:137)]

otherList <- NULL

#Make other column for data
for (i in 1:nrow(kokomaaEdit)) {
  
  otherNumber <- sum(kokomaaEdit[i,10:137])
  otherList <- rbind(otherList, otherNumber)
}

#Make final datafram
kv <- kokomaaEdit[, c(1:9)]
kv$muut <- otherList
colnames(kv) <- c("alue", "kok", "ps", "kesk", "kd", "sdp", "vas", "vihr", "rkp", "muut")

#Tee lista kaikista kunnista
kuntalista <- kv$alue

#Change to numeric
cols <- colnames(kv)
for (i in 2:length(cols)) {
  selectCol <- cols[i]
  kv[selectCol] <- as.numeric(levels(kv[[selectCol]]))[kv[[selectCol]]]
}

  
  #Laske äänestysero muihin kuntiin
  for (j in 1:nrow(kv)) {
    kv[kuntalista[i]][j,] <- abs(kv$kok[j] - kv$kok[i]) + abs(kv$sdp[j] - kv$sdp[i]) + abs(kv$kesk[j] - kv$kesk[i]) + abs(kv$ps[j] - kv$ps[i]) + abs(kv$vihr[j] - kv$vihr[i]) + abs(kv$vas[j] - kv$vas[i]) + abs(kv$rkp[j] - kv$rkp[i]) + abs(kv$kd[j] - kv$kd[i]) + abs(kv$muut[j] - kv$muut[i])
  }
  
}

#Tee lista "alue, puolue, äänimäärä"
puoluepct <- subset(kv, select=c(1:10))
puoluepct <- puoluepct %>% gather("puolue","value",2:10)
colnames(puoluepct) <- c("alue", "puolue", "value")

#Tee kuntalistasta dataframe
kuntalista <- as.data.frame(kuntalista)
colnames(kuntalista) <- "x"

#Write data to tsv
library(readr)

write_tsv(kv, "kv2012.tsv")
write.csv(kuntalista, "kuntalista2012.csv", fileEncoding="UTF-8", row.names=FALSE)
write_tsv(puoluepct, "puoluepct2012.tsv")

#Make html code for select list
sapply(kuntalista, function(x) paste0("<option value='", x,"' >", x, "</option>" ))

