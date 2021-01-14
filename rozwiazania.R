#java -jar selenium-server-standalone-3.0.1.jar #uruchomic w Terminalu

#install.packages(c("RSelenium","seleniumPipes","dplyr"))

#Potrzebne biblioteki
library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(gtools)
library(stringr)

#Polaczenie z naszym serwerem
remDr <- remoteDr(remoteServerAddr = "http://localhost", port = 4444, browserName = "chrome", newSession = TRUE)
remDr %>% go("http://otomoto.pl/osobowe")

wektorLinkow <- c()
for(i in 1:50){
  newUrl <- paste0("https://otomoto.pl/osobowe/?page=",i)
  remDr %>% go(newUrl)
  elems <- remDr %>% findElements(using = "tag name", "h2")
  
  for (j in 1:length(elems)){
    e <- findElementsFromElement(elems[[j]],using = "tag name", "a")
    if(length(e)>0){
      link <- e[[1]] %>% getElementAttribute("href")
      wektorLinkow <- c(wektorLinkow,link)
    }
  }
}
wektorLinkowU <- wektorLinkow %>% unique()
liczbaLinkow <- length(wektorLinkowU)


#Cena i szczegoly ogloszen zebrane w Data Frame

zrobWiersz <- function(w, wektorLinkowU, remDr){
  remDr %>% go(wektorLinkowU[w])
  el <- remDr %>% findElement(using = "class name", "price-wrapper")
  el <- el %>% findElementFromElement("class name","offer-price__number")
  cena <- NA
  cena <- el %>% getElementText()
  cena <- cena %>% str_replace_all("[^\\d,]","") %>% str_replace_all(",",".") %>% as.numeric()
  
  szczegoly <- remDr %>% findElements("class name","offer-params__list")
  opisy1 <- (szczegoly[[1]] %>% findElementsFromElement("class name","offer-params__label"))
  opisy2 <- (szczegoly[[2]] %>% findElementsFromElement("class name","offer-params__label"))
  wartosci1 <- (szczegoly[[1]] %>% findElementsFromElement("class name","offer-params__value"))
  wartosci2 <- (szczegoly[[2]] %>% findElementsFromElement("class name","offer-params__value"))
  listaSzczegolyOpis <- c(opisy1,opisy2)
  listaSzczegolyWartosci <- c(wartosci1,wartosci2)
  
  nazwyKolumn <- unlist(lapply(listaSzczegolyOpis, getElementText))
  wartosci <- unlist(lapply(listaSzczegolyWartosci, getElementText))
  
  df1 <- data.frame (matrix(wartosci, nrow=1, ncol=length(wartosci)) )
  names(df1) <- nazwyKolumn
  df1 <- cbind(cena,df1)
}
#zrobWiersz(1, wektorLinkowU, remDr)

auta <- NULL

for(item in 1:liczbaLinkow){
  skip <- FALSE
  
  tryCatch(
    df1 <- zrobWiersz(item, wektorLinkowU, remDr), error=function(e){skip<<-TRUE}
  )
  
  if(skip){next}
  if(is.null(auta)){
    auta <- df1
  }
  else{
    auta <- smartbind(auta,df1)
  }
  View(auta)
}

auta[["Przebieg"]] <- auta[["Przebieg"]] %>% str_replace_all("[^\\d,]","") %>% str_replace_all(",",".") %>% as.numeric()
auta[["Pojemność skokowa"]] <- auta[["Pojemność skokowa"]] %>% str_replace_all("cm3","") %>% str_replace_all("[^\\d,]","") %>% str_replace_all(",",".") %>% as.numeric()
auta[["Moc"]] <- auta[["Moc"]] %>% str_replace_all("KM","") %>% str_replace_all("[^\\d,]","") %>% str_replace_all(",",".") %>% as.numeric()
auta[["Emisja CO2"]] <- auta[["Emisja CO2"]] %>% str_replace_all("g/km","") %>% str_replace_all("[^\\d,]","") %>% str_replace_all(",",".") %>% as.numeric()
View(auta)