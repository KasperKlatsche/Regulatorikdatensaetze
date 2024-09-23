library(epubr)
library(readxl)

pfadStart <- "C:\\Users\\Julius\\Documents\\Datenanalyse\\LLMfuerCompliance\\Testdaten Christian Schreiber"
pfadZiel <- "./"

pfadKWG <- paste(pfadStart, "KreditwesenGesetzKWG.epub", sep="\\")
zielKWG <- paste(pfadZiel, "KWGalsCSV.csv", sep="\\")

pfadWPHG <- paste(pfadStart, "WertpapierhandelsGesetzWpHG.epub", sep="\\")
zielWPHG <- paste(pfadZiel,"WPHGalsCSV.csv", sep="\\")

pfadKAGB <- paste(pfadStart, "KapitalhandelsGesetzbuchKAGB.epub", sep="\\")
zielKAGB <- paste(pfadZiel, "KAGBalsCSV.csv", sep="\\")

pfadHGB <- paste(pfadStart, "HandelsgesetzbuchHGB.epub", sep="\\")
zielHGB <- paste(pfadZiel, "HGBalsCSV.csv", sep="\\")

pfadMaRisk <- paste(pfadStart, "20240618_MaRisk_AlsExcel_mitQV_v0.1.xlsx", sep="\\")
zielMaRisk <- paste(pfadZiel, "MaRISKalsCSV.csv", sep="\\")

pfadDORA <- paste(pfadStart, "20240912_DORA_AlsExcel_v0.1.xlsx", sep="\\")
zielDORA <- paste(pfadZiel, "DORAalsCSV.csv", sep="\\")

zielToKWG <- paste(pfadZiel, "MaRISKtoKwgCSV.csv", sep="\\")
zielToKAGB <- paste(pfadZiel, "MaRISKtoKagbCSV.csv", sep="\\")
zielToWPHG <- paste(pfadZiel, "MaRISKtoWphgCSV.csv", sep="\\")
zielToHGB <- paste(pfadZiel, "MaRISKtoHgbCSV.csv", sep="\\")

#-------------------zuerst das KWG in CSV umwandeln--------------------------------------------------------

epub <- epub(pfadKWG)

textbezeichnungKWG <- "KWG"

kwg <- data.frame(Index = integer(0), Wortlaut = character(0), stringsAsFactors = FALSE)

#1-5 des KWG sind vorgeplaenkel. 254 des KWG ist Anhang. Alles dazwischen ist Gesetzestext
for(i in 6:253){
  #absatzMuster <- "\\(\\d+\\)"
  absatzMuster <- "\\(\\d+[a-z]?\\)"
  
  paragraph <- epub$data[[1]]$text[i] #der ganze Paragraph
  paragraph <- strsplit(paragraph, "Fußnote")[[1]][1]#Fussnote entfernen
  absaetze <- strsplit(paragraph, absatzMuster)[[1]] #auftrennen in Absaetze
  
  #den Absatz Index extrahieren
  absatzMatches <- gregexpr(absatzMuster, paragraph)
  absatzIndex <- regmatches(paragraph, absatzMatches)[[1]]
  if(length(absatzIndex) < length(absaetze)){
    absatzIndex <- c("",absatzIndex)
  }
  
  paragraphMuster <- "^§\\s\\d+[a-z]?"
  paragraphMatches <- gregexpr(paragraphMuster, paragraph)
  paragraphIndex <- regmatches(paragraph, paragraphMatches)[[1]][1]
  
  index <- paste(textbezeichnungKWG, paragraphIndex, absatzIndex)
  
  tabelle <- cbind(index, absaetze)
  kwg <- rbind(kwg, tabelle)
}
names(kwg) <- c("kwgIndex", "kwgWortlaut")
kwg <- kwg[kwg$kwgIndex!="KWG NA ",]

write.csv(kwg, zielKWG)

#-------------------dann das WpHG in CSV umwandeln--------------------------------------------------------

epub <- epub(pfadWPHG)

textbezeichnungWPHG <- "WpHG"

wphg <- data.frame(Index = integer(0), Wortlaut = character(0), stringsAsFactors = FALSE)

#wie bei KWG. vorher nachgesehen mit head(epub$data) und epub$data[[1]]$text[length(epub$data[[1]]$text)]
for(i in 5:184){
  #absatzMuster <- "\\(\\d+\\)"
  absatzMuster <- "\\(\\d+[a-z]?\\)"
  
  paragraph <- epub$data[[1]]$text[i] #der ganze Paragraph
  paragraph <- strsplit(paragraph, "Fußnote")[[1]][1]#Fussnote entfernen
  absaetze <- strsplit(paragraph, absatzMuster)[[1]] #auftrennen in Absaetze
  
  #den Absatz Index extrahieren
  absatzMatches <- gregexpr(absatzMuster, paragraph)
  absatzIndex <- regmatches(paragraph, absatzMatches)[[1]]
  if(length(absatzIndex) < length(absaetze)){
    absatzIndex <- c("",absatzIndex)
  }
  
  paragraphMuster <- "^§\\s\\d+[a-z]?"
  paragraphMatches <- gregexpr(paragraphMuster, paragraph)
  paragraphIndex <- regmatches(paragraph, paragraphMatches)[[1]][1]
  
  index <- paste(textbezeichnungWPHG, paragraphIndex, absatzIndex)
  
  tabelle <- cbind(index, absaetze)
  wphg <- rbind(wphg, tabelle)
}
names(wphg) <- c("wphgIndex", "wphgWortlaut")
wphg <- wphg[-c(137),] #da ist ein Paragraph doppelt drin
wphg <- wphg[wphg$wphgIndex!="WpHG NA ",]

write.csv(wphg, zielWPHG)

#-------------------dann das KAGB in CSV umwandeln--------------------------------------------------------

epub <- epub(pfadKAGB)

textbezeichnungKAGB <- "KAGB"

kagb <- data.frame(Index = integer(0), Wortlaut = character(0), stringsAsFactors = FALSE)

#wie bei KWG. vorher nachgesehen mit head(epub$data) und epub$data[[1]]$text[length(epub$data[[1]]$text)]
for(i in 6:481){
  #absatzMuster <- "\\(\\d+\\)"
  absatzMuster <- "\\(\\d+[a-z]?\\)"
  
  paragraph <- epub$data[[1]]$text[i] #der ganze Paragraph
  paragraph <- strsplit(paragraph, "Fußnote")[[1]][1]#Fussnote entfernen
  absaetze <- strsplit(paragraph, absatzMuster)[[1]] #auftrennen in Absaetze
  
  #den Absatz Index extrahieren
  absatzMatches <- gregexpr(absatzMuster, paragraph)
  absatzIndex <- regmatches(paragraph, absatzMatches)[[1]]
  if(length(absatzIndex) < length(absaetze)){
    absatzIndex <- c("",absatzIndex)
  }
  
  paragraphMuster <- "^§\\s\\d+[a-z]?"
  paragraphMatches <- gregexpr(paragraphMuster, paragraph)
  paragraphIndex <- regmatches(paragraph, paragraphMatches)[[1]][1]
  
  index <- paste(textbezeichnungKAGB, paragraphIndex, absatzIndex)
  
  tabelle <- cbind(index, absaetze)
  kagb <- rbind(kagb, tabelle)
}
names(kagb) <- c("kagbIndex", "kagbWortlaut")
kagb <- kagb[kagb$kagbIndex!="KAGB NA ",]

write.csv(kagb, zielKAGB)

#-------------------dann das HGB in CSV umwandeln--------------------------------------------------------

epub <- epub(pfadHGB)

textbezeichnungHGB <- "HGB"

hgb <- data.frame(Index = integer(0), Wortlaut = character(0), stringsAsFactors = FALSE)

#wie bei KWG. vorher nachgesehen mit head(epub$data) und epub$data[[1]]$text[length(epub$data[[1]]$text)]
for(i in 5:814){
  #absatzMuster <- "\\(\\d+\\)"
  absatzMuster <- "\\(\\d+[a-z]?\\)"
  
  paragraph <- epub$data[[1]]$text[i] #der ganze Paragraph
  paragraph <- strsplit(paragraph, "Fußnote")[[1]][1]#Fussnote entfernen
  absaetze <- strsplit(paragraph, absatzMuster)[[1]] #auftrennen in Absaetze
  
  #den Absatz Index extrahieren
  absatzMatches <- gregexpr(absatzMuster, paragraph)
  absatzIndex <- regmatches(paragraph, absatzMatches)[[1]]
  if(length(absatzIndex) < length(absaetze)){
    absatzIndex <- c("",absatzIndex)
  }
  
  paragraphMuster <- "^§\\s\\d+[a-z]?"
  paragraphMatches <- gregexpr(paragraphMuster, paragraph)
  paragraphIndex <- regmatches(paragraph, paragraphMatches)[[1]][1]
  
  index <- paste(textbezeichnungHGB, paragraphIndex, absatzIndex)
  
  tabelle <- cbind(index, absaetze)
  hgb <- rbind(hgb, tabelle)
}
names(hgb) <- c("hgbIndex", "hgbWortlaut")
hgb <- hgb[hgb$hgbIndex!="HGB NA ",]

write.csv(hgb, zielHGB)

#-------------------dann das MaRisk in CSV umwandeln--------------------------------------------------------

marisk <- read_excel(pfadMaRisk)
index <- paste(marisk$Abschnitt, marisk$Nummer, marisk$Absatz)
wortlaut <- gsub("\r\n", " ", marisk$Wortlaut)
referenz <- marisk$Referenzen
marisk <- cbind(index, wortlaut, referenz)
colnames(marisk) <- c("mariskIndex", "mariskWortlaut", "mariskReferenz")

write.csv(marisk, zielMaRisk)

#-------------------dann das DORA in CSV umwandeln--------------------------------------------------------

dora <- read_excel(pfadDORA)
index <- paste(dora$`DORA: Artikel`, ", Abs:", dora$`DORA: Absatz`, sep="")
wortlaut <- gsub("\r\n", " ", dora$`DORA: Anforderung aus Digital Operational Resilience Act (VERORDNUNG (EU) 2022/2554 DES EUROPÄISCHEN PARLAMENTS UND DES RATES vom 14. Dezember 2022) (Volltext)`)
dora <- cbind(index, wortlaut)
colnames(dora) <- c("doraIndex", "dorakWortlaut")

write.csv(dora, zielDORA)

#------------------Relations in files legen----------------------------------------------------------------
#MaRisk
relMARISK <- strsplit(marisk[,3], "; ") #relations aufgetrennt
indexMARISK <- marisk[,1]
toKWG <- data.frame(Marisk = character(0), Kwg = character(0), stringsAsFactors = FALSE)
toWPHG <- data.frame(Marisk = character(0), Wphg = character(0), stringsAsFactors = FALSE)
toKAGB <- data.frame(Marisk = character(0), Kagb = character(0), stringsAsFactors = FALSE)
toHGB <- data.frame(Marisk = character(0), Hgb = character(0), stringsAsFactors = FALSE)

for(i in 1:length(indexMARISK)){
  if(is.na(relMARISK[[i]][1])) next #wenn es keine relation gibt, dann nächster bitte
  for(j in relMARISK[[i]]) {
    #jetzt alle relationen durchgehen
    switch(strsplit(j, " ")[[1]][1],
           KWG={
             alleZiele <- kwg$kwgIndex[grepl(j, kwg$kwgIndex) | kwg$kwgIndex == j]
             if(length(alleZiele) == 0) next
             neueZeilen <- cbind(marisk[i,1], alleZiele)
             toKWG <- rbind(toKWG, neueZeilen)
           },
           WpHG={
             alleZiele <- wphg$wphgIndex[grepl(j, wphg$wphgIndex) | wphg$wphgIndex == j]
             if(length(alleZiele) == 0) next
             neueZeilen <- cbind(marisk[i,1], alleZiele)
             toWPHG <- rbind(toWPHG, neueZeilen)
           },
           KAGB={
             alleZiele <- kagb$kagbIndex[grepl(j, kagb$kagbIndex) | kagb$kagbIndex == j]
             if(length(alleZiele) == 0) next
             neueZeilen <- cbind(marisk[i,1], alleZiele)
             toKAGB <- rbind(toKAGB, neueZeilen)
           },
           HGB={
             alleZiele <- hgb$hgbIndex[grepl(j, hgb$hgbIndex) | hgb$hgbIndex == j]
             if(length(alleZiele) == 0) next
             neueZeilen <- cbind(marisk[i,1], alleZiele)
             toHGB <- rbind(toHGB, neueZeilen)
           }
           )
  }
}

names(toKWG) <- c("fromMARISK", "toKWG")
names(toWPHG) <- c("fromMARISK", "toWPHG")
names(toKAGB) <- c("fromMARISK", "toKAGB")
names(toHGB) <- c("fromMARISK", "toHGB")

write.csv(toKWG, zielToKWG)
write.csv(toWPHG, zielToWPHG)
write.csv(toKAGB, zielToKAGB)
write.csv(toHGB, zielToHGB)