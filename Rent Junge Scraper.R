##Need to clean up saveloc logic.
##Add another question prompt about what the file name should be
##Have a seperate object for the file name so that it doesn't try
##To save a file named "" to the working directory and therefore 
##Fail each time.


rentscrape <- function(){
  #options(show.error.messages = FALSE)
  library(RCurl)
  library(XML)
  library(gsubfn)
  library(stringr)
  savloc <- getwd()
  input <- "https://www.rentjungle.com/average-rent-in-boston-rent-trends/"
  txt <- getURLContent(input)[[1]]
  tab <- strsplit(txt,"data=")[[1]]
  usetab <- paste(",", tab[2], sep ="")
  c1 <- gsub('\\\\', '', usetab)
  c2 <- gsub("\\[", "", c1)
  c3 <- gsub("n", "", c2)
  c4 <- strsplit(c3, split= "\\]")
  tt <- data.frame(X=c4)
  colnames(tt) <- "X"
  tt1 <- data.frame(X=gsub('"', "", tt$X))
  tt2 <- data.frame(X=str_split_fixed(tt1$X, ",", n= Inf))
  dates <- tt2$X.2
  dates <- data.frame(str_split_fixed(dates, "/", n= Inf))
  daten <- c("Month", "Year")
  colnames(dates) <- daten
  tt3 <- cbind.data.frame(Month=dates$Month, Year=dates$Year, AllBeds=tt2$X.3, OneBed=tt2$X.4, TwoBeds=tt2$X.5)
    print(tail(tt3))
  cont <- toupper(readline("Does this look right? (Y/N)......................"))
  if (cont == "N"){
    ccd <- toupper(readline("Quit? (Y/N)................"))
    if (ccd == "Y"){
    stop()
    } else rentscrape()
  } else 
  writeLines(c("The current save directory is:", savloc))
  savck <- toupper(readline("Would you like to change the save file directory? (Y/N)................"))
  if (savck == "Y"){ repeat {
  savloc  <- toupper(readline("Please enter the file name and directory path you want all of this to get saved to (path/to/my.csv):"))
  savloc <- gsub("\\\\", "\\/", savloc)
  writeLines(c("You have changed the save directory to:", savloc))
  e <- toupper(readline("Is this right? (Y/N)............."))
  if (e == "Y") break}
  } else
  write.table(tt3, file = savloc, sep = ",", row.names = FALSE)
  NAS <- na.omit(read.csv(file = savloc))
  write.table(NAS, file = savloc, sep = ",", row.names = FALSE)
  UpdateMon <- NAS[nrow(NAS),"Month"]
  printmon <- if (UpdateMon == 01){
    "January"
  } else if (UpdateMon == 02){
    "February"
  } else if (UpdateMon == 03){
    "March"
  } else if (UpdateMon == 04){
    "April"
  } else if (UpdateMon == 05){
    "May"
  } else if (UpdateMon == 06){
    "June"
  } else if (UpdateMon == 07){
    "July"
  } else if (UpdateMon == 08){
    "August"
  } else if (UpdateMon == 09){
    "September"
  } else if (UpdateMon == 10){
    "October"
  } else if (UpdateMon == 11){
    "November"
  } else if (UpdateMon == 12){
    "December"
  }
  UpdateYear <- NAS[nrow(NAS),"Year"]
  finmes <- data.frame(MONTH=printmon, YEAR=UpdateYear)
  writeLines(sprintf("You have scraped data through %s, %s", finmes$MONTH, finmes$YEAR))
  writeLines("Goodbye")
break}

