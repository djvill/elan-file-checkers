#### UpdateElanCheckerDict.R
####
#### Dan Villarreal
#### Version 1.0.2
#### 20 August 2018
####
#### Combines dictionaries from Celex and LaBB-CAT for Southland Elan File Checker.
####

##This script is the middle step in a 3-step process to update the dictionary
##  used by the Southland Elan File Checker app, assuming Celex English (or at
##  least the EOW.cd file) is already downloaded:
##1. Download dictionary (LaBB-CAT > word layers > phonemes layer dictionary >
##  Export to CSV, aka https://labbcat.canterbury.ac.nz/southland/admin/layers/dictionary?layer_id=50) 
##  as "dictionary_phonemes.csv". (If it's not in the Downloads folder, it should 
##  be in the Celex English folder.)
##2. Source this script
##3. Republish the app on shinyapps.io

oldWD <- getwd()
setwd("C:/Users/Dan/Documents/Daily Docs/Research/NZILBB/Southland/Corpus-Building/Elan File Checkers/")
downloadDir <- "C:/Users/Dan/Downloads/"
celex <- read.table("Celex English/EOW/EOW.CD", header=FALSE, sep="\\", quote="", fill=TRUE)
celex <- c(as.character(celex[,2]), as.character(celex[,10]))
celex <- celex[celex!=""]
if (file.exists(paste0(downloadDir, "dictionary_phonemes.csv"))) {
  file.copy(from=paste0(downloadDir, "dictionary_phonemes.csv"), 
            to="Celex English/dictionary_phonemes.csv",
            overwrite=TRUE)
  file.remove(paste0(downloadDir, "dictionary_phonemes.csv"))
}
stopifnot(file.exists("Celex English/dictionary_phonemes.csv"))
added <- read.csv("Celex English/dictionary_phonemes.csv", header=FALSE)[,1]
vLetters <- c("a", "e", "i", "o", "u")
cLetters <- letters[!(letters %in% vLetters)]
goodDigraphs <- c("ng", "th", "dh", "sh", "ch", "wh", "ph", "gn", "kn", "pn", "ps", "pt", "wr")
(plusV <- c(outer(c(cLetters, goodDigraphs), vLetters, paste0)))
(tildeCodes <- paste0(c(letters, goodDigraphs, plusV), "~"))
##See https://sourceforge.net/p/labbcat/code/HEAD/tree/WEB-INF/classes/nz/ac/canterbury/ling/celex/english/CelexEnglishDataGenerator.java#l1501 [r2458]
dict <- unique(c(as.character(celex), as.character(added), tildeCodes))
suppressWarnings(dict <- c(dict[is.na(as.numeric(dict))], "infinity"))
dict <- tolower(dict)
dict <- dict[!duplicated(dict)]
writeLines(dict, "CB Elan File Checker/dict/dict.txt")
writeLines(dict, "JH Elan File Checker/dict/dict.txt")
setwd(oldWD)
rm(downloadDir, celex, added, vLetters, cLetters, goodDigraphs, plusV, tildeCodes, oldWD)