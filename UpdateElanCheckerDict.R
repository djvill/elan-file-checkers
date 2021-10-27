#### UpdateElanCheckerDict.R
#### Dan Villarreal
####
#### Combines dictionaries from Celex and LaBB-CAT for Southland Elan File Checker.
####


##This script is the middle step in a 3-step process to update the dictionary
##  used by the Southland Elan File Checker app, assuming Celex English (or at
##  least the EOW.cd file) is already downloaded:
##1. Download dictionary (LaBB-CAT > word layers > phonemes layer dictionary >
##  Export to CSV, aka https://labbcat.canterbury.ac.nz/southland/admin/layers/dictionary?layer_id=50) 
##  as "dictionary_phonemes.csv" into the Celex English folder.
##2. Source this script
##3. Republish the app on shinyapps.io

library(here)
library(stringr)

##Read CELEX English EOW file (English Orthography, Wordforms)
celex <- read.table(here("Celex English/EOW/EOW.CD"), 
                    header=FALSE, sep="\\", quote="", fill=TRUE)
##2nd column (main spellings) & 10th column (alternative spellings, which has many empties)
celex <- c(as.character(celex[,2]), str_subset(as.character(celex[,10]), "", negate=TRUE))

##Add downloaded dictionary
if (file.exists("Celex English/dictionary_phonemes.csv")) {
  added <- read.csv("Celex English/dictionary_phonemes.csv", header=FALSE)[,1]
} else {
  added <- character(0L)
}


##Add allowed tilde codes
##See https://sourceforge.net/p/labbcat/code/HEAD/tree/WEB-INF/classes/nz/ac/canterbury/ling/celex/english/CelexEnglishDataGenerator.java#l1501 [r2458]
vLetters <- c("a", "e", "i", "o", "u")
cLetters <- setdiff(letters, vLetters)
goodDigraphs <- c("ng", "th", "dh", "sh", "ch", "wh", "ph", "gn", "kn", "pn", "ps", "pt", "wr")
plusV <- c(outer(c(cLetters, goodDigraphs), vLetters, paste0))
tildeCodes <- paste0(c(letters, goodDigraphs, plusV), "~")

##Put dictionaries together
dict <- unique(c(as.character(celex), as.character(added), tildeCodes))
suppressWarnings(dict <- c(dict[is.na(as.numeric(dict))], "infinity"))
dict <- tolower(dict)
dict <- dict[!duplicated(dict)]
writeLines(dict, "CB Elan File Checker/dict/userDict.txt")
writeLines(dict, "JH Elan File Checker/dict/userDict.txt")
writeLines(dict, "SouthlandCBElanFileCheckerZIP/dict/userDict.txt")

rm(celex, added, vLetters, cLetters, goodDigraphs, plusV, tildeCodes)