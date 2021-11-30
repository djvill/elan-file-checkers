#### convertDict.R
#### Dan Villarreal
####
#### Converts dictionaries from Celex and LaBB-CAT to txt format for Southland Elan File Checker.
####
#### Only needs to be run once for CELEX & tilde dictionaries.
#### To update LaBB-CAT dictionary, download (LaBB-CAT > word layers > phonemes
#### layer dictionary > Export to CSV) and save as "dictionary_phonemes.csv" to
#### relevant dict folder
####

convertDict <- function(type = c("CELEX", "LaBB-CAT", "tilde")[1], root, dictPath, 
                        outPath = paste(c("APLS", "CB", "JH"), "Elan File Checker/")[1], 
                        outFile, save = FALSE) {
  ##Check type
  if (!(type %in% c("CELEX", "LaBB-CAT", "tilde"))) {
    stop("Unknown type: ", type)
  }
  
  ##Get root if missing
  if (missing(root)) {
    library(here)
    root <- paste0(here(), "/")
  }
  
  ##Get path to file if missing
  if (type %in% c("CELEX", "LaBB-CAT") && missing(dictPath)) {
    if (type=="CELEX") {
      ##Celex: EOW file (English Orthography, Wordforms)
      dictPath <- paste0(root, "Celex English/EOW/EOW.CD") 
    } else if (type=="LaBB-CAT") { 
      ##LaBB-CAT: Downloaded phonemes file
      dictPath <- paste0(root, outPath, "dict/dictionary_phonemes.csv")
    }
    
    ##Check that file exists
    if (!file.exists(dictPath)) {
      stop("File does not exist at ", dictPath)
    }
  }
  
  ##Read & process file
  if (type=="CELEX") {
    celex <- read.table(dictPath, header=FALSE, sep="\\", quote="", fill=TRUE)
    ##2nd column (main spellings) & 10th column (alternative spellings, which has many empties)
    dict <- c(as.character(celex[,2]), stringr::str_subset(as.character(celex[,10]), ".+"))
  } else if (type=="LaBB-CAT") { 
    library(readr)
    dict <- unique(read_csv(dictPath, col_names=c("word","pronounce"), col_types="cc", 
                            quote="", comment="\"##")$word)
  } else if (type=="tilde") {
    vLetters <- c("a", "e", "i", "o", "u")
    cLetters <- setdiff(letters, vLetters)
    goodDigraphs <- c("ng", "th", "dh", "sh", "ch", "wh", "ph", "gn", "kn", "pn", "ps", "pt", "wr")
    plusV <- c(outer(c(cLetters, goodDigraphs), vLetters, paste0))
    dict <- paste0(c(letters, goodDigraphs, plusV), "~")
  }
  
  ##Optionally save
  if (save) {
    ##If outfile missing, get outfile
    if (missing(outFile)) {
      if (type=="CELEX") {
        outFile <- paste0(root, outPath, "dict/celexDict.txt") 
      } else if (type=="LaBB-CAT") { 
        outFile <- paste0(root, outPath, "dict/labbCatDict.txt") 
      } else if (type=="tilde") {
        outFile <- paste0(root, outPath, "dict/tildeCodes.txt")
      }
    }
    
    writeLines(dict, outFile)
  }
  
  ##Return dictionary invisibly
  invisible(dict)
}



