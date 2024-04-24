## tg-to-csv
##
## A lightweight Praat script for converting TextGrids to csv, suitable for
## calling on the command line
##

form: "Convert to csv"
	sentence: "inPath", ""
  sentence: "outPath", ""
endform

tg = Read from file: inPath$
table = Down to Table: 0, 6, 1, 0
if outPath$ = "" 
  basename$ = replace_regex$(inPath$, ".+/", "", 0)
  outPath$ = replace_regex$(basename$, "\.[Tt]ext[Gg]rid", ".csv", 1)
endif
Save as comma-separated file: outPath$
