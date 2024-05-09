## praat-version
##
## On R 4.3.2 for Windows, system("Praat.exe --version") only returns a single
## character "P" rather than the full version string (e.g., "Praat 6.4.10 
## (April 21 2024)"). This is a workaround (albeit one that only returns e.g.
## "6.4.10")
##

form: "Write Praat version to file"
  sentence: "outPath", ""
endform

writeFileLine: outPath$, praatVersion$
