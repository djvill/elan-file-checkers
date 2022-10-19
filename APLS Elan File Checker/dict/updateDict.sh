outFile=aplsDict.txt
# outFile=tmp.txt
Rscript -e "source('../../convertDict.R') ; convertDict('internal', dictPath='APLS-dict.csv', outFile='$outFile', save=TRUE)"