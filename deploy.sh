#!/usr/bin/env bash
##deploy.sh: Deploy app to shinyapps.io

##Don't deploy if any testing setting is set to TRUE
sd=$(grep ^showDebug app.R)
ild=$(grep ^inclLocalDict app.R)
old=$(grep ^onlyLocalDict app.R)
if [[ $(wc -l <<<$sd) -gt 1 ]] || [[ $(wc -l <<<$ild) -gt 1 ]] || [[ $(wc -l <<<$old) -gt 1 ]]
then
	echo "ERROR in deploying app.R:"
	echo "  Found duplicate line(s) for showDebug, inclLocalDict, and/or onlyLocalDict"
	echo "  Remove duplicate line(s) from app.R, and re-run this script"
	exit 1
fi
if [[ $sd == *TRUE ]] || [[ $ild == *TRUE ]] || [[ $old == *TRUE ]]
then
	echo "ERROR in deploying app.R:"
	echo "  At least one of showDebug, inclLocalDict, or onlyLocalDict is set to TRUE"
	echo "  Set all to FALSE, and re-run this script"
fi

##Deploy app to shinyapps.io
Rscript -e "rsconnect::deployApp(appFiles=c('app.R', 'trs-utils.R', 'dict/unisynDict.txt', 'www', 'misc'), launch.browser=FALSE)"
