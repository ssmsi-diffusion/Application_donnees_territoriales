library("shiny")
repertoire = "C:/Users/ssmsi/Documents/AppTerritoires/donneesTerritoriales"
setwd(repertoire)

# # Lancer l'application dans la console R
# runApp(repertoire)

# # Déploiement de l'appli
library('rsconnect')

# rsconnect::setAccountInfo(name='ssmsi',
#                           token='CA22DD1C2191C1D6C86DFE839359D50E',
#                           secret='GmhKZECrVS2H6QHCmEs6j8zdt2xhkOI4rDCYXjCR')
#                           # token = 'BFDBFF87C5889195CCCEA39450D7140F',
#                           # secret = 'o6pjmlaMDn9PVDpUxN17rEoSgA86ocweQZKZYNDq')

options(rsconnect.locale = 'your_locale')
rsconnect::appDependencies()
rsconnect::deployApp(repertoire,account='ssmsi')

# rsconnect::showLogs(repertoire,account='ssmsi')

