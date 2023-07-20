#package installation
#set up system library

install.packages("terra")
install.packages("remotes")
install.packages("Require") #if this is off CRAN try 
#remotes::install_github("PredictiveEcology/Require")
install.packages("data.table")
install.packages("magrittr")
install.packages("whitebox") #for euclidean distance
#note you will likely have to manually download WhiteboxTools - 
#https://www.whiteboxgeo.com/WBT_Windows/WhiteboxTools_win_amd64.zip
#and extracting to a directory, then running "wbt_init(exe_path = '<the path>')
whitebox::wbit_init("../WhiteboxTools_win_amd64/WBT/whitebox_tools.exe")

Require::pkgSnapshot()
