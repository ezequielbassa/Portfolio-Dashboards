install.packages("rsconnect")
library(rsconnect)


rsconnect::setAccountInfo(name='ezequielbassa', 
token='86E1E20DB5661FCB62105CB343B9E013', 
secret='WGRY7sa9gT1J7eXHvJmc2bC5LRhPfSZN9/ankFV3')

rsconnect::deployApp(
  appDir  =
    "//wsl.localhost/Ubuntu-24.04/home/ezequielbassa/Portolio/1Workplace",
  appName = "maritime-climate-dashboard",
  appTitle = "Maritime Workplace Climate Survey 2026"
)