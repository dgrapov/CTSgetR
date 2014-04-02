![CTSgetR](https://github.com/dgrapov/CTSgetR/blob/master/etc/ctsgetR_logo.png?raw=true)

R interface to the [Chemical Translation Service (CTS)] (http://cts.fiehnlab.ucdavis.edu/)

### Installation
```R
install.packages("devtools")
install.packages("RJSONIO")
install.packages("RCurl") 
library(devtools)
library(RJSONIO)
library(RCurl) 
install_github(repo = "CTSgetR", username = "dgrapov")
library(CTSgetR)
```

### How to use the interface
```R
help(CTSgetR)
```

### Check out some [translation examples](https://github.com/dgrapov/CTSgetR/wiki/Chemical-Translation-System-in-R).
