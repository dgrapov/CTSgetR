![CTSgetR](https://github.com/dgrapov/CTSgetR/blob/master/etc/ctsgetR_logo.png?raw=true)

R interface to the [Chemical Translation Service (CTS)] (http://cts.fiehnlab.ucdavis.edu/)

### Installation

```r
library(devtools) # install.packages("devtools") if missing
install_github(repo = "CTSgetR", username = "dgrapov",ref="simple")
library(CTSgetR)
```

### How to use the interface
```r
help(CTSgetR)
```

### Example usage

```r
library(CTSgetR)
#translate from chemical name to InchiKey
id<-"alanine"
from<-"Chemical Name"
to<-"InChIKey"
CTSgetR(id,from,to,progress=FALSE)
```

```
##   Chemical Name                    InChIKey
## 1       alanine QNAYBMKLOCPYGJ-REOHCLBHSA-N
```

```r
#translate from "InChIKey" to multiple identifiers
id<-c("DMULVCHRPCFFGV-UHFFFAOYSA-N","ZPUCINDJVBIVPJ-LJISPDSOSA-N","ZAGRKAFMISFKIO-QMTHXVAHSA-N")
from<-"InChIKey"
to<- c("Chemical Name", "PubChem CID", "KEGG","Human Metabolome Database")
multi.CTSgetR(id,from,to,progress=FALSE)
```

```
##                      InChIKey Chemical Name PubChem CID   KEGG
## 1 DMULVCHRPCFFGV-UHFFFAOYSA-N                      6089 C08302
## 2 ZPUCINDJVBIVPJ-LJISPDSOSA-N                    446220 C01416
## 3 ZAGRKAFMISFKIO-QMTHXVAHSA-N                  11861108       
##   Human Metabolome Database
## 1                 HMDB05973
## 2                          
## 3
```

```r
#return all possible results for the translation between "PubChem CID" and "Chemical Name"
id<-c("446220")
from<-"PubChem CID"
to<- c("Chemical Name")
CTSgetR(id,from,to,progress=FALSE,limit.values=FALSE)[,2]
```

```
## [1] Methyl (1R,2R,3S,5S)-3-(benzoyloxy)-8-methyl-8-azabicyclo[3.2.1]octane-2-carboxylate               
## [2] 8-Azabicyclo[3.2.1]octane-2-carboxylic acid, 3-(benzoyloxy)-8-methyl-, methyl ester, (1R,2R,3S,5S)-
## 2 Levels: Methyl (1R,2R,3S,5S)-3-(benzoyloxy)-8-methyl-8-azabicyclo[3.2.1]octane-2-carboxylate ...
```


### Check out some more [translation examples](https://github.com/dgrapov/CTSgetR/wiki/Chemical-Translation-System-in-R).

