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
##                      InChIKey
## 1 DMULVCHRPCFFGV-UHFFFAOYSA-N
## 2 ZPUCINDJVBIVPJ-LJISPDSOSA-N
## 3 ZAGRKAFMISFKIO-QMTHXVAHSA-N
##                                                                          Chemical Name
## 1                                                1H-Indole-3-ethanamine, N,N-dimethyl-
## 2 Methyl (1R,2R,3S,5S)-3-(benzoyloxy)-8-methyl-8-azabicyclo[3.2.1]octane-2-carboxylate
## 3                       Ergoline-8-carboxylic acid, 9,10-didehydro-6-methyl-, (8beta)-
##   PubChem CID   KEGG Human Metabolome Database
## 1        6089 C08302                 HMDB05973
## 2      446220 C01416                          
## 3    11861108
```

```r
#return all possible results for the translation between "PubChem CID" and "Chemical Name"
id<-c("446220")
from<-"PubChem CID"
to<- c("Chemical Name")
CTSgetR(id,from,to,progress=FALSE,limit.values=FALSE)
```

```
##   PubChem CID
## 1      446220
## 2      446220
##                                                                                         Chemical Name
## 1                Methyl (1R,2R,3S,5S)-3-(benzoyloxy)-8-methyl-8-azabicyclo[3.2.1]octane-2-carboxylate
## 2 8-Azabicyclo[3.2.1]octane-2-carboxylic acid, 3-(benzoyloxy)-8-methyl-, methyl ester, (1R,2R,3S,5S)-
```


### Check out some more [translation examples](https://github.com/dgrapov/CTSgetR/wiki/Chemical-Translation-System-in-R).

