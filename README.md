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
##   fromIdentifier searchTerm toIdentifier                       value
## 1  Chemical Name    alanine     InChIKey QNAYBMKLOCPYGJ-REOHCLBHSA-N
```

```r
#translate from one to many identifiers 
id<-c("DMULVCHRPCFFGV-UHFFFAOYSA-N","ZPUCINDJVBIVPJ-LJISPDSOSA-N","ZAGRKAFMISFKIO-QMTHXVAHSA-N")
from<-"InChIKey"
to<- c("Chemical Name", "PubChem CID", "KEGG","Human Metabolome Database")
CTSgetR(id,from,to,progress=FALSE,limit.values = FALSE)
```

```
## Warning in if (!to %in% opts | !from %in% opts) {: the condition has length
## > 1 and only the first element will be used
```

```
##    fromIdentifier                  searchTerm              toIdentifier
## 1        InChIKey DMULVCHRPCFFGV-UHFFFAOYSA-N             Chemical Name
## 2        InChIKey ZPUCINDJVBIVPJ-LJISPDSOSA-N             Chemical Name
## 3        InChIKey ZPUCINDJVBIVPJ-LJISPDSOSA-N             Chemical Name
## 4        InChIKey ZAGRKAFMISFKIO-QMTHXVAHSA-N             Chemical Name
## 5        InChIKey ZAGRKAFMISFKIO-QMTHXVAHSA-N             Chemical Name
## 6        InChIKey DMULVCHRPCFFGV-UHFFFAOYSA-N               PubChem CID
## 7        InChIKey ZPUCINDJVBIVPJ-LJISPDSOSA-N               PubChem CID
## 8        InChIKey ZAGRKAFMISFKIO-QMTHXVAHSA-N               PubChem CID
## 9        InChIKey ZAGRKAFMISFKIO-QMTHXVAHSA-N               PubChem CID
## 10       InChIKey DMULVCHRPCFFGV-UHFFFAOYSA-N                      KEGG
## 11       InChIKey ZPUCINDJVBIVPJ-LJISPDSOSA-N                      KEGG
## 12       InChIKey ZAGRKAFMISFKIO-QMTHXVAHSA-N                      KEGG
## 13       InChIKey DMULVCHRPCFFGV-UHFFFAOYSA-N Human Metabolome Database
## 14       InChIKey ZPUCINDJVBIVPJ-LJISPDSOSA-N Human Metabolome Database
## 15       InChIKey ZAGRKAFMISFKIO-QMTHXVAHSA-N Human Metabolome Database
##                                                                                                  value
## 1                                                                                                error
## 2                 Methyl (1R,2R,3S,5S)-3-(benzoyloxy)-8-methyl-8-azabicyclo[3.2.1]octane-2-carboxylate
## 3  8-Azabicyclo[3.2.1]octane-2-carboxylic acid, 3-(benzoyloxy)-8-methyl-, methyl ester, (1R,2R,3S,5S)-
## 4                                       Ergoline-8-carboxylic acid, 9,10-didehydro-6-methyl-, (8beta)-
## 5                                            (8beta)-6-Methyl-9,10-didehydroergoline-8-carboxylic acid
## 6                                                                                                 6089
## 7                                                                                               446220
## 8                                                                                             11861108
## 9                                                                                                 6717
## 10                                                                                              C08302
## 11                                                                                              C01416
## 12                                                                                                    
## 13                                                                                           HMDB05973
## 14                                                                                                    
## 15
```

### Check out some more [translation examples](https://github.com/dgrapov/CTSgetR/wiki/Chemical-Translation-System-in-R).

## TODO
- [ ] Get POST to work
- [ ] Make it more awesome!

