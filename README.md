---
output:
  html_document:
    highlight: tango
    keep_md: yes
    theme: readable
---



## ![CTSgetR](https://github.com/dgrapov/CTSgetR/blob/master/inst/ctsgetR_logo.png?raw=true)

### R interface to the [Chemical Translation Service (CTS)](http://cts.fiehnlab.ucdavis.edu/)

#### `CTSgetR` provides a consitent interface to translation of chemical names and over 200 database identifiers including `InChIKey`, `HMDB`, `KEGG` and `PubChem`. Translation of chemical names is hard. Use `CTSgetR` to robustly translate chemical names to other identifiers through 1) conversion to `InChIKey` 2) `biological` or `popularity` scoring and 3) translation to over 200 biological database identifiers. `CTSgetR` uses a sqlite database to cache and speed all of your routine translations.

### Installation

```r
install_github("dgrapov/CTSgetR")
```

### Make sure CTS API is available

```r
library(CTSgetR)
GET('https://cts.fiehnlab.ucdavis.edu/services') %>%
  http_status(.) %>%
  {if( .$category != 'Success'){stop('Oops looks like https://cts.fiehnlab.ucdavis.edu/services is down!') }} 
```

### View some of the possible translation options between > 200 databases

```r
trans<-unlist(valid_from())
head(trans,10)
```

```
##  [1] "BioCyc"                    "CAS"                      
##  [3] "ChEBI"                     "Chemical Name"            
##  [5] "Human Metabolome Database" "InChIKey"                 
##  [7] "KEGG"                      "LMSD"                     
##  [9] "LipidMAPS"                 "PubChem CID"
```

### Find a database of interest

```r
want<-'CID'
trans[grepl(want,trans,ignore.case=TRUE)]
```

```
## [1] "PubChem CID"
```


### Initialize a local database to speed up routine queries

```r
db_name<-'cts.sqlite'
init_CTSgetR_db(db_name)
db_stats()
```

```
## [[1]]
##                   translation  n
## 1 Chemical Name <--> InChIKey  4
## 2   InChIKey <--> PubChem CID  4
## 3       KEGG <--> PubChem CID 22
## 
## $total
## [1] 30
```


### Translation examples

#### `Chemical Name` to `InChIKey`

```r
id<-c("alanine",'lactic acid')
from<-"Chemical Name"
to<-"InChIKey"

CTSgetR(id,from,to,db_name)
```

```
##            id          from       to                         key
## 1     alanine Chemical Name InChIKey QNAYBMKLOCPYGJ-REOHCLBHSA-N
## 2 lactic acid Chemical Name InChIKey JVTAAEKCZFNVCJ-UHFFFAOYSA-N
```

#### One identifier to many

```r
id<-c("alanine",'lactic acid')
from<-"Chemical Name"
to<- c( "PubChem CID", "KEGG","Human Metabolome Database")

(out<-to %>%
  map(~CTSgetR(id,from,.,db_name)) 
  %>% do.call('rbind',.))
```

```
##            id          from                        to         key
## 1     alanine Chemical Name               PubChem CID        5950
## 2 lactic acid Chemical Name               PubChem CID    19789253
## 3     alanine Chemical Name                      KEGG      C00041
## 4 lactic acid Chemical Name                      KEGG      C01432
## 5     alanine Chemical Name Human Metabolome Database HMDB0000161
## 6 lactic acid Chemical Name Human Metabolome Database HMDB0144295
```



#### Many identifiers to one

```r
to<- "InChIKey"

(out2<-out %>%
  pmap_dfr(function(...) {
    tmp <- tibble(...)
    CTSgetR(tmp$key,tmp$to,to,db_name)
  
  }))
```

```
##            id                      from       to                         key
## 1        5950               PubChem CID InChIKey QNAYBMKLOCPYGJ-REOHCLBHSA-N
## 2    19789253               PubChem CID InChIKey JVTAAEKCZFNVCJ-UHFFFAOYSA-N
## 3      C00041                      KEGG InChIKey QNAYBMKLOCPYGJ-REOHCLBHSA-N
## 4      C01432                      KEGG InChIKey JVTAAEKCZFNVCJ-UHFFFAOYSA-N
## 5 HMDB0000161 Human Metabolome Database InChIKey QNAYBMKLOCPYGJ-REOHCLBHSA-N
## 6 HMDB0144295 Human Metabolome Database InChIKey JVTAAEKCZFNVCJ-UHFFFAOYSA-N
```
