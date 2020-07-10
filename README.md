---
output:
  html_document:
    highlight: tango
    keep_md: yes
    theme: readable
---



## ![CTSgetR](https://github.com/dgrapov/CTSgetR/blob/master/etc/ctsgetR_logo.png?raw=true)

### R interface to the [Chemical Translation Service (CTS)](http://cts.fiehnlab.ucdavis.edu/)

#### `CTSgetR` provides a consitent interface to translation of chemical names and over 200 database identifiers including `InChIKey`, `HMDB`, `KEGG` and `PubChem`. Translation of chemical names is hard. Use `CTSgetR` to robustly translate chemical names to other identifiers through 1) conversion to `InChIKey` 2) `biological` or `popularity` scoring and 3) translation to over 200 biological database identifiers. `CTSgetR` uses a sqlite database to cache and speed all of your routine translations.

### Installation

```r
install_github("dgrapov/CTSgetR")
```


### View some of the possible translation options between > 200 databases

```r
library(CTSgetR)
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
##                   translation n
## 1 Chemical Name <--> InChIKey 4
## 2   InChIKey <--> PubChem CID 4
## 3       KEGG <--> PubChem CID 3
## 
## $total
## [1] 11
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

#### One to many

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



#### Many to one

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
### About

```r
sessionInfo()
```

```
## R version 3.6.2 (2019-12-12)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 18363)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] CTSgetR_1.3   RSQLite_2.2.0 httr_1.4.1    purrr_0.3.3   dplyr_0.8.3  
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.3       knitr_1.27       magrittr_1.5     tidyselect_0.2.5
##  [5] bit_1.1-15.2     R6_2.4.1         rlang_0.4.2      blob_1.2.1      
##  [9] stringr_1.4.0    tools_3.6.2      xfun_0.12        DBI_1.1.0       
## [13] htmltools_0.4.0  yaml_2.2.0       bit64_0.9-7      assertthat_0.2.1
## [17] digest_0.6.23    tibble_2.1.3     crayon_1.3.4     vctrs_0.2.1     
## [21] curl_4.3         zeallot_0.1.0    memoise_1.1.0    glue_1.3.1      
## [25] evaluate_0.14    rmarkdown_2.0    stringi_1.4.4    compiler_3.6.2  
## [29] pillar_1.4.3     backports_1.1.5  jsonlite_1.6     pkgconfig_2.0.3
```
