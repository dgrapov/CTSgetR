---
output:
  html_document:
    highlight: tango
    keep_md: yes
    theme: readable
---



# ![CTSgetR](https://github.com/dgrapov/CTSgetR/blob/master/inst/ctsgetR_logo.png?raw=true)

<hr>

## R interface to the [Chemical Translation Service (CTS)](http://cts.fiehnlab.ucdavis.edu/)

<hr>

#### `CTSgetR` provides a consitent interface to translation of chemical names and over 200 database identifiers including `InChIKey`, `HMDB`, `KEGG` and `PubChem`. Translation of chemical names is hard. Use `CTSgetR` to robustly translate chemical names to other identifiers through 1) conversion to `InChIKey` 2) `biological` or `popularity` scoring and 3) translation to over 200 biological database identifiers. `CTSgetR` uses a sqlite database to cache and speed all of your routine translations.


#### This package supports metabolite identifier translation:

* #### [in R](#in-R)
* #### deployed as an [opencpu](https://www.opencpu.org/) [API](#API)
* #### from a [shiny UI](#shiny) using asynchronous local or API calls

<a name="in-R"></a>

### Installation 


### Make sure CTS API is available


### View some of the possible translation options between > 200 databases

```
##  [1] "BioCyc"                    "CAS"                      
##  [3] "ChEBI"                     "Chemical Name"            
##  [5] "Human Metabolome Database" "InChIKey"                 
##  [7] "KEGG"                      "LMSD"                     
##  [9] "LipidMAPS"                 "PubChem CID"
```

### Find a database of interest

```
## [1] "PubChem CID"
```


### Initialize a local database to speed up routine queries

```
## [1] "Creating a new database"
```

```
## [[1]]
##                               translation  n
## 1             Chemical Name <--> InChIKey 10
## 2 Human Metabolome Database <--> InChIKey  1
## 3                      InChIKey <--> KEGG  6
## 4               InChIKey <--> PubChem CID  4
## 5                   KEGG <--> PubChem CID 22
## 
## $total
## [1] 43
```


### Translation examples

#### `Chemical Name` to `InChIKey`

```
##            id          from       to                         key
## 1     alanine Chemical Name InChIKey QNAYBMKLOCPYGJ-REOHCLBHSA-N
## 2 lactic acid Chemical Name InChIKey JVTAAEKCZFNVCJ-UHFFFAOYSA-N
```

#### One identifier to many
##### The example below shows the alternative `data.frame` input format for more complex queries.

```
##            id          from                        to
## 1     alanine Chemical Name               PubChem CID
## 2 lactic acid Chemical Name               PubChem CID
## 3     alanine Chemical Name                      KEGG
## 4 lactic acid Chemical Name                      KEGG
## 5     alanine Chemical Name Human Metabolome Database
## 6 lactic acid Chemical Name Human Metabolome Database
```

```
##            id          from                        to         key
## 1     alanine Chemical Name Human Metabolome Database HMDB0000161
## 2 lactic acid Chemical Name Human Metabolome Database HMDB0144295
## 3     alanine Chemical Name                      KEGG      C00041
## 4 lactic acid Chemical Name                      KEGG      C01432
## 5     alanine Chemical Name               PubChem CID        5950
## 6 lactic acid Chemical Name               PubChem CID    19789253
```



#### Many identifiers to one
##### Build up complex queries by combingn  data frames of `id`, `from` to `to` values.

```
##            id                      from                        to
## 1     alanine             Chemical Name               PubChem CID
## 2 lactic acid             Chemical Name               PubChem CID
## 3     alanine             Chemical Name                      KEGG
## 4 lactic acid             Chemical Name                      KEGG
## 5     alanine             Chemical Name Human Metabolome Database
## 6 lactic acid             Chemical Name Human Metabolome Database
## 7 HMDB0000161 Human Metabolome Database                      KEGG
## 8 HMDB0000161 Human Metabolome Database               PubChem CID
```

```
##            id                      from                        to         key
## 1     alanine             Chemical Name Human Metabolome Database HMDB0000161
## 2 lactic acid             Chemical Name Human Metabolome Database HMDB0144295
## 3     alanine             Chemical Name                      KEGG      C00041
## 4 lactic acid             Chemical Name                      KEGG      C01432
## 5     alanine             Chemical Name               PubChem CID        5950
## 6 lactic acid             Chemical Name               PubChem CID    19789253
## 7 HMDB0000161 Human Metabolome Database                      KEGG      C00041
## 8 HMDB0000161 Human Metabolome Database               PubChem CID        5950
```

<hr>

<a name="API"></a>

## Deploy `CTSgetR` as a `docker`ized `API`

<hr>

### The following [docker]() image and [docker-compose]() commands can be used to `build` and run the `CTSgetR` package as an [opencpu](https://hub.docker.com/r/opencpu/ubuntu-18.04) based `API`.

* ### [CTSgetR image]()
* ### [ocpuclient](): client library for accessign `CTSgetR` `API`

### The `CTSgetR` image contains an opencpu and Rstudio server
* ### `localhost/ocpu/`: [opencpu-server](https://www.opencpu.org/)
* ### `localhost/rstudio/` : [rstudio server](https://hub.docker.com/r/opencpu/rstudio) (use user: opencpu and password:<mypassword> )

###  Build docker `image`
#### `build`
```
export rstudio_pass=mypassword # rstudio server password for user opencpu
docker-compose -f docker-compose.yml build --force-rm

```

#### Launch API
```
#mount to persist internal sqlite DB between updates 
export ctsgetr_db_mount=<local path to save database e.g. /mypath>
docker-compose -f docker-compose.yml up -d

```

#### Test API endpoints

##### `bash`
```bash
curl http://localhost/ocpu/library/CTSgetR/R/heartbeat
```
#### `R`
##### heartbeat
```r
library(ocpuclient)

base_url<-'http://localhost/ocpu/'

endpoint<-'library/CTSgetR/R/heartbeat'
url<-paste0(base_url,endpoint)
post_ocpu(url=url)
```

##### translation
```r
#translate
endpoint<-'library/CTSgetR/R/CTSgetR'
url<-paste0(base_url,endpoint)

id <-
  c("C15973",
    "C00026")
from <- "KEGG"
to <- "PubChem CID"

body<-list(id=id,from=from,to=to,db_name=db_name)


post_ocpu(url=url,body=body)

```

<a name="shiny"></a>

## Launch `shiny` UI using asynchronous `opencpu` API 

#### The following example shows a how to use a `shiny` module combined with `futures` and `promises`  `R` packages to connect to an `opencpu` API. 

```r
library(shiny)
library(tippy)
library(CTSgetR) # local calls
library(ocpuclient) # CTSgetR opencpu API calls
```

#### Specify local database or API options
```r
#one of local
Sys.setenv('ctsgetr_DB'='inst/ctsgetr.sqlite') #see section `in R` showing how to initialize a local databse
#or API
Sys.setenv('ctsgetr_DB'='/ctsgetr/inst/ctsgetr.sqlite') # in API docker for mount
Sys.setenv('CTSgetR_API'='http://localhost/ocpu/library/CTSgetR/R/CTSgetR') # url of API endpoint
````

#### User input translations

```r
    library(promises)
    library(future)
    plan(multisession)
    
    
    #module
    ui <- fluidPage(
      
      sidebarLayout(position = "left",
                    sidebarPanel(tagList(mod_CTSgetR_ui("translate"))),
                    mainPanel(verbatimTextOutput("main_out")))
      
    )
    
    server <- function(input, output, session) {
      
      translation <- mod_CTSgetR_server('translate')
      
      output$main_out <- renderPrint({
        translation() %...>% print(.)
        
      })
    }
    
    shinyApp(ui, server)

```

#### Connect to other shiny components
```r 
library(promises)
    library(future)
    plan(multisession)
    
    
    example<-data.frame('chemical_name' = c('alanine','DMT'))
    
    #module
    ui <- fluidPage(
      
      sidebarLayout(position = "left",
                    sidebarPanel(tagList(mod_CTSgetR_ui("translate"))),
                    mainPanel(verbatimTextOutput("main_out")))
      
    )
    
    server <- function(input, output, session) {
      
      #make `example` a reactive returning a data frame to update dynamically
      translation <- mod_CTSgetR_server('translate',data=example)
      
      output$main_out <- renderPrint({
        translation() %...>% print(.)
        
      })
    }
    
    shinyApp(ui, server)
```
