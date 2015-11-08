[![paypal](https://raw.githubusercontent.com/dgrapov/CDS/gh-pages/www/images/donate.png)](http://createdatasol.com/)

![CTSgetR](https://github.com/dgrapov/CTSgetR/blob/master/etc/ctsgetR_logo.png?raw=true)

R interface to the [Chemical Translation Service (CTS)] (http://cts.fiehnlab.ucdavis.edu/)

### Installation
```r
library(devtools) # install.packages("devtools") if missing
library(jsonlite) # install.packages("jsonlite")
install_github(repo = "CTSgetR", username = "dgrapov")
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
## 1       alanine QNAYBMKLOCPYGJ-UWTATZPHSA-N
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
## 1 DMULVCHRPCFFGV-UHFFFAOYSA-N           DMT        6089 C08302
## 2 ZPUCINDJVBIVPJ-LJISPDSOSA-N   (-)-Cocaine      446220 C01416
## 3 ZAGRKAFMISFKIO-QMTHXVAHSA-N LSD precursor        6717       
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
##   [1] Moonrocks                                                                                                
##   [2] methyl (1S,3S,4R,5R)-8-methyl-3-(phenylcarbonyloxy)-8-azabicyclo[3.2.1]octane-4-carboxylate              
##   [3] Freeze                                                                                                   
##   [4] benzoylmethylecgonine                                                                                    
##   [5] Cocain                                                                                                   
##   [6] methyl (1S,3S,4R,5R)-3-benzoyloxy-8-methyl-8-azabicyclo[3.2.1]octane-4-carboxylate                       
##   [7] (-)-Cocaine                                                                                              
##   [8] Hell                                                                                                     
##   [9] 3beta-Hydroxy-2beta-tropanecarboxylic acid methyl ester benzoate (ester)                                 
##  [10] [1R-(exo,exo)]-3-(benzoyloxy)-8-methyl-8-azabicyclo[3.2.1]octane-2-carboxylic acid, methyl ester         
##  [11] Foo Foo                                                                                                  
##  [12] Cabello                                                                                                  
##  [13] Benzoylmethylecgonine                                                                                    
##  [14] l-Cocain                                                                                                 
##  [15] COCAINE                                                                                                  
##  [16] 8-Azabicyclo[3.2.1]octane-2-carboxylic acid, 3-(benzoyloxy)-8-methyl-, methyl ester, (1R,2R,3S,5S)- (9CI)
##  [17] Corine                                                                                                   
##  [18] beta-Cocain                                                                                              
##  [19] Happy powder                                                                                             
##  [20] Ecgonine methyl ester benzoate                                                                           
##  [21] Cholly                                                                                                   
##  [22] 3beta-Hydroxy-1alphaH,5alphaH-tropane-2beta-carboxylic acid methyl ester benzoate                        
##  [23] Kokan                                                                                                    
##  [24] Yeyo                                                                                                     
##  [25] Charlie                                                                                                  
##  [26] Ecgonine methyl ester benzoate solution                                                                  
##  [27] methylbenzoylecgonine                                                                                    
##  [28] Star dust                                                                                                
##  [29] (R)-Cocaine                                                                                              
##  [30] Girl                                                                                                     
##  [31] 2-methyl-3beta-hydroxy-1alphaH,5alphaH-tropane-2beta-carboxylate benzoate (ester)                        
##  [32] Cecil                                                                                                    
##  [33] Kibbles n' Bits                                                                                          
##  [34] Cocaine solution                                                                                         
##  [35] Cocaine                                                                                                  
##  [36] ecgonine methyl ester benzoate                                                                           
##  [37] Blow                                                                                                     
##  [38] Methyl 3beta-hydroxy-1alphaH,5alphaH-tropane-2beta-carboxylate benzoate (ester)                          
##  [39] Cocaina                                                                                                  
##  [40] Dust                                                                                                     
##  [41] Methyl Benzoylecgonine                                                                                   
##  [42] Bump                                                                                                     
##  [43] Rock                                                                                                     
##  [44] 1-alpha-H,5-alpha-H-Tropane-2-beta-carboxylic acid, 3-beta-hydroxy-, methyl ester, benzoate (ester) (8CI)
##  [45] Ecgonine, methyl ester, benzoate (ester)                                                                 
##  [46] Bernies                                                                                                  
##  [47] Bouncing Powder                                                                                          
##  [48] methyl [1R-(exo,exo)]-3-(benzoyloxy)-8-methyl-8-azabicyclo[3.2.1]octane-2-carboxylate                    
##  [49] Kokain                                                                                                   
##  [50] Methyl 3-beta-hydroxy-1-alpha-H,5-alpha-H-tropane-2-beta-carboxylate benzoate (ester)                    
##  [51] Cocktail                                                                                                 
##  [52] Kokayeen                                                                                                 
##  [53] Coke                                                                                                     
##  [54] Caviar                                                                                                   
##  [55] (1R,2R,3S,5S)-2-Methoxycarbonyltropan-3-yl benzoate                                                      
##  [56] Toke                                                                                                     
##  [57] cocaine                                                                                                  
##  [58] Snort                                                                                                    
##  [59] Happy dust                                                                                               
##  [60] Leaf                                                                                                     
##  [61] 8-Azabicyclo(3.2.1)octane-2-carboxylic acid, 3-(benzoyloxy)-8-methyl-, methyl ester, (1R-(exo,exo))-     
##  [62] Flake                                                                                                    
##  [63] 1-alpha-H,5-alpha-H-Tropane-2-beta-carboxylic acid, 3-beta-hydroxy-, methyl ester, benzoate              
##  [64] Toot                                                                                                     
##  [65] Cocaine free base                                                                                        
##  [66] 1-Cocaine                                                                                                
##  [67] Burese                                                                                                   
##  [68] Green gold                                                                                               
##  [69] Prime Time                                                                                               
##  [70] COC                                                                                                      
##  [71] HSDB 6469                                                                                                
##  [72] Chicken Scratch                                                                                          
##  [73] Cocaine, l-                                                                                              
##  [74] Gold dust                                                                                                
##  [75] cocainum                                                                                                 
##  [76] Dama blanca                                                                                              
##  [77] 3-Tropanylbenzoate-2-carboxylic acid methyl ester                                                        
##  [78] Eritroxilina                                                                                             
##  [79] Flex                                                                                                     
##  [80] (1R,2R,3S,5S)-2-(methoxycarbonyl)tropan-3-yl benzoate                                                    
##  [81] Pimp's drug                                                                                              
##  [82] methyl benzoylecgonine                                                                                   
##  [83] 3-(Benzoyloxy)-8-methyl-8-azabicyclo-(3.2.1)octane-2-carboxylic acid methyl ether                        
##  [84] Blast                                                                                                    
##  [85] Florida Snow                                                                                             
##  [86] Heaven                                                                                                   
##  [87] G-Rock                                                                                                   
##  [88] Lady                                                                                                     
##  [89] 2-beta-Tropanecarboxylic acid, 3-beta-hydroxy-, methyl ester, benzoate (ester)                           
##  [90] 50-36-2                                                                                                  
##  [91] (1S,3S,4R,5R)-3-benzoyloxy-8-methyl-8-azabicyclo[3.2.1]octane-4-carboxylic acid methyl ester             
##  [92] Jam                                                                                                      
##  [93] Badrock                                                                                                  
##  [94] Erytroxylin                                                                                              
##  [95] Blizzard                                                                                                 
##  [96] Bazooka                                                                                                  
##  [97] Star-spangled powder                                                                                     
##  [98] Happy trails                                                                                             
##  [99] Sweet Stuff                                                                                              
## [100] Sugar                                                                                                    
## [101] Cola                                                                                                     
## [102] Goofball                                                                                                 
## [103] C" Carrie                                                                                                
## [104] 2b-Carbomethoxy -3b-benzoyloxy tropane                                                                   
## [105] Neurocaine                                                                                               
## [106] (-)-cocaine                                                                                              
## [107] 2-beta-Carbomethoxy-3-beta-benzoxytropane                                                                
## [108] Zip                                                                                                      
## [109] Candy                                                                                                    
## [110] Sleighride                                                                                               
## [111] l-cocaine                                                                                                
## [112] Bernice                                                                                                  
## [113] L-Cocaine                                                                                                
## [114] methyl (1R,2R,3S,5S)-3-(benzoyloxy)-8-methyl-8-azabicyclo[3.2.1]octane-2-carboxylate                     
## [115] White girl or lady                                                                                       
## [116] Line                                                                                                     
## [117] Coca                                                                                                     
## [118] Snow (birds)                                                                                             
## [119] Trails                                                                                                   
## 119 Levels: Moonrocks ...
```


### Check out some more [translation examples](https://github.com/dgrapov/CTSgetR/wiki/Chemical-Translation-System-in-R).

