#tests
context("CTS one-to-many translation")

library(httr)

#in
#translate from "InChIKey" to multiple identifiers
id<-c("DMULVCHRPCFFGV-UHFFFAOYSA-N","ZPUCINDJVBIVPJ-LJISPDSOSA-N","ZAGRKAFMISFKIO-QMTHXVAHSA-N")
from<-"InChIKey"
to<- c("Chemical Name", "PubChem CID", "KEGG","Human Metabolome Database")


#expected result (could change with db) 5/22/16
res<-structure(list(fromIdentifier = structure(c(1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "InChIKey", class = "factor"), 
searchTerm = structure(c(1L, 2L, 2L, 3L, 3L, 1L, 2L, 3L, 
3L, 1L, 2L, 3L, 1L, 2L, 3L), .Label = c("DMULVCHRPCFFGV-UHFFFAOYSA-N", 
"ZPUCINDJVBIVPJ-LJISPDSOSA-N", "ZAGRKAFMISFKIO-QMTHXVAHSA-N"
), class = "factor"), toIdentifier = structure(c(1L, 1L, 
1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L), .Label = c("Chemical Name", 
"PubChem CID", "KEGG", "Human Metabolome Database"), class = "factor"), 
value = structure(c(1L, 3L, 2L, 5L, 4L, 6L, 7L, 8L, 9L, 10L, 
11L, 12L, 13L, 12L, 12L), .Label = c("error", "8-Azabicyclo[3.2.1]octane-2-carboxylic acid, 3-(benzoyloxy)-8-methyl-, methyl ester, (1R,2R,3S,5S)-", 
"Methyl (1R,2R,3S,5S)-3-(benzoyloxy)-8-methyl-8-azabicyclo[3.2.1]octane-2-carboxylate", 
"(8beta)-6-Methyl-9,10-didehydroergoline-8-carboxylic acid", 
"Ergoline-8-carboxylic acid, 9,10-didehydro-6-methyl-, (8beta)-", 
"6089", "446220", "11861108", "6717", "C08302", "C01416", 
"", "HMDB05973"), class = "factor")), .Names = c("fromIdentifier", 
"searchTerm", "toIdentifier", "value"), row.names = c(NA, 15L
), class = "data.frame")

#unstable results based on CTS
test_that("one to many translations", {
    skip_on_cran()
    expect_equal(CTSgetR(id,from,to,limit.values=FALSE), res)
})