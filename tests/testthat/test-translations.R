#tests
context("CTS translation")

#in
id<-c("C15973","C00026","C05381","C15972","C00091","C00042","C05379","C00311","C00036","C00024","C00149","C00417","C00158","C00022","C05125","C16254","C00122","C16255","C00074")
from<-"KEGG" 
to<-"PubChem CID"

#expected result
res<-structure(list(fromIdentifier = structure(c(1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "KEGG", class = "factor"), 
searchTerm = structure(1:19, .Label = c("C15973", "C00026", 
"C05381", "C15972", "C00091", "C00042", "C05379", "C00311", 
"C00036", "C00024", "C00149", "C00417", "C00158", "C00022", 
"C05125", "C16254", "C00122", "C16255", "C00074"), class = "factor"), 
toIdentifier = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "PubChem CID", class = "factor"), 
value = structure(c(1L, 2L, 3L, 1L, 4L, 5L, 7L, 8L, 9L, 10L, 
11L, 12L, 13L, 16L, 17L, 1L, 18L, 1L, 20L), .Label = c("", 
"51", "440649", "92133", "1110", "21952380", "972", "1198", 
"970", "444493", "222656", "643757", "19782904", "311", "88113319", 
"1060", "440568", "21883788", "444972", "1005", "58114173", 
"59658623"), class = "factor")), .Names = c("fromIdentifier", 
"searchTerm", "toIdentifier", "value"), row.names = c(NA, 19L
), class = "data.frame")

test_that("one to one translation", {
  expect_equal(CTSgetR(id,from,to), res)
})