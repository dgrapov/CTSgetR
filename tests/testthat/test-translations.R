#skip_on_cran(tests
context("CTS translation")

#test agains local db or set to NULL to test API
db_name<-'../../inst/ctsgetr.sqlite'

print(getwd())
#in
id <-
  c(
    "C15973",
    "C00026",
    "C05381",
    "C15972",
    "C00091",
    "C00042",
    "C05379",
    "C00311",
    "C00036",
    "C00024",
    "C00149",
    "C00417",
    "C00158",
    "C00022",
    "C05125",
    "C16254",
    "C00122",
    "C16255",
    "C00074"
  )
from <- "KEGG"
to <- "PubChem CID"

#expected result
res<-structure(list(id = c("C15973", "C00026", "C05381", "C15972", 
                           "C00091", "C00042", "C05379", "C00311", "C00036", "C00024", "C00149", 
                           "C00417", "C00158", "C00022", "C05125", "C16254", "C00122", "C16255", 
                           "C00074"), from = c("KEGG", "KEGG", "KEGG", "KEGG", "KEGG", "KEGG", 
                                               "KEGG", "KEGG", "KEGG", "KEGG", "KEGG", "KEGG", "KEGG", "KEGG", 
                                               "KEGG", "KEGG", "KEGG", "KEGG", "KEGG"), to = c("PubChem CID", 
                                                                                               "PubChem CID", "PubChem CID", "PubChem CID", "PubChem CID", "PubChem CID", 
                                                                                               "PubChem CID", "PubChem CID", "PubChem CID", "PubChem CID", "PubChem CID", 
                                                                                               "PubChem CID", "PubChem CID", "PubChem CID", "PubChem CID", "PubChem CID", 
                                                                                               "PubChem CID", "PubChem CID", "PubChem CID"), key = c(NA, "51", 
                                                                                                                                                     "440649", NA, "92133", "1110", "972", "1198", "970", "444493", 
                                                                                                                                                     "222656", "643757", "19782904", "1060", "440568", NA, "21883788", 
                                                                                                                                                     NA, "1005")), row.names = c(NA, -19L), class = "data.frame")


test_that("one to one translation", {
  skip_on_cran()
  expect_equivalent(CTSgetR(id, from, to,db_name), res)
  # expect_equal(getwd(), 'foo')
})
