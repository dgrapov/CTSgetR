# SQLite functions -------------------------------------------------------
#source, source_id, target, target_id as columns
#alphabetical ordering on source/target 
#rowise all columns hash for update lookups

##TODO create a database of all KEGG,CID,names and inchi

#format API response to db structure (names)
db_transform<-function(obj){
  
  #match database
  #trying to detect column
  #directed i.e. don't need symmetrical queries from
  ord<-order(c(obj$from[1] %>% as.character(),obj$to[1] %>% as.character()))
  
  name<-c('source','target')[ord]
  id<-c('source_id','target_id')[ord]
  
  list(data=data.frame(obj$from %>% unlist(),obj$to %>% unlist(),obj$id %>% unlist(),obj$key %>% unlist()) %>%
         setNames(.,c(name,id)),inverted = ord[1]> ord[2]) 
  
}

#unique key
db_key<-function(obj){
  
  #this assumes data is normalized 
  #for db structure already
  paste(obj$source,obj$target,obj$source_id,obj$target_id, sep='_')
  
}

#check if key exists in db
#and add if absent else add methods for upsert
db_add<-function(id, from, to, key,db_name='inst/ctsgetr.sqlite',upsert=FALSE,verbose=FALSE){
  
  mydb <- tryCatch(dbConnect(RSQLite::SQLite(), db_name),error=function(e){})
  
  if(is.null(mydb)){stop('No database found!')}
  
  
  #format input
  input <- data.frame(id, from, to,key) %>% 
    db_transform(.) %>% .$data %>%
    mutate(key_index=db_key(.))
  
  #check if values exist
  #if so do not replace
  if(!upsert){
    query<-"SELECT EXISTS(SELECT 1 FROM CTSgetR WHERE key_index=:key_index);"
    params<-list(key_index=input$key_index %>% as.character()) # all exist
    old<-dbGetQuery(mydb, query,params) %>% unlist() %>% as.logical()
    
    input<-input[!old,]
    
  }
  
  #add
  if(nrow(input) > 0){
    dbWriteTable(mydb, "CTSgetR", input, append=TRUE)
  } else {
    
    if(verbose){ 
      print('All values are already in the database, use upsert=TRUE to replace.')
    }
    
  }
  
  DBI::dbDisconnect(mydb)
  
}

db_get <-function(id, from, to, db_name='inst/ctsgetr.sqlite'){
  
  mydb <- tryCatch(dbConnect(RSQLite::SQLite(), db_name),error=function(e){})
  
  if(is.null(mydb)){print('No database found!');return()}
  
  
  #format input
  input <- data.frame(id, from, to,key=NA) %>% db_transform()
  invert<-!input$inverted
  input<-input$data
  
  
  if(invert){
    query <- "SELECT * FROM CTSgetR
  WHERE
  (
  (source = :source and target = :target)
  AND
  (source_id = :source_id)
  )"
    
    pnames<-c('source','target','source_id')
    
  } else{
    query <- "SELECT * FROM CTSgetR
  WHERE
  (
  (source = :source and target = :target)
  AND
  (target_id = :target_id)
  )"
    
    pnames<-c('source','target','target_id')
  }
  
  
  params <-input[pnames] %>% as.list()
  
  
  res<-dbGetQuery(mydb, query,params) 
  
  #format as API results
  if(invert){
    vars <- c("id"='source_id' , 'from'='source', "key"='target_id' , 'to' = 'target')
    
  } else {
    vars <- c("id"='target_id' , 'from'='target', "key"='source_id' , 'to' = 'source')
  }
  
  
  out<- res %>%
    rename(., !!vars) %>%
    select(-key_index) %>%
    .[c('id','from','to','key')]
  
  DBI::dbDisconnect(mydb)
  
  return(out)
}

#get edge list stats
#' @export
#' @import RSQLite
#' @param data logical wether to return the database data as a data.frame, defaults to FALSE 
#' @param db_name name of sqlite database
db_stats<-function(data=FALSE,db_name='inst/ctsgetr.sqlite'){
  
  mydb <- dbConnect(RSQLite::SQLite(), db_name)
  query<-"SELECT * FROM CTSgetR"
  res<-dbGetQuery(mydb, query)
  DBI::dbDisconnect(mydb)
  
  x<-paste(as.character(res$source),as.character(res$target),sep=' <--> ')
  if(data){
    res
  } else {
    list(data.frame(table(x)) %>% setNames(.,c('translation','n')),total=length(x))
  }
  
}

#' @export
#' @param db_name string sqlite database name
#' @import RSQLite
init_CTSgetR_db<-function(db_name='inst/ctsgetr.sqlite'){
  
  x<-structure(list(source = structure(c(1L, 1L, 1L), .Label = "InChIKey", class = "factor"), 
                    target = structure(c(1L, 1L, 1L), .Label = "PubChem CID", class = "factor"), 
                    source_id = structure(1:3, .Label = c("JVTAAEKCZFNVCJ-UHFFFAOYSA-N", 
                                                          "BWLBGMIXKSTLSX-UHFFFAOYSA-N", "AEMRFAOFKBGASW-UHFFFAOYSA-N"
                    ), class = "factor"), target_id = structure(1:3, .Label = c("19789253", 
                                                                                "11671", "3698251"), class = "factor"), key_index = c("InChIKey_PubChem CID_JVTAAEKCZFNVCJ-UHFFFAOYSA-N_19789253", 
                                                                                                                                      "InChIKey_PubChem CID_BWLBGMIXKSTLSX-UHFFFAOYSA-N_11671", 
                                                                                                                                      "InChIKey_PubChem CID_AEMRFAOFKBGASW-UHFFFAOYSA-N_3698251"
                                                                                )), class = "data.frame", row.names = c(NA, -3L))
  mydb <- dbConnect(RSQLite::SQLite(), db_name)
  dbWriteTable(mydb, "CTSgetR", x, overwrite=TRUE)
}

