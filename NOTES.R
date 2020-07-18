#TODO 
# add local copy and build to dockerfile
#change rstudio server login from opencpu 


#from WSL
cd /mnt/c/Users/think/Dropbox/Software/ARCHIVE/CTSgetR


#build base image from opencpu


#build 
export rstudio_pass=dave_777 # rstudio server password for user opencpu
docker-compose -f docker-compose.yml build --force-rm


#start service
export ctsgetr_db_mount=/mnt/c/Users/think/Dropbox/Software/ARCHIVE/CTSgetR
docker-compose -f docker-compose.yml up -d

#interactive debug
docker exec -ti ctsgetr bash


#configure app armor
#http://bbroeksema.github.io/2014/11/20/using-sqlite-with-opencpu.html


#how to update the db?
#could use s3 to copy?




# R tests -----------------------------------------------------------------
#generic function to calculate and
#get results from opencpu
library(curl)
post_ocpu<-function(...){
  
  out<-list(results=NULL,error=NULL)
  
  res<-POST(...,encode='json',verbose())
  
  #error at API layer
  if(status_code(res) >=400){
    
    out['error']<-rawToChar(res$content)
    return(out)
  }
  
  # res_headers<-httr:::parse_http_headers(res$headers)[[1]]$headers # 
  res_headers<-headers(res)
  res_url<-res_headers$location
  
  #all endpoints
  locs<-readLines(curl(res_url))
  
  value<-'R/.val' # results
  error<-'console' # error message
  
  
  if(value %in% locs){
    
    tmp<-paste0(value,'/json') 
    tmp<-paste0(res_url,tmp)
    .name<-'results'
    
  } else {
    #R error
    tmp<-error
    tmp<-paste0(res_url,tmp)
    .name<-'error'
    
  }
  
  
  res<-fromJSON(readLines(curl(tmp)))
  
  if(!'list' %in% class(res)){
    out[[.name]]<-list(res)
  } else {
    out[[.name]]<-res
  }
  
  return(out)
  
  
}

#test API
library(CTSgetR)
library(httr)
library(jsonlite)


base_url<-'http://localhost/ocpu/'

#debug
endpoint<-'library/CTSgetR/R/api_debug'
url<-paste0(base_url,endpoint)

post_ocpu(url=url)

#db 
db_name<-'/ctsgetr/inst/ctsgetr.sqlite'
endpoint<-'library/CTSgetR/R/db_stats'
url<-paste0(base_url,endpoint)
body<-list(data=FALSE, db_name=db_name)

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


#locking with a mounted db?
db_name<-'/ctsgetr/inst/ctsgetr.sqlite'

file.exists(db_name)
do.call('db_stats',list(db_name='/ctsgetrdb/ctsger.sqlite'))
db_stats(db_name=db_name1)

#tests
# endpoint<-'library/CTSgetR/R/api_echo'
endpoint<-'library/CTSgetR/R/CTSgetR'

url<-paste0(base_url,endpoint)


id <-
  c(
    "C15973",
    "C00026"
  )
from <- "KEGG"
to <- "PubChem CID"

#tests in R
body<-list(id=id,from=from,to=to)
body<-list(id=id,from=from,to=to,db_name='inst/ctsgetr.sqlite')
do.call('CTSgetR',body)

do.call('CTSgetR::db_stats',list())


#API
db_name<-'/ctsgetrdb/ctsgetr.sqlite'

db_fun<-function(){
  Sys.getenv('ctsgetr_db')
}


body<-list(id=id,from=from,to=to)

#client db path
body<-list(id=id,from=from,to=to,db_name=db_name)


#server db
body<-list(id=id,from=from,to=to,db_name=db_fun())

endpoint<-'library/CTSgetR/R/CTSgetR'
url<-paste0(base_url,endpoint)

post_ocpu(url=url,body=body)

#OLDER

(x<-do.call('api_echo',list(toJSON(body))))
str(x)
fromJSON(x)

curl(val)

res<-POST(url,body=body,encode='json',verbose())

res_headers<-httr:::parse_http_headers(res$headers)[[1]]$headers
res_url<-res_headers$location

locs<-readLines(curl(res_url))

all_out<-lapply(locs,function(i){
  
  url<-paste0(res_url,i)
  
  readLines(curl(url))
  
})

names(all_out)<-locs

all_out

tmp<-paste0(res_url,'R/.val')
(results<-fromJSON(readLines(curl(tmp))))

tmp<-paste0(res_url,'R/console')
(results<-fromJSON(readLines(curl(tmp))))



results<-GET(res_url)

(out<-cat(rawToChar(results$content)))

#get location
rawToChar(headers(res))


# fromJSON(readLines(rawToChar(results$content)))


GET('http://localhost:4000/ocpu/tmp/x02060018908b3d/R/.val')
