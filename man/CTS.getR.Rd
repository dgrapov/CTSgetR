\name{CTSgetR}
\alias{CTSgetR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
R interface for the Chemical Translation System
}
\description{
Tools to translate identifiers between > 200 of the most common biological databases including: Chemical Name, InChIKey, PubChem CID, ChemSpider, BioCyc, ChEBI, CAS, HMDB, KEGG and LipidMAPS.
}
\usage{
CTSgetR(id,from,to,parallel,async,limit.values,server)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
A vector of metabolite identifiers or names to translate. 
}
  \item{from}{
Database name describing \code{id}. See \code{\link{CTS.options}}.
}
  \item{to}{
Database name to translate \code{id} to. See \code{\link{CTS.options}}.
}
  \item{parallel}{
Parse results in parallel (defaults to \code{FALSE}).
}
\item{async}{
Carry out web query asynchronously (defaults to \code{FALSE}).
}
 \item{limit.values}{
Return a single value or a comma separated string of multiple values (defaults to \code{FALSE}).
}
\item{server}{
Web address for the Chemical Translation System web services (defaults to http://cts.fiehnlab.ucdavis.edu/service/convert).
}
}
\details{
One or many metabolite identifiers (\code{id}) of a single database type (\code{from}) are translated to a single database identifier (\code{to}). For one to many translations see \code{\link[CTSgetR]{multi.CTSgetR}}.
}
\value{
A 2 column data frame containing from and to translated values. Values not found in the database are returned as "error".
}
\references{
http://cts.fiehnlab.ucdavis.edu/
}
\author{
Dmitry Grapov
}
\note{
http://cts.fiehnlab.ucdavis.edu/
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{multi.CTSgetR}}, \code{\link{CTS.options}}
}
\examples{
#translate between KEGG and PubChem CIDs
id<-c("C15973","C00026","C05381","C15972","C00091","C00042","C05379","C00311","C00036","C00024","C00149","C00417","C00158","C00022","C05125","C16254","C00122","C16255","C00074")
from<-"KEGG" 
to<-"PubChem CID"
CTSgetR(id,from,to)

#asynchronous query of CTS, for large queries
#use \code{async.limit} to not overwhelm the server
CTSgetR(id,from,to,async=TRUE,async.limit=100) 

#translate from InchiKey to chemical name and allow multiple return values
id <- c("QNAYBMKLOCPYGJ-REOHCLBHSA-N")
from <- "InChIKey"
to <- "Chemical Name"
CTSgetR(id,from,to,limit.values=FALSE)


}


