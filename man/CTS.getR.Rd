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
One or many metabolite identifiers (\code{id}) of a single database type (\code{from}) are translated to a single database identifier (\code{to}). For one to many translations see \code{\link{multi.CTSgetR}}.
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
#translate from PubChem CID to InChIKey
id<-c(14242, 5760)
from<-"PubChem CID"
to<-"InChIKey"
CTSgetR(id,from,to)

#translate from InchI to pubchem CID
id<- "ZPUCINDJVBIVPJ-LJISPDSOSA-N"
from<-"InChIKey"
to<-"PubChem CID"
CTSgetR(id,from,to)

#translate from pubchem CID many chemical name synonyms
id<- 5950
from<-"PubChem CID"
to<-"Chemical Name"
CTSgetR(id,from,to, limit.values=FALSE) 
}


