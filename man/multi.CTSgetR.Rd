\name{multi.CTSgetR}
\alias{multi.CTSgetR}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Translate from one to many chemical identifiers.
}
\description{
Use the Chemical Translation System (CTS) to translate between > 200 of the most common biological database identifiers including: Chemical Name, InChIKey, InChI Code, PubChem CID, Pubchem SID, ChemDB, ChemSpider, Golm Metabolome Database (GMD), BioCyc, ChEBI, CAS, HMDB, KEGG, LipidMAPS.
}
\usage{
multi.CTSgetR(id,from,to,...)
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
  \item{...}{
See \code{\link{CTSgetR}} options. 
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame containing from and to translated values. Values not found in the database are returned as "".
}
\references{
http://cts.fiehnlab.ucdavis.edu/
}
\author{
Dmitry Grapov
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[CTSgetR]{CTSgetR}}
}
\examples{
id<-c(14242, 5760, 1175)
from<-"PubChem CID"
to<-c("InChIKey", "BioCyc","Human Metabolome Database", "KEGG")
multi.CTSgetR(id,from,to)

}
