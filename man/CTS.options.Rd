\name{CTS.options}
\alias{CTS.options}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get names of databases available for translation
}
\description{
View a list of >200 available databases for translations including: Chemical Name, InChIKey, InChI Code, PubChem CID, Pubchem SID, ChemDB, ChemSpider, Golm Metabolome Database (GMD), BioCyc, ChEBI, CAS, HMDB, KEGG, LipidMAPS.
}
\usage{
CTS.options()
}
%- maybe also 'usage' for other objects documented here.

\details{
Use returned values as input for \code{to} and \code{from} for \code{\link[CTSgetR]{CTSgetR}} and \code{\link[CTSgetR]{multi.CTSgetR}}.%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of available database names.
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
\code{\link[CTSgetR]{CTSgetR}}, \code{\link[CTSgetR]{multi.CTSgetR}}
}
\examples{
CTS.options()
}