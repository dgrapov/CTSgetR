\name{CTS.options}
\alias{CTS.options}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Translate between multiple chemical identifiers.
}
\description{
View a list of >200 available databases for translations including: Chemical Name, InChIKey, InChI Code, PubChem CID, Pubchem SID, ChemDB, ChemSpider, Golm Metabolome Database (GMD), BioCyc, ChEBI, CAS, HMDB, KEGG, LipidMAPS.
}
\usage{
CTS.options()
}
%- maybe also 'usage' for other objects documented here.

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a vector of available database names
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
CTSgetR, multi.CTSgetR
}
\examples{
CTS.options()
}