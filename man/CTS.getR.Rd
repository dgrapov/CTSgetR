\name{CTS.getR}
\alias{CTS.getR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CTS.getR(id, from, to, progress = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{from}{
%%     ~~Describe \code{from} here~~
}
  \item{to}{
%%     ~~Describe \code{to} here~~
}
  \item{progress}{
%%     ~~Describe \code{progress} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (id, from, to, progress = TRUE) 
{
    if (require(RCurl) == FALSE) {
        install.packages("RCurl")
        library(RCurl)
    }
    else {
        library(RCurl)
    }
    if (require(RJSONIO) == FALSE) {
        install.packages("RJSONIO")
        library(RJSONIO)
    }
    else {
        library(RJSONIO)
    }
    if (progress == TRUE) {
        pb <- txtProgressBar(min = 0, max = length(id), style = 3)
    }
    start <- "http://uranus.fiehnlab.ucdavis.edu:8080/cts-2.0-beta-2/service/convert"
    out <- lapply(1:length(id), function(i) {
        if (progress == TRUE) {
            setTxtProgressBar(pb, i)
        }
        url <- paste(start, from, to, id[i], sep = "/")
        url <- gsub("\\ ", "\%20", url)
        out <- as.character(unlist(tryCatch(getURL(url, ssl.verifypeer = FALSE), 
            error = function(e) {
                "error"
            })))
        tryCatch(data.frame(fromJSON(out))[, 1, drop = FALSE], 
            error = function(e) {
                data.frame(matrix("error", nrow = 3))
            })
    })
    tmp <- do.call("cbind", out)
    tmp <- cbind(from = id, to = to, result = as.character(unlist(tmp[4, 
        ])))
    colnames(tmp) <- c(paste("from", from, sep = " :"), "to:", 
        "RESULT")
    if (progress == TRUE) {
        close(pb)
    }
    return(tmp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
