\name{Lindley Distribution}
\alias{dlind}
\alias{plind}
\alias{qlind}
\alias{rlind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Lindley Distribution
%%  ~~function to do ... ~~
}
\description{
  Density, distribution function, quantile function and random
  generation for the Lindley distribution with ?? equal to theta.
}
\usage{
dlind(x, theta, log=FALSE)
plind(x, theta, lower.tail = TRUE, log.p = FALSE)
qlind(q, theta, lower.tail = TRUE, log.p = FALSE)
rlind(n, theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector of quantiles.}
  \item{q}{vector of probabilities.}
  \item{n}{number of observations. If ‘length(n) > 1’, the length is
          taken to be the number required.}
  \item{theta}{ TODO }
  \item{log, log.p}{logical; if TRUE, probabilities p are given as
    log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x]
          otherwise, P[X > x].}
}
\details{
     If ‘mean’ or ‘sd’ are not specified they assume the default values
     of ‘0’ and ‘1’, respectively.

     The normal distribution has density

            f(x) = 1/(sqrt(2 pi) sigma) e^-((x - mu)^2/(2 sigma^2))     
     
     where mu is the mean of the distribution and sigma the standard
     deviation.
}
\value{
     ‘dlind’ gives the density, ‘plind’ gives the distribution
     function, ‘qlind’ gives the quantile function, and ‘rlind’
     generates random deviates.

     The length of the result is determined by ‘n’ for ‘rlind’, and is
     the maximum of the lengths of the numerical arguments for the
     other functions.

     The numerical arguments other than ‘n’ are recycled to the length
     of the result.  Only the first elements of the logical arguments
     are used.

     %%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{ TODO 
%% ~put references to the literature/web site here ~
}
\author{ TODO
%%  ~~who you are~~
}
\note{ TODO
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  Distributions for other standard distributions, including
  \code{\link{norm}} for the normal distribution
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- rlind(4,5)
dx <- dlind(x,5)
Dx <- plind(x,5)
xx <- qlind(Dx,5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ distribution }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
