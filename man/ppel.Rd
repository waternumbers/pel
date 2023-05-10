\name{PEL_TODO}
\alias{dpel}
\alias{ppel}
\alias{qpel}
\alias{rpel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ppel
%%  ~~function to do ... ~~
}
\description{
  Density, distribution function, quantile function and random
  generation for the normal distribution with mean equal to ‘mean’
  and standard deviation equal to ‘sd’.
}
\usage{
dpel(x, a, q, v, log=FALSE)
ppel(x, a, q, v, lower.tail = TRUE, log.p = FALSE)
qpel(u, a, q, v, lower.tail = TRUE, log.p = FALSE)
rpel(n, a, q, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector of quantiles.}
  \item{u}{vector of probabilities.}
  \item{n}{number of observations. If ‘length(n) > 1’, the length is
          taken to be the number required.}
  \item{a}{ TODO }
  \item{q}{ TODO }
  \item{v}{ TODO }
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
     ‘dnorm’ gives the density, ‘pnorm’ gives the distribution
     function, ‘qnorm’ gives the quantile function, and ‘rnorm’
     generates random deviates.

     The length of the result is determined by ‘n’ for ‘rnorm’, and is
     the maximum of the lengths of the numerical arguments for the
     other functions.

     The numerical arguments other than ‘n’ are recycled to the length
     of the result.  Only the first elements of the logical arguments
     are used.

     For ‘sd = 0’ this gives the limit as ‘sd’ decreases to 0, a point
     mass at ‘mu’.  ‘sd < 0’ is an error and returns ‘NaN’.
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
x <- rpel(4,1,2,5)
dx <- dpel(x,1,2,5)
Dx <- ppel(x,1,2,5)
xx <- qpel(Dx,1,2,5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
