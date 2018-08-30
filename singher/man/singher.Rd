\name{SingHer}

\alias{SingHer}

\title{Singleton Heritability Estimation}
\description{SingHer estimates the heritability of a phenotype explained by singleton genetic variants.}

\usage{
SingHer( x, y, M, constrain, quantnorm=F )
}

\arguments{
  \item{x}{Singleton fraction vector}
  \item{y}{Quantitative phenotype}
  \item{M}{Total number of singletons}
  \item{constrain}{Whether to force sig2g in (0,1)}
  \item{quantnorm}{Whether to internally quantile normalize y before estimating heritability}
}
\value{

  \item{h2}{Estimated heritability of y due to the singletons x}

  \item{mu}{Estimated mean of the singleton effects on y}
  \item{sig2g}{Estimated variance of the singleton effects on y}

  \item{sig2e}{Estimated residual variance of y}

  \item{h2mu}{Estimated heritability of y due only to the average singleton effects}
  \item{h2rand}{Estimated heritability of y due only to the variance in singleton effects}
  \item{M}{Total number of singletons}
  \item{N}{Number of samples}

  \item{sd.mu}{Estimated standard error of mu}

}

\author{Andy Dahl}
\references{Hernandez et al. 2017, in preparation}
\note{Singher does not currently fit background genetic effects, i.e. the standard notion of heritability. sig2g is defined without normalizing singleton genotypes to variance 1; with normalization, the term is sig2g/(N-2).}
\details{SingHer estimates heritability in a standard REML framework. Unlike standard h2 estimates, SingHer accounts for the possibility that singleton effects may not be mean zero, eliminating one source of bias.}
\examples{

	x	<- rpois( 100, 10 )
	M	<- sum(x)
	x <- x/M

	y	<- .1*x + rnorm(100) 

	SingHer( x, y, M )
}
