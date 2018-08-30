\name{h2_fxn}

\alias{h2_fxn}

\title{Heritability identities}
\description{Helper code to evaulate heritability in the presence of non-mean-zero genetic effect sizes.}

\usage{
	h2_fxn( sig2g, sig2e, x, mu=NA, sd.mu=NA, muonly=F )
}

\arguments{
  \item{sig2g, sig2e}{Estimated variance components}
  \item{x}{Vector of singleton fractions}
  \item{mu,sd.mu}{Estimated mean genetic effect and its standard error}
  \item{muonly}{Whether to include sig2g in the heritability estimate}
}
\value{Estimated heritability}

\author{Andy Dahl}
\note{h2 estimates excluding the mean effect are obtained when mu and sd.mu are NA.}
\details{h2_fxn is a simple way to evaluate the heritability implied by an estimated parameter set.}
\examples{
	N	<- 100

	x	<- rpois( 100, 10 )
	M	<- sum(x)
	x <- x/M


	h2_fxn( 1, 1, x, NA, NA )
	h2_fxn( 1, 1, x, 1, 1 )
	h2_fxn( 1, 1, x, 1, 1, muonly=TRUE )
	h2_fxn( 0, 1, x, 1, 1 )
}
