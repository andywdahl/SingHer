\name{eigen_lmm}

\alias{eigen_lmm}
\alias{eigen_lmm_obj}

\title{Whitened Linear Mixed Model Solver}
\description{Standard algorithm to fit linear mixed models with one variance component assuming upstream eigendecomposition and rotation.}

\usage{
	eigen_lmm(yprime, Lam.K, constrained = TRUE)
	eigen_lmm_obj(delta, yprime, Lam.K)
}

\arguments{
  \item{yprime}{A vector of independent phenotypes}
  \item{Lam.K}{A vector of kinship eigenvalues used to define entry-specific variances for yprime}
  \item{constrained}{Whether to enforce sig2g in (0,1)}
  \item{delta}{The signal-to-noise ratio, sig2e/sig2g}
}
\value{

eigen_lmm returns the variance components of heritability:
  \item{h2}{Estimated heritability of y due to the eigenvalues Lam.K}
  \item{sig2g}{Estimated variance of y due to the eigenvalues Lam.K}
  \item{sig2e}{Estimated residual variance in y}

eigen_lmm_obj returns the likelihood of a particular delta, which is a sufficient statistic after profiling out the total variance of yprime.

}

\author{Andy Dahl}

\details{
eigen_lmm optimizes the likelihood assuming that yprime ~ N( 0, sig2g*Lam.K + sig2e ). By assuming the eigendecomposition and whitening is pre-computed, the algorithm runs in O(N) time and space.
The search is performed over sig2g in (0,1) by default; without constraints, the search is performed over the entire feasible range (-1/max(Lam.K),Inf).
}
