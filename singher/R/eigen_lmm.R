eigen_lmm	<- function (yprime, Lam.K, constrained = TRUE){

	if (constrained) {
		delta			<- optimise(eigen_lmm_obj, c(0, 1e5), maximum = TRUE, Lam.K=Lam.K, yprime=yprime)$maximum
	} else {

		### these parameters correspond to h^2>0, with h^2>1 for delta<0
		opt_lims_plus <- c(-min(Lam.K), 1e5)
		opt_plus <- optimise(eigen_lmm_obj,opt_lims_plus, maximum = TRUE, Lam.K=Lam.K, yprime=yprime)

		### these parameters correspond to h^2<0
		opt_lims_minus <- c(-1000, -max(Lam.K))
		opt_minus <- optimise(eigen_lmm_obj, opt_lims_minus, maximum = TRUE, Lam.K=Lam.K, yprime=yprime)

		delta <- ifelse(opt_plus$objective > opt_minus$objective, opt_plus$maximum, opt_minus$maximum)
	}

	sig2g <- mean(yprime^2/(Lam.K + delta))
	sig2e <- delta * sig2g

	list(h2=sig2g/(sig2g+sig2e),sig2g=sig2g, sig2e=sig2e)
}

eigen_lmm_obj	<- function(delta, yprime, Lam.K){
	denominator <- Lam.K + delta
	sig2g <- mean(yprime^2/denominator)
	sum(dnorm( yprime, mean=0, sd=sqrt(sig2g*denominator), log=TRUE ))
}
