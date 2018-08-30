SingHer	<- function( x, y, M, constrain=F, quantnorm=F ){

	if( quantnorm ){
		ranks			<- rank(y)
		quantiles <- qnorm(ranks/(length(ranks) + 1))
		y					<- quantiles 
	}

	N	<- length(x)
	resy	<- resid( lm( y ~ 1 + x ) )
	out		<- eigen_lmm( yprime=resy, Lam.K=x, constrained=constrain )
	sig2g	<- out$sig2g
	sig2e	<- out$sig2e

	lm_out<- summary( lm( y ~ 1+x, weights=1/(sig2g*x+sig2e) ) )$coef[2,1:2]
	mu		<- lm_out[1]
	sd.mu	<- lm_out[2]

	h2			<- h2_fxn( sig2g, sig2e, x, mu	, sd.mu=T )
	h2rand	<- h2_fxn( sig2g, sig2e, x, mu=0, sd.mu=F )
	h2mu		<- h2_fxn( sig2g, sig2e, x, mu	, sd.mu=NA, muonly=T )

	list( h2=h2, mu=mu, sig2g=sig2g/(N-2), sig2e=sig2e, h2mu=h2mu, h2rand=h2rand, M=M, N=N, sd.mu=sd.mu )
}
