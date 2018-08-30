h2_fxn	<- function( sig2g, sig2e, x, mu=NA, sd.mu=NA, muonly=F ){
	N0	<- length(x)-2
	if( is.na( mu ) )
		return( (sig2g/N0)/(sig2g/N0 + sig2e) )
	if( is.na(sd.mu) ){
		mu2_vx	<- mu^2*var(x)
	} else {
		mu2_vx	<- (mu^2+sd.mu^2)*var(x)
	}
	vY	<- mu2_vx + sig2g/N0 + sig2e
	if( muonly ){
		return( (mu2_vx            )/vY )
	} else {
		return( (mu2_vx + sig2g/N0 )/vY )
	} 
}
