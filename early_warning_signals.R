# function to calculate changes in AR(1)

ar1_func <- function(ts, bw='default', wl='default') {
	# inputs
	# ts - time series to calculate EWS on
	# bw - kernal smoothing bandwidth, default is to not detrend
	# wl - window length used to calculate EWS on, default is half the length of the ts
	
	l <- length(ts)
	
	if (bw != 'default') {
		stopifnot(is.numeric(bw))
		detrended <- ts - ksmooth(1:l, ts, bandwidth=bw, x.out=1:l)$y
	} else {
		detrended <- ts
	}
	
	if (wl == 'default') {
		wl <- floor(length(ts)/2)
	}
	
	stopifnot(is.numeric(wl))
	
	ar1 <- rep(NA, l-wl+1)
	for (i in 1:(l-wl+1)) {
		ar1[i] <- ar.ols(detrended[i:(i+wl-1)], aic=FALSE, order.max=1)$ar
	}

	return(ar1)
}