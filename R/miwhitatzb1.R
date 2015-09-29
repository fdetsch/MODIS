# Author: A.Lobo 2012 Agustin.Lobo@ictja.csic.es
# Date : August 2012
# Licence GPL v3

# slightly modified by Matteo Mattiuzzi

miwhitatzb2 <- function(orgTS,w=NULL, l=10, f=2, minval=-3000, maxval=10000, minlength=80, maxiter=3, ver=F)
{
  # Modified Whittaker smoother
  # according to Atzberger & Eilers 2011 International Journal of Digital Earth 4(5):365-386.
  # A.Lobo 2012 Agustin.Lobo@ictja.csic.es
  # Input from Clement Atzberger <clement.atzberger@boku.ac.at>
  # see whittaker_smoother.m 
  
#       orgTS  	    : the time series to be filtered. It is assumed that the
#                    observations are equally spaced. Missing values and
#                    flag values should be either coded as NA or assigned
#                    a value < minVAL or > maxVAL
#
#       l	        : smoothing parameter. Allowed are (non-integer) 
#                    values > 0.
#
#		f           : differences for opimization in filter (i.e. f=2)
#
#       minval      : scalar, indicating which values in the input time
#                    series are valid (i.e. orgTS >= minval),
#                    respectively, not valid (i.e. orgTS < minval). The
#                    value of [minVAL] must be specified in the scaling of
#                    the input time series [orgTS]. If the smoother
#                    yields an output < minVAL, this element is set to
#                    minval
# 
#       maxval      : scalar, indicating which values in the input time
#                    series are valid (i.e. orgTS <= maxval),
#                    respectively, not valid (i.e. orgTS > maxval). The
#                    value of [maxVAL] must be specified in the scaling of
#                    the input time series [orgTS]. If the smoother
#                    yields an output > maxVAL, this element is set to
#                    maxval
#
#       minlength   : minimum number of valid observations in [orgTS] for
#                    that a filtering is performed. Not valid observations
#                    are those (1) value = NA, (2) value < minval, or (3)
#                    value > maxval. [minlength] should be a scalar
#                    (usually an integer). If the number of valid
#                    observations is lower than the specified minimum
#                    requirement, [filteredTS] = [orgTS] ... that is no
#                    filtering is performed
#
#       maxiter     : scalar (integer) indicating the number of iterations
#                    to be performed. If maxiter = 1, only a least square
#                    fit is performed. If maxiter > 1, the upper envelope
#                    of the input time series [orgTS] is fitted
#  
##########################################################################
  
  # Replacement of not valid measurements by NA (.. this should also include
  # flag values)
  orgTS[orgTS > maxval] <- NA
  orgTS[orgTS < minval] <- NA

  # prepare weights
  if (is.null(w))
  {
    w <-  orgTS * 0 + 1
  } 
  nas <- is.na(orgTS)
  w[nas]     <- 0 
  orgTS[nas] <- 0 # whit1() does not accept NAs !!! but value will be ignored because of miw
  
  for(i in 1:maxiter)
  {
    filteredTS <- whit2(orgTS,w=w,lambda=l)
    #The modification according to whittaker_smoother.m:
    #"Finds x positions were measured values are lower than the fitted
    #values. These positions are than set to the fitted values at these places"
    orgTS[orgTS<filteredTS] <- filteredTS[orgTS<filteredTS]
  } 

	# All filtered values lower than minval or larger than maxval are set to
	# these limits
	filteredTS[filteredTS > maxval & !is.na(filteredTS)] = maxval
	filteredTS[filteredTS < minval & !is.na(filteredTS)] = minval
	
	return(filteredTS)
}
  
miwhitatzb1 <- function(orgTS,w=NULL, l=10, f=2, minval=-3000, maxval=10000, minlength=80, maxiter=3, ver=F)
{
# Modified Whittaker smoother
# according to Atzberger & Eilers 2011 International Journal of Digital Earth 4(5):365-386.
# A.Lobo 2012 Agustin.Lobo@ictja.csic.es
# Input from Clement Atzberger <clement.atzberger@boku.ac.at>
# see whittaker_smoother.m 
########################################################################
#
#       orgTS  	    : the time series to be filtered. It is assumed that the
#                    observations are equally spaced. Missing values and
#                    flag values should be either coded as NA or assigned
#                    a value < minVAL or > maxVAL
#
#       l           : smoothing parameter. Allowed are (non-integer) 
#                    values > 0.
#
#		    f           : differences for opimization in filter (i.e. f=2)
#
#       minval      : scalar, indicating which values in the input time
#                    series are valid (i.e. orgTS >= minval),
#                    respectively, not valid (i.e. orgTS < minval). The
#                    value of [minVAL] must be specified in the scaling of
#                    the input time series [orgTS]. If the smoother
#                    yields an output < minVAL, this element is set to
#                    minval
# 
#       maxval      : scalar, indicating which values in the input time
#                    series are valid (i.e. orgTS <= maxval),
#                    respectively, not valid (i.e. orgTS > maxval). The
#                    value of [maxVAL] must be specified in the scaling of
#                    the input time series [orgTS]. If the smoother
#                    yields an output > maxVAL, this element is set to
#                    maxval
#
#       minlength   : minimum number of valid observations in [orgTS] for
#                    that a filtering is performed. Not valid observations
#                    are those (1) value = NA, (2) value < minval, or (3)
#                    value > maxval. [minlength] should be a scalar
#                    (usually an integer). If the number of valid
#                    observations is lower than the specified minimum
#                    requirement, [filteredTS] = [orgTS] ... that is no
#                    filtering is performed
#
#       maxiter     : scalar (integer) indicating the number of iterations
#                    to be performed. If maxiter = 1, only a least square
#                    fit is performed. If maxiter > 1, the upper envelope
#                    of the input time series [orgTS] is fitted
#  
##########################################################################
  
  # Replacement of not valid measurements by NA (.. this should also include
  # flag values)
  orgTS[orgTS > maxval] <- NA
	orgTS[orgTS < minval] <- NA
  
  # prepare weights
  if (is.null(w))
  {
    w <- orgTS * 0 + 1
  } 
  nas        <- is.na(orgTS)
  w[nas]     <- 0 
  orgTS[nas] <- 0 # whit1() does not accept NAs !!! but value will be ignored because of miw
  
  for(i in 1:maxiter)
  {
    filteredTS <- whit1(orgTS,w=w,lambda=l)
    #The modification according to whittaker_smoother.m:
    #"Finds x positions were measured values are lower than the fitted
    #values. These positions are than set to the fitted values at these places"
    orgTS[orgTS<filteredTS] <- filteredTS[orgTS<filteredTS]
  }
  
	# All filtered values lower than minval or larger than maxval are set to
	# these limits
	filteredTS[filteredTS > maxval & !is.na(filteredTS)] = maxval
	filteredTS[filteredTS < minval & !is.na(filteredTS)] = minval
	
	return(filteredTS)
}
  
  
  
