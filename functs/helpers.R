# -----------------
# Helper Functions
# -----------------

print("Loading helper functions")

# Exponential and Logarithmic Transformations
expit <- function(x) exp(x)/(1+exp(x))

logit <- function(x) log(x/(1-x))

# Functions Defining Contrasts of Interests and Inverting those Contrasts
RDFunc <- function(a,b) a-b
RDInvApply <- function(out,effect) out-effect
logRRFunc <- function(a,b) log(a/b)
logRRInvApply <- function(out,effect) out*exp(-1*effect)
logORFunc <- function(a,b) log(a*(1-b)/((1-a)*b))
logORInvApply <- function(out,effect) expit(-1*effect+log(out/(1-out)))

# Create a vector with each cluster's first intervention period, by observation
StartTimeVec <- function(Periods, Clusters, Trts) {
  StartTimes <- tapply(Periods[Trts==1], Clusters[Trts==1], FUN=min)
  NumPds <- tapply(Periods, Clusters, FUN=length)
  return(rep(as.vector(StartTimes), times=NumPds))
}