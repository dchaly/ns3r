# Functions for ns-3 trace analysis.
# (c) 2014-2015 Dmitry Ju. Chalyy
#
# The function calculates the network load which is made by certain endpoints. 
# If endpoint emits the packet the network load increases by the byte length of the packet.
# If endpoint reads the packet from network the network load decreases.
# Parameters: 
# * networktrace - a data frame which is formed by trace-reader.R function
# * endpoints - a list of endpoints which are taken into account
simpleNetworkLoad <- function(networktrace, endpoints=c()) {
  timevector <- c()
  loadvector <- c()
  byteloadvector <- c()
  actionvector <- networktrace$action %in% c("+", "r")
  nodevector <- networktrace$node %in% endpoints
  actions <- sapply(networktrace[actionvector & nodevector, 1], function(x) if (x=="+") return(1) else return(-1))
  times <- networktrace[actionvector & nodevector, 2]
  bytesidx <- grep("IPLength", names(networktrace), fixed=TRUE)
  prevtime <- 0.0
  curload <- 0
  curbytes <- 0
  if (!identical(bytesidx, integer(0))) bytes <- networktrace[actionvector & nodevector, bytesidx] else bytes <- NULL
  for (i in 1:length(times)) {
    if (prevtime != times[i]) {
      timevector <- c(timevector, prevtime)
      loadvector <- c(loadvector, curload)
      if (!is.null(bytes)) byteloadvector <- c(byteloadvector, curbytes)
      timevector <- c(timevector, times[i])
      loadvector <- c(loadvector, curload)
      if (!is.null(bytes)) byteloadvector <- c(byteloadvector, curbytes)
      prevtime <- times[i]
    }
    curload = curload + actions[i]
    if (!is.null(bytes)) curbytes = curbytes + actions[i]*bytes[i]
  }
  df <- data.frame(timevector, loadvector)
  if (!is.null(bytes)) df <- cbind(df, byteloadvector)
  df
}
