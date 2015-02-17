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
  actionvector <- networktrace$action %in% c("+", "r", "d")
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

traceExp <- function(expfilename) {
  minbandwidth = 1.0
  maxbandwidth = 3.0
  bandwidthstep = 1.0
  mindelay = 10
  maxdelay = 100
  delaystep = 45
  minqueue = 70
  maxqueue = 100
  queuestep = 30
  protocol = c("reno", "trickles")
  expbw <- c()
  expdel <- c()
  expq <- c()
  expprot <- c()
  expperf <- c()
  expdrops <- c()
  for (bw in seq(minbandwidth, maxbandwidth, by=bandwidthstep)) {
    for (del in seq(mindelay, maxdelay, by=delaystep)) {
      for (q in seq(minqueue, maxqueue, by=queuestep)) {
        for (prot in protocol) {
          tracename <- paste0(prot,".", toString(bw),".",toString(del), ".", toString(q),".tr")
          args <- paste0("--run \"scratch/", expfilename," --bandwidth=", toString(bw),"Mbps --delay=", toString(del),"ms --queue=", toString(q)," --protocol=", prot, " --duration=20 --trace=",tracename,"\"")
          res <- system2("./waf", args, stdout=TRUE)
          expbw <- c(expbw, bw)
          expdel <- c(expdel, del)
          expq <- c(expq, q)
          expprot <- c(expprot, prot)
          if (is.null(attr(res, "status"))) {
            expperf <- c(expperf, as.numeric(unlist(strsplit(res[5], " ", fixed=TRUE))[4]))
            exptrace <- genericTraceReader(tracename, parsers=list(eventParser()))
            expdrops <- c(expdrops, nrow(exptrace[exptrace$action=="d",]))
          } else {
            expperf <- c(expperf, NA)
            expdrops <- c(expdrops, NA)
          }
        }
      }
    }
  }
  resdf <- data.frame(expbw, expdel, expq, expprot, expperf, expdrops)
  names(resdf) <- c("bandwidth", "delay", "queue", "protocol", "performance", "drops")
  resdf
}

plotres <- function(expres) {
  mf_labeller <- function(var, value) {
    value <- as.character(value)
    if (var=="delay") {
      value <- paste(value, "ms")
    }
    if (var=="bandwidth") {
      value <- paste(value, "Mb")
    }
    return(value)
  }
  
  tplot <- ggplot(expres, aes(x=queue, y=performance/1000000, group=protocol))+
    geom_point(aes(shape=protocol, colour=protocol), size=2)
  tplot <- tplot + scale_y_continuous(limits=c(0,8),breaks=seq(0,8,1))
  tplot <- tplot + ylab("Performance, MB")+xlab("Queue size, packets")
  tplot <- tplot + facet_grid(delay~bandwidth, labeller=mf_labeller)
  tplot <- tplot+theme_bw()+theme(legend.title=element_blank(), legend.text=element_text(face="bold"),legend.key = element_blank())
  tplot <- tplot+scale_colour_discrete(labels=c("TCP Reno", "Trickles"))+
    scale_shape_discrete(labels=c("TCP Reno", "Trickles"))+
    theme(legend.position="bottom")
  tplot
}
