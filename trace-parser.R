# This file can be used to read ns-3 traces into R as data frames
# (c) 2014-2015 Dmitry Ju. Chalyy

# This function is a predicate which returns true if x represents a trace line
# with Trickles packet
tricklesShiehLines <- function(x) {
  return(!identical(grep("TricklesShieh", x, fixed=TRUE), integer(0)))
}

# This object contains elements which can parse event data
# from ns-3 trace.
# The event data consists of the following elements:
# action - +/i/r/d - operation occured on device queue
# time - time the event occured
# node - node where event occured
# interface - the interface of the node
eventParser <- function() {
  actions <- c()
  eventtimes <- c()
  nodes <- c()
  interfaces <- c()
  clearParseData <- function() {
    actions <<- c()
    eventtimes <<- c()
    nodes <<- c()
    interfaces <<- c()
  }
  # This element parses line consisting of packet headers
  eventParseHeader <- function(headers) {
    if (eventExists(headers)) {
      t <- unlist(strsplit(headers[1], " ", fixed=TRUE))
      actions <<- c(actions, t[1])
      eventtimes <<- c(eventtimes, as.numeric(t[2]))
      ni <- sub(".*\\/([0-9]+).*([0-9]+)\\/.*", "\\1 \\2", t[3])
      nodes <<- c(nodes, unlist(strsplit(ni, " ", fixed=TRUE))[1])
      interfaces <<- c(interfaces, unlist(strsplit(ni, " ", fixed=TRUE))[2])
    } else {
      actions <<- c(actions, NA)
      eventtimes <<- c(eventtimes, NA)
      nodes <<- c(nodes, NA)
      interfaces <<- c(interfaces, NA)
    }
  }
  # This element returns column names
  eventParseCols <- function() {
    c("action", "time", "node", "interface")
  }
  # This predicate returns true whether the event exists in the line
  # of course the considered event elements always must exist
  eventExists <- function(headers) {
    return(TRUE)
  }
  # This function returns actual data
  eventData <- function() {
    list(factor(actions), eventtimes, factor(nodes), factor(interfaces))
  }
  list(parser=eventParseHeader, clear=clearParseData, exists=eventExists, data=eventData, names=eventParseCols)
}

# The object parses IP headers. This is made in the same manner as parsing event information
ipv4Parser <- function() {
  ipsrc <- c()
  ipdst <- c()
  iplen <- c()
  clearParseData <- function() {
    ipsrc <<- c()
    ipdst <<- c()
    iplen <<- c()
  }
  ipv4ParseHeader <- function(headers) {
    if (ipv4Exists(headers)) {
      t <- unlist(strsplit(sub(".*length: ([0-9]+) ([0-9]+.[0-9]+.[0-9]+.[0-9]+) > ([0-9]+.[0-9]+.[0-9]+.[0-9]+).*", "\\1 \\2 \\3", headers[grep("^Ipv4Header", headers)]), " ", fixed=TRUE))
      iplen <<- c(iplen, as.numeric(t[1]))
      ipsrc <<- c(ipsrc, t[2])
      ipdst <<- c(ipdst, t[3])
    } else {
      iplen <<- c(actions, NA)
      ipsrc <<- c(eventtimes, NA)
      ipdst <<- c(nodes, NA)
    }
  }
  ipv4ParseCols <- function() {
    c("srcIP", "dstIP", "IPLength")
  }
  ipv4Exists <- function(headers) {
    return(!identical(grep("^Ipv4Header", headers), integer(0)))
  }
  ipv4Data <- function() {
    list(factor(ipsrc), factor(ipdst), iplen)
  }
  list(parser=ipv4ParseHeader, clear=clearParseData, exists=ipv4Exists, data=ipv4Data, names=ipv4ParseCols)
}

# This object parses TCP headers
tcpParser <- function() {
  tcpsrc <- c()
  tcpdst <- c()
  tcpseq <- c()
  tcpack <- c()
  tcpwin <- c()
  clearParseData <- function() {
    tcpsrc <<- c()
    tcpdst <<- c()
    tcpseq <<- c()
    tcpack <<- c()
    tcpwin <<- c()
  }
  tcpParseHeader <- function(headers) {
    if (tcpExists(headers)) {
      t <- unlist(strsplit(sub("TcpHeader.*\\(([0-9]+) > ([0-9]+) Seq=([0-9]+) Ack=([0-9]+) Win=([0-9]+).*", "\\1 \\2 \\3 \\4 \\5", headers[grep("^TcpHeader", headers)]), " ", fixed=TRUE))
      tcpsrc <<- c(tcpsrc, as.numeric(t[1]))
      tcpdst <<- c(tcpdst, as.numeric(t[2]))
      tcpseq <<- c(tcpseq, as.numeric(t[3]))
      tcpack <<- c(tcpack, as.numeric(t[4]))
      tcpwin <<- c(tcpwin, as.numeric(t[5]))
    } else {
      tcpsrc <<- c(tcpsrc, NA)
      tcpdst <<- c(tcpdst, NA)
      tcpseq <<- c(tcpseq, NA)
      tcpack <<- c(tcpack, NA)
      tcpwin <<- c(tcpwin, NA)
    }
  }
  tcpParseCols <- function() {
    c("srcPort", "dstPort", "seq", "ack", "win")
  }
  tcpExists <- function(headers) {
    return(!identical(grep("^TcpHeader", headers), integer(0)))
  }
  tcpData <- function() {
    list(tcpsrc, tcpdst, tcpseq, tcpack, tcpwin)
  }
  list(parser=tcpParseHeader, clear=clearParseData, exists=tcpExists, data=tcpData, names=tcpParseCols)
}

# This object parses Trickles header
tricklesParser <- function() {
  ptype <- c()
  trickleno <- c()
  parentno <- c()
  reqsize <- c()
  recovery <- c()
  tsval <- c()
  tsecr <- c()
  rtt <- c()
  sacks <- c()
  firstloss <- c()
  clearParseData <- function() {
    ptype <<- c()
    trickleno <<- c()
    parentno <<- c()
    reqsize <<- c()
    recovery <<- c()
    tsval <<- c()
    tsecr <<- c()
    rtt <<- c()
    sacks <<- c()
    firstloss <<- c()
  }
  tricklesParseHeader <- function(headers) {
    if (tricklesExists(headers)) {
      t <- unlist(strsplit(sub("TricklesHeader.*\\(([A-Z]+) Trickle=([0-9]+) Parent=([0-9]+) FirstLoss=([0-9]+) SACKS=(.+) Request=([0-9]+) Recovery=(.+) tsval=([0-9]+) tsecr=([0-9]+).*", "\\1 \\2 \\3 \\4 \\5 \\6 \\7 \\8 \\9", headers[grep("^TricklesHeader", headers)]), " ", fixed=TRUE))
      ptype <<- c(ptype, t[1])
      trickleno <<- c(trickleno, as.numeric(t[2]))
      parentno <<- c(parentno, as.numeric(t[3]))
      reqsize <<- c(reqsize, as.numeric(t[6]))
      recovery <<- c(recovery, t[7])
      tsval <<- c(tsval, as.numeric(t[8]))
      tsecr <<- c(tsecr, as.numeric(t[9]))
      sacks <<- c(sacks, t[5])
      firstloss <<- c(firstloss, as.numeric(t[4]))
      t <- sub("^TricklesHeader.*RTT=([0-9]+).*", "\\1", headers[grep("^TricklesHeader", headers)])
      rtt <<- c(rtt, as.numeric(t))
    } else {
      ptype <<- c(ptype, NA)
      trickleno <<- c(trickleno, NA)
      parentno <<- c(parentno, NA)
      reqsize <<- c(reqsize, NA)
      recovery <<- c(recovery, NA)
      tsval <<- c(tsval, NA)
      tsecr <<- c(tsecr, NA)
      rtt <<- c(rtt, NA)
      sacks <<- c(sacks, NA)
      firstloss <<- c(firstloss, NA)
    }
  }
  tricklesParseCols <- function() {
    c("packetType", "trickle", "parent", "requestsize", "recovery", "tsval", "tsecr", "rtt", "sack", "firstloss")
  }
  tricklesExists <- function(headers) {
    return(!identical(grep("^TricklesHeader", headers), integer(0)))
  }
  tricklesData <- function() {
    list(factor(ptype), trickleno, parentno, reqsize, factor(recovery), tsval, tsecr, rtt, sacks, firstloss)
  }
  list(parser=tricklesParseHeader, clear=clearParseData, exists=tricklesExists, data=tricklesData, names=tricklesParseCols)
}

# This object parses TricklesShieh header
tricklesShiehParser <- function() {
  tcpbase <- c()
  startcwnd <- c()
  ssthresh <- c()
  clearParseData <- function() {
    tcpbase <<- c()
    startcwnd <<- c()
    ssthresh <<- c()
  }
  tricklesShiehParseHeader <- function(headers) {
    if (tricklesShiehExists(headers)) {
      t <- unlist(strsplit(sub("TricklesShiehHeader.*\\(TCPBase=([0-9]+) startCwnd=([0-9]+) ssthresh=([0-9]+).*", "\\1 \\2 \\3", headers[grep("^TricklesShiehHeader", headers)]), " ", fixed=TRUE))
      tcpbase <<- c(tcpbase, as.numeric(t[1]))
      startcwnd <<- c(startcwnd, as.numeric(t[2]))
      ssthresh <<- c(ssthresh, as.numeric(t[3]))
    } else {
      tcpbase <<- c(tcpbase, NA)
      startcwnd <<- c(startcwnd, NA)
      ssthresh <<- c(ssthresh, NA)
    }
  }
  tricklesShiehParseCols <- function() {
    c("tcpbase", "startcwnd", "ssthresh")
  }
  tricklesShiehExists <- function(headers) {
    return(!identical(grep("^TricklesShiehHeader", headers), integer(0)))
  }
  tricklesShiehData <- function() {
    list(tcpbase, startcwnd, ssthresh)
  }
  list(parser=tricklesShiehParseHeader, clear=clearParseData, exists=tricklesShiehExists, data=tricklesShiehData, names=tricklesShiehParseCols)
}

# This object parses payload information
payloadParser <- function() {
  payload <- c()
  clearParseData <- function() {
    payload <<- c()
  }
  convertMultiple <- function(x) {
    tmp <- sub("\\(size=([0-9]+).*", "\\1", x)
    if (!identical(grep("^[0-9]+$", tmp), integer(0))) {
      return(as.numeric(tmp))
    } else {
      return(0)
    }
  }
  payloadParseHeader <- function(headers) {
    if (payloadExists(headers)) {
      l <- (unlist(strsplit(headers[grep("Payload", headers)], "Payload ")))[-1]
      payload <<- c(payload, sum(unlist(lapply(l, convertMultiple))))
    } else {
      payload <<- c(payload, 0)
    }
  }
  payloadParseCols <- function() {
    c("payload")
  }
  payloadExists <- function(headers) {
    return(return(!identical(grep("Payload", headers), integer(0))))
  }
  payloadData <- function() {
    list(payload)
  }
  list(parser=payloadParseHeader, clear=clearParseData, exists=payloadExists, data=payloadData, names=payloadParseCols)
}

# The function reads the trace file "filename" 
# skipping the lines where the predicate lines returns FALSE (by default all lines will be read)
# and applying to each line parsers from list "parsers"
# The function returns data frame containing clean data from the trace file
genericTraceReader <- function(filename, lines = function(x) TRUE, parsers=list()) {
  if (!file.exists(filename)) stop("Trace file not exists!")
  if (length(parsers)==0) stop("No parsers found!")
  for (parser in parsers) parser$clear()
  for (line in readLines(filename)) {
    if (lines(line)) {
      headers <- unlist(strsplit(line, "ns3::", fixed=TRUE))
      for (parser in parsers) {
        parser$parser(headers)
      }
    }
  }
  df <- NULL
  dfnames <- c()
  for (parser in parsers) {
    for (d in parser$data()) {
      if (is.null(df)) {
        df <- data.frame(d, stringsAsFactors=FALSE)
      } else df <- cbind(df, d, stringsAsFactors=FALSE)
    }
    dfnames <- c(dfnames, parser$names())
  }
  names(df) <- dfnames
  for (parser in parsers) {
    parser$clear()
  }
  df
}
