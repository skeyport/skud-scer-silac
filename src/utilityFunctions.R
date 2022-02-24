
no.legend <- theme(legend.position="none")

geom_diagline <- function(linetype='solid',size=0.1,colour="grey20",...) {
    geom_abline(slope=1,intercept=0,linetype=linetype,colour=colour)
}

scientific_10 <- function(x) {
    xout <- gsub("1e", "10^{", format(x),fixed=TRUE)
    xout <- gsub("{-0", "{-", xout,fixed=TRUE)
    xout <- gsub("{+", "{", xout,fixed=TRUE)
    xout <- gsub("{0", "{", xout,fixed=TRUE)
    xout <- paste(xout,"}",sep="")
    return(parse(text=xout))
}

scale_x_log10nice <- function(name=waiver(),omag=seq(-10,20),...) {
    breaks10 <- 10^omag
    scale_x_log10(name,breaks=breaks10,labels=scientific_10(breaks10),...)
}

scale_y_log10nice <- function(name=waiver(),omag=seq(-10,20),...) {
    breaks10 <- 10^omag
    scale_y_log10(name,breaks=breaks10,labels=scientific_10(breaks10),...)
}

scale_loglog <- function(xname=waiver(), yname=waiver(), ...) {
    list(scale_x_log10nice(name=xname, ...),scale_y_log10nice(name=yname,...))
}

scale_x_log2nice <- function(name=waiver(),omag=seq(-6,6),...) {
    breaks2 <- 2^omag
    scale_x_log10(name,breaks=breaks2,
                  labels=parse(text=paste("2^{",omag,"}")),...)
}

scale_y_log2nice <- function(name=waiver(),omag=seq(-6,6),...) {
    breaks2 <- 2^omag
    scale_y_log10(name,breaks=breaks2,
                  labels=parse(text=paste("2^{",omag,"}")),...)
}

scale_loglog2 <- function(...) {
    list(scale_x_log2nice(...),scale_y_log2nice(...))
}

theme_density <- function(...) {
    list(scale_y_continuous(expand=c(0.01,0.01)),
         theme(panel.border=element_blank(),
               panel.background=element_blank(),
               axis.line=element_blank(),
               axis.text.y=element_blank(),
               axis.title.y=element_blank(),
               axis.ticks.y=element_blank()),...)
}

# No legend for plots
# Usage: ggplot(d, aes(...)) + no.legend
no.legend <- theme(legend.position="none")


logfinite <- function(x) {
  xl <- log(x)
  xl[!is.finite(xl)] <- NA
  xl
}

corfinite <- function(x,y=NULL,use='pairwise.complete.obs',method="pearson") {
    # wrapper to calculate correlation coefficients for only finite values
    # useful for correlations of log-transformed values
  if (!is.null(y)) {
    niceinds <- which(is.finite(x) & is.finite(y))
    res <- cor(x[niceinds],y[niceinds],method=method)
  } else {
    x[!is.finite(x)] <- NA
    res <- cor(x,use=use,method=method)
    
  }
  res
}

logcor <- function(x,y=NULL,use='pairwise.complete.obs',method="pearson") {
    # wrapper for correlations of log-transformed values
  x <- as.matrix(x)
  ly <- y
  if (!is.null(y)) {ly <- log(as.matrix(y))}
  corfinite(log(x),ly,use=use,method=method)
}

odds <- function(p) {
  p/(1-p)
}

odds2p <- function(o) {
  o/(1+o)
}

p2odds <- function(p) {
  res <- odds(p)
  res[!is.finite(res)] <- NA
  res
}

logodds <- function(x) {
  # log odds, a shortcut 
  res <- log(odds(x))
  res[!is.finite(res)] <- NA
  res
}

invlogodds <- function(x) {
  # inverse log odds: a = invlogodds(logodds(a))
  y <- exp(x)
  y/(1+y)
}
