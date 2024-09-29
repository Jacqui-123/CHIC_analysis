
#update the rolling means to include a date midpoint using the IHA library
library(zoo)
library(caTools)
library(plyr)
library(zoo)
library(caTools)
library(plyr)

rm(group2, group2Funs, runmean.iha)

group2 <- function(x, year = c('water', 'calendar'), mimic.tnc = T, ...){
  stopifnot(is.zoo(x), inherits(index(x), 'Date') | inherits(index(x), 'POSIXt'))
  year <- match.arg(year)
  yr <- switch(year,
               water = water.year(index(x)),
               calendar = year(index(x)))
  rollx <- runmean.iha(x, year = yr, mimic.tnc = mimic.tnc)
  xd <- cbind(year = yr, as.data.frame(rollx$means))
  midpoints <- rollx$midpoints
  # Include midpoints as part of the output
  res <- ddply(xd, .(year), function(x) group2Funs(x[,-1]), ...)

  # Adding midpoints to the result
  midpoints_df <- data.frame(year = yr, midpoints)
  res <- merge(res, midpoints_df, by = "year", all.x = TRUE)

  return(res)
}

runmean.iha <- function(x, year = NULL, mimic.tnc = F){
  window <- c(1, 3, 7, 30, 90)
  vrunmean <- Vectorize(runmean, vectorize.args = 'k')

  if (mimic.tnc){
    sx <- split(coredata(x), year)
    means_list <- lapply(sx, function(s) {
      means <- vrunmean(s, k = window, alg = 'fast', endrule = 'NA')
      means
    })
    means <- do.call('rbind', means_list)

    # Midpoints calculation for mimic.tnc
    midpoints_list <- lapply(sx, function(s) {
      s_length <- length(s)
      mids <- sapply(window, function(w) {
        seq_len(s_length) - (w %/% 2)  # Calculate midpoints
      })
      mids
    })
    midpoints <- do.call('rbind', midpoints_list)

  } else {
    means <- vrunmean(coredata(x), k = window, alg = 'fast', endrule = 'NA')

    # Midpoints calculation for non-mimic case
    s_length <- length(coredata(x))
    midpoints <- sapply(window, function(w) {
      seq_len(s_length) - (w %/% 2)  # Calculate midpoints
    })
  }

  colnames(means) <- sprintf('w%s', window)
  colnames(midpoints) <- sprintf('w%s_mid', window)
  return(list(means = means, midpoints = midpoints))
}

group2Funs <- function(x){
  rng <- as.numeric(apply(x, 2, range, na.rm=T))
  baseindex <- min(x[,3], na.rm = T) / mean(x[,1], na.rm = T)
  zeros <- length(which(x[,1] == 0))
  ans <- c(rng, zeros, baseindex)
  nms <- sprintf(c('%1$s Day Min', '%1$s Day Max'), rep(c(1, 3, 7, 30, 90), each=2))
  names(ans) <- c(nms, 'Zero flow days', 'Base index')
  ans
}

