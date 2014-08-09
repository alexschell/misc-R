## Script to display trends in time use estimates from a TagTime
## log file. Two smoothing stages: simple moving average followed
## by exponential moving average.

# TODO: replace index-based averaging with time-based averaging
# Not huge priority: see tagtime_trends_timestamps.png

library("TTR")

log.path <- "path/to/tagtime/log/file"  # CHANGEME
tags <- c("prod3", "prod0", "slp", "proj", "work")
  # vector of tag names (or regular expressions) to plot trends of
  # should be roughly similar in frequency
sma.window <- 50  # window size for simple moving average
ema.rate <- 0.01  # parameter for exponential moving average

# parse log file
ttlog <- readLines(log.path)
ttlog <- strsplit(ttlog, " ")
dat <- data.frame(times = sapply(ttlog, function(x) as.numeric(x[1])),
                  tags = sapply(ttlog, function(x) paste(x[-1][x[-1] != ""],
                                                         collapse = " ")))

mat <- sapply(tags, function(x) as.numeric(grepl(x, dat$tags)))
n.obs <- dim(mat)[1]

# smoothe
sma <- sapply(1:(n.obs - sma.window),
              function(x) apply(mat, 2,
                                function(y) mean(y[x:(x + 50)])))
sma <- t(sma)
ema <- apply(sma, 2, function(x) EMA(x, n = 1, ratio = ema.rate))
ema <- ema * 168  # convert from fraction to hours per week

# compute plot dimensions
n.tags <- length(tags)
n.rows <- trunc(sqrt(n.tags))
n.cols <- min(which(1:10 * n.rows >= n.tags))  # assume 10 or fewer rows

# plot
dev.new()
par(mfrow = c(n.rows, n.cols))
for (i in 1:length(tags)) {
  plot(ema[,i], type = "l", ylim = range(ema), ylab = "",
       main = paste("Time spent on", colnames(ema)[i], "(hrs/wk)"))
}
