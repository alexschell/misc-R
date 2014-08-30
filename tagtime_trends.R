## Script to display trends in time use estimates from a TagTime
## log file. Histogram format with kernel density estimate overlay.

log.path <- "path/to/log/file"  # CHANGEME
ping.interval <- 45  # mean interval between pings in minutes (default 45)
tags <- c("your", "tags", "here")  # CHANGEME
  # vector of tag names (or regular expressions) to plot trends of
n.bins <- 20 # number of bins for the histograms
bandwidth <- 0.7  # smoothness parameter for density estimates

# parse log file

ttlog <- readLines(log.path)
ttlog <- strsplit(ttlog, " ")
dat <- data.frame(time = sapply(ttlog, function(x) as.numeric(x[1])), 
                  tags = sapply(ttlog, function(x) {
                    str <- x[x != ""]
                    paste(str[-(length(str) - 2:0)][-1], collapse = " ")
                  }))

mat <- sapply(tags, function(x) grepl(x, dat$tags))
n.pings <- dim(mat)[1]
n.tags <- dim(mat)[2]

# compute histograms and densities

breaks <- seq(min(dat$time), max(dat$time), length = n.bins + 1)
bin.width <- diff(breaks[1:2])  # histogram bin width in seconds

densities <- histograms <- uptimes <- list()
for (i in 1:n.tags) {
  uptime <- dat$time[mat[,i]]
  uptimes[[i]] <- uptime
  h <- hist(uptime, plot = FALSE, breaks = breaks)
  h$counts <- h$counts * ping.interval / 60 * 7 * 24 * 3600 / bin.width
    # convert from pings per bin to hours per week
  histograms[[i]] <- h
  d <- density(uptime, adjust = bandwidth, cut = -0.5)
  d$y <- d$y * sum(h$counts) * bin.width  # scale density to plot
  densities[[i]] <- d
}

# plotting

ymax <- max(sapply(histograms, function(x) max(x$counts)))
n.rows <- trunc(sqrt(n.tags))
n.cols <- min(which(1:10 * n.rows >= n.tags)) # assume 10 or fewer rows

dev.new()
par(mfrow = c(n.rows, n.cols))
for (i in 1:n.tags) {
  plot(histograms[[i]], main = paste("Time spent on ", tags[i]), 
       col = "aliceblue", border = "white", ylim = c(0, ymax), 
       ylab = "Hours per week", xaxt = 'n', xlab = "")
  x.label.int <- seq(min(dat$time), max(dat$time), length = 8)
  x.label.str <- as.Date(as.POSIXct(x.label.int, origin = "1970-01-01"))
  axis(side = 1, at = x.label.int, labels = x.label.str)
  rug(uptimes[[i]], tick = 0.02, side = 1, col = "lightcoral")
  lines(densities[[i]], lwd = 2, col = "lightcoral")
}
