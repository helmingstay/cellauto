require(seewave)
require(audio)

.fn <- 'noise.wav'
.rate <-44.1e3
## freq bounds
.bounds <- c(30, 640)
## nsamples at each freq
.nn <- 7000
## weeeeeird
#.bounds <- c(10, 1e4)
#.nn <- 100
.freq.sample <- seq(from=.bounds[1], to=.bounds[2], length.out = .rate)

## fade in/out
## in n samples
.nedge <- 1e4
.edge <- (0:(.nedge-1))/.nedge
## seconds
.len <- 200
.env <- c(.edge, rep(1, .rate*.len-2*.nedge), rev(.edge))
#.ifreq <- rep(runif(.len*(.rate/10), min=.bounds[1], max=.bounds[2]), each=10)
.nsamp <- .len*(.rate/.nn)
.ifreq <- rep(sample(.freq.sample, .nsamp, replace=T, prob=1/.freq.sample^2), each=.nn)
aa <- synth2(ifreq=.ifreq, env=.env, f=.rate, output='audioSample')
save.wave(aa, .fn)

