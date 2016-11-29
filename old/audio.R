## deps: http://rug.mnhn.fr/seewave/inst.html
library(seewave)
library(audio)

.freqs <- c(30, 32, 35, 38, 40, 43, 50, 55, 65, 70, 80, 85, 95)

.ff <- 44.1e3
bb <- tempdir()
.cw <- setwd(bb)

.nedge <- 1e4
.edge <- (0:(.nedge-1))/.nedge
.len <- 40
.scale=1.5
.env <- c(.edge, rep(1, .ff*.len-2*.nedge), rev(.edge))

#for (jj in 2:6) {
    for (ii in 1:length(.freqs)) {
        #.ifreq <- seq(from=.freqs[ii-1], to=.freqs[ii], length.out=.ff*jj)
        .ifreq <- seq(from=.freqs[ii], to=.freqs[ii]*.scale, length.out=.ff*.len)
        if(ii%%3) .ifreq <- rev(.ifreq)
        aa <- synth2(ifreq=.ifreq, env=.env, f=.ff, output='audioSample')
        .fn <- sprintf(file.path(bb, 'Raudio.%03d.wav'),ii)
        save.wave(aa, .fn)
    }
    system("sox --combine merge 'Raudio.00*.wav' -c 2 out.fin.wav")
    system("ffmpeg -i out.fin.wav out.fin.flac")
#}

#system("sox --combine concat 'out.0*.wav' out.fin.wav")
setwd(.cw)


