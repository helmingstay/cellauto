
f <- 44.1e3 # sampling frequency
 d <- 10    # duration (1 s)
cf <- 440

if(F){
    ## very harsh
    clarinet <- c(1, 0, 0.5, 0, 0.14, 0, 0.5, 0, 0.12, 0, 0.17)
     s <- synth(f=f, d=d, cf = 235.5, harmonics=clarinet)
    save.wave(s, 'audio.clar.wav')
}

s <- synth(f=f,d=d,cf=cf,am=c(50,10))
     # pure tones with am
     # and phase shift of pi radian (180 degrees)
     s <- synth(f=f,d=d,cf=cf,am=c(50,10,pi))
save.wave(s, 'audio.amphase.wav')
     # pure tone with +1000 Hz linear fm
     s <- synth(f=f,d=d,cf=cf,fm=c(0,0,1000))
save.wave(s, 'audio.linfm.wav')
     # pure tone with sinusoidal fm
     # (maximum excursion of 250 Hz, frequency of 10
     s <- synth(f=f,d=d,cf=cf,fm=c(250,10,0))
save.wave(s, 'audio.sinfm.wav')

 
s <- synth(f=f,d=d,cf=cf,fm=c(400,3/d,0),shape="tria")
save.wave(s, 'audio.trifm.wav')


