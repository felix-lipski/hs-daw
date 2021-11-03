HC      = ghc
HC_OPTS = -threaded

scf4: exe
	alacritty --hold --command ./run.sh

exe: main.hs audio.hs
	$(HC) -o exe audio.hs main.hs $(HC_OPTS)

clean:
	rm exe *.o *.hi *.dyn_o *.dyn_hi
