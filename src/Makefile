scf4: exe
	alacritty --hold --command ./run.sh

exe: main.hs
	ghc -threaded -o exe main.hs

clean:
	rm exe
