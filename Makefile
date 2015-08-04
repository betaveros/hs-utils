all : allshifts amp bfi everyn factorize fill grouper ioc kdfoot looper mycal randomline sortchar sortlen square sum tac transpose unamp
clean :
	rm *.hi *.o bin/*
allshifts : allshifts.hs
	ghc --make allshifts.hs -O -o bin/allshifts
amp : amp.hs
	ghc --make amp.hs -O -o bin/amp
bfi : bfi.hs
	ghc --make bfi.hs -O -o bin/bfi
everyn : everyn.hs
	ghc --make everyn.hs -O -o bin/everyn
factorize : factorize.hs
	ghc --make factorize.hs -O -o bin/factorize
fill : fill.hs
	ghc --make fill.hs -O -o bin/fill
grouper : grouper.hs
	ghc --make grouper.hs -O -o bin/grouper
ioc : grouper.hs
	ghc --make ioc.hs -O -o bin/ioc
kdfoot : kdfoot.hs
	ghc --make kdfoot.hs -O -o bin/kdfoot
looper : looper.hs
	ghc --make looper.hs -O -o bin/looper
mycal : mycal.hs
	ghc --make mycal.hs -O -o bin/mycal
randomline : randomline.hs
	ghc --make randomline.hs -O -o bin/randomline
sortchar : sortchar.hs
	ghc --make sortchar.hs -O -o bin/sortchar
sortlen : sortlen.hs
	ghc --make sortlen.hs -O -o bin/sortlen
square : square.hs
	ghc --make square.hs -O -o bin/square
sum : sum.hs
	ghc --make sum.hs -O -o bin/sum
tac : tac.hs
	ghc --make tac.hs -O -o bin/tac
transpose : transpose.hs
	ghc --make transpose.hs -O -o bin/transpose
unamp : unamp.hs
	ghc --make unamp.hs -O -o bin/unamp
