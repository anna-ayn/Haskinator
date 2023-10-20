COMP = ghc

Haskinator : Oraculo
	$(COMP) Haskinator.hs Oraculo

Oraculo : 
	$(COMP) Oraculo.hs

clean:
	rm *.o *.hi Haskinator