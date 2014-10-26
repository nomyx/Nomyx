version := 0.7.2
installroot := Nomyx-$(version)/
deployroot := /home/cdupont/tmp/Nomyx

cibuild: cabalinstall tar upload

test:
	./Nomyx/tests.sh

cabalinstall:
	cabal sandbox init
	cabal install --ghc-options=-DNO_INTERPRET_QUOTES Nomyx-Language/ Nomyx-Core/ Nomyx-Web/ Nomyx/ --enable-documentation --haddock-hyperlink-source 

tar:
	rm -rf $(installroot)
	mkdir $(installroot)
	cp -R .cabal-sandbox $(installroot)
	cp cabal.sandbox.config $(installroot)
	cp launchNomyx.sh $(installroot)
	tar -czvf Nomyx-$(version).tar.gz $(installroot)
        

upload:
	scp Nomyx-$(version).tar.gz kau@www.nomyx.net:
      
deploy:
	ssh kau@www.nomyx.net upload.sh

