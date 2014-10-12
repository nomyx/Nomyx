version := 0.7.2
installroot := Nomyx-$(version)/
deployroot := /home/cdupont/tmp/Nomyx

cibuild: cabalinstall tar deploy

test:
	./Nomyx/tests.sh

cabalinstall:
	cabal sandbox init
	cabal install Nomyx-Language/ Nomyx-Core/ Nomyx-Web/ Nomyx/ --enable-documentation --haddock-hyperlink-source 

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
	ssh kau@www.nomyx.net "tar -xzvf Nomyx-$(version).tar.gz; mv $(deployroot) $(deployroot)-sav; mv Nomyx $(deployroot);"

