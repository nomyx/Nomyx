version := 0.7.0
installroot := Nomyx-$(version)/

cibuild: cabalinstall deb

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
	#cd install_root && fpm -s dir -t deb -n nomyx -v $(version) -d aptitude --prefix / .
        

upload:
	scp Nomyx-$(version).tar.gz kau@www.nomyx.net:
      
deploy:
	ssh kau@www.nomyx.net "tar -xzvf Nomyx-0.7.0.tar.gz; cd $(installroot); ./launchNomyx.sh"

