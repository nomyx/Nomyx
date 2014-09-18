version := 0.7.0

cibuild: cabalinstall deb

test:
	./Nomyx/tests.sh

cabalinstall:
	cabal sandbox init
	cabal install Nomyx-Language/ Nomyx-Core/ Nomyx-Web/ Nomyx/ --enable-documentation --haddock-hyperlink-source 

deb:
	rm -rf install_root
	mkdir install_root
	mv .cabal-sandbox install_root/
	mv cabal.sandbox.config install_root/
	cp launchNomyx install_root/ 
	cd install_root && fpm -s dir -t deb -n nomyx -v $(version) -d aptitude --prefix / .

deploy:
	scp nomyx_$(version)_amd64.deb kau@ec2-54-235-196-19.compute-1.amazonaws.com
        
