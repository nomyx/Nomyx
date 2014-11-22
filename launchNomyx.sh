
version=0.7.3
cmd="cabal exec .cabal-sandbox/bin/Nomyx -- -h www.nomyx.net -p 8000 -r ~/Nomyx/ -f ./.cabal-sandbox/share/x86_64-linux-ghc-7.6.2/Nomyx-Web-$(version)/ -s ./.cabal-sandbox/share/doc/x86_64-linux-ghc-7.6.2/Nomyx-Language-$(version)/ -a XXXX -m"

screen -S nomyx $cmd


