

cmd="cabal exec .cabal-sandbox/bin/Nomyx -- -h www.nomyx.net -p 9998 -r . -f ./.cabal-sandbox/share/x86_64-linux-ghc-7.6.2/Nomyx-Web-0.7.0/ -s ./.cabal-sandbox/share/doc/x86_64-linux-ghc-7.6.2/Nomyx-Language-0.7.0/ -a NXPSD"

screen -S nomyx -X quit
screen -dmS nomyx $cmd


