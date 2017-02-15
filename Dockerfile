FROM haskell:8.0.1

COPY stack.yaml                                /opt/Nomyx/stack.yaml
COPY Nomyx/Nomyx.cabal                         /opt/Nomyx/Nomyx/Nomyx.cabal
COPY Nomyx-Core/Nomyx-Core.cabal               /opt/Nomyx/Nomyx-Core/Nomyx-Core.cabal
COPY Nomyx-Language/Nomyx-Language.cabal       /opt/Nomyx/Nomyx-Language/Nomyx-Language.cabal
COPY Nomyx-Library/Nomyx-Library.cabal         /opt/Nomyx/Nomyx-Library/Nomyx-Library.cabal
COPY Nomyx-Web/Nomyx-Web.cabal                 /opt/Nomyx/Nomyx-Web/Nomyx-Web.cabal
COPY Nomyx-Api/Nomyx-Api.cabal                 /opt/Nomyx/Nomyx-Api/Nomyx-Api.cabal
COPY Nomyx-Auth/Nomyx-Auth.cabal               /opt/Nomyx/Nomyx-Auth/Nomyx-Auth.cabal
COPY Nomyx-Client/Nomyx-Client.cabal           /opt/Nomyx/Nomyx-Client/Nomyx-Client.cabal
COPY Imprevu/Imprevu.cabal                     /opt/Nomyx/Imprevu/Imprevu.cabal
COPY Imprevu-Happstack/Imprevu-Happstack.cabal /opt/Nomyx/Imprevu-Happstack/Imprevu-Happstack.cabal
COPY shortcut/shortcut.cabal                   /opt/Nomyx/shortcut/shortcut.cabal

WORKDIR /opt/Nomyx
RUN stack install --system-ghc --only-snapshot 

COPY . /opt/Nomyx
RUN stack install --system-ghc 

ENV PATH /usr/bin:$PATH
CMD stack exec --system-ghc -- Nomyx -p localhost:8000 --noTTY

EXPOSE 8000
EXPOSE 8001
