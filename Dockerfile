FROM haskell:8.0.1

COPY . /opt/Nomyx
WORKDIR /opt/Nomyx
RUN stack install --system-ghc 

ENTRYPOINT ["stack exec Nomyx --system-ghc"]
