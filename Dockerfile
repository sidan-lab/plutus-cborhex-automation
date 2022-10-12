FROM haskell:8.10
RUN cabal update && cabal install pandoc citeproc
ENTRYPOINT ["pandoc"]
RUN cabal run
