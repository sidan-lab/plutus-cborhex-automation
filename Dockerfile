FROM thoughtbot/ghc

RUN cabal update
RUN cabal install happy yesod-bin
