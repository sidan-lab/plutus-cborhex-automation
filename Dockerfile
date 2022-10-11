# here I need what?
FROM haskell

# Install dependencies
RUN cabal update
RUN cabal install happy yesod-bin

EXPOSE 80

CMD cabal repl
