FROM haskell:8.0.1

RUN apt-get update
RUN apt-get install -y haskell-platform libpq-dev libcurl4-gnutls-dev
ENV CRANBERRY_HOME /cranberry
RUN mkdir $CRANBERRY_HOME

WORKDIR $CRANBERRY_HOME
ADD . $CRANBERRY_HOME
RUN cabal sandbox init
RUN cabal update
RUN cabal install --reorder-goals

CMD [".cabal-sandbox/bin/cranberry", "-p", "80"]
