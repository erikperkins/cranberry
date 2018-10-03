FROM haskell:8.0.2

RUN apt-get update
RUN apt-get -y install libpq-dev

RUN cabal update
RUN cabal install snap snap-templates

ENV CRANBERRY_HOME /opt/server

WORKDIR $CRANBERRY_HOME

ADD ./cranberry.cabal $CRANBERRY_HOME/cranberry.cabal

ENV PATH /root/.cabal/bin:$PATH

ADD ./ $CRANBERRY_HOME

RUN cd $CRANBERRY_HOME && cabal install
