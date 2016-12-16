FROM haskell:8.0.1

RUN apt-get update
RUN apt-get -y install libpq-dev

RUN cabal update
RUN cabal install snap snap-templates

ENV SNAP_HOME /opt/server

WORKDIR $SNAP_HOME

ADD ./snap_app.cabal $SNAP_HOME/snap_app.cabal

ENV PATH /root/.cabal/bin:$PATH

ADD ./ $SNAP_HOME

RUN cd $SNAP_HOME && cabal install
