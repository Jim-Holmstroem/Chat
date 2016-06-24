FROM haskell:7.10

ENV WORKDIR /opt/server

WORKDIR ${WORKDIR}
COPY LICENSE Chat.cabal Setup.hs stack.yaml ${WORKDIR}/
RUN stack build --only-dependencies

COPY src ${WORKDIR}/src
COPY app ${WORKDIR}/app
RUN stack build

CMD [ "stack", "exec", "Chat" ]
