FROM haskell:7.10

ENV WORKDIR /opt/server

WORKDIR ${WORKDIR}

COPY stack.yaml ${WORKDIR}/
RUN stack setup --stack-setup-yaml stack.yaml

COPY LICENSE Chat.cabal Setup.hs ${WORKDIR}/
RUN stack build --only-dependencies

COPY src ${WORKDIR}/src
COPY app ${WORKDIR}/app
RUN stack build

COPY static ${WORKDIR}/static

CMD [ "stack", "exec", "Chat" ]
