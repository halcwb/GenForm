FROM dcurylo/fsharp-mono-netcore

WORKDIR /service

COPY . /service

RUN mono .paket/paket.exe install && \
    chmod +x runtests.sh && \
    dotnet build && \
    ./runtests.sh


