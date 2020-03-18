FROM gitpod/workspace-full:latest

USER root

ENV PATH=$HOME/.dotnet/dotnet
RUN wget https://dotnetwebsite.azurewebsites.net/download/dotnet-core/scripts/v1/dotnet-install.sh && \
    bash dotnet-install.sh -v 3.1.200 && \
    rm -rf dotnet-install.sh;
