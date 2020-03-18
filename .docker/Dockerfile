FROM gitpod/workspace-full:latest

USER root

RUN wget https://dotnetwebsite.azurewebsites.net/download/dotnet-core/scripts/v1/dotnet-install.sh && \
    bash -c dotnet-install.sh -v 3.1.200 && \
    rm -rf dotnet-install.sh;
