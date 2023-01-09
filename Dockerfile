FROM mcr.microsoft.com/dotnet/sdk:6.0
WORKDIR /src
COPY . .
# Restore the dependencies of the project
RUN dotnet restore
RUN dotnet publish -c release -o ./bin/release/webserver --no-self-contained --no-restore
EXPOSE 8080
CMD ["./bin/release/webserver/Server"]