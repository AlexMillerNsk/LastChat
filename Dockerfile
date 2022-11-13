FROM mcr.microsoft.com/dotnet/sdk:6.0
WORKDIR /LastChat
COPY . .
# Restore the dependencies of the project
RUN dotnet restore
RUN dotnet publish -c release -o ./publish --no-self-contained --no-restore
EXPOSE 8080
CMD ["./publish/Server"]