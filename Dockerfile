FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /LastChat
COPY . .
RUN dotnet restore
WORKDIR /LastChat/Shared
COPY . .
WORKDIR /LastChat/Server
COPY . .
EXPOSE 8080
CMD dotnet run