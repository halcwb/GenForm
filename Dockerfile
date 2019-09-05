FROM microsoft/dotnet:2.2-aspnetcore-runtime-alpine
COPY /deploy /
WORKDIR /Informedica.GenForm.Server
EXPOSE 8085
ENTRYPOINT [ "dotnet", "Informedica.GenForm.Server.dll" ]

