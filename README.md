# GenForm
This solution is intended as a microservice running in a Docker container to provide a service 
to get medication information. It also provides a user interface to be able to browse the product
inventory along with dosing rules.


## Directory structure
In order to be able to run the service, there has to be a folder in the root directory of the server,
called `data`. This has to contain:

* `cache`: contains cache files 
* `formulary`: contains mapping files
* `zindex`: contains the source files with product information


## Prerequisites
This project follows the [SAFE-Stack guidelines](https://safe-stack.github.io/).
You'll need to install the following pre-requisites in order to build SAFE applications

* The [.NET Core SDK 2.2](https://www.microsoft.com/net/download/)
* [FAKE](https://fake.build/) (>= 5.12) installed as global tool (`dotnet tool install -g fake-cli`)
* Paket installed as global tool (`dotnet tool install paket --add-source https://www.myget.org/F/paket-netcore-as-tool/api/v3/index.json -g`) (optional*)
* [node.js](https://nodejs.org/) (>= 8.0)
* [yarn](https://yarnpkg.com/) (>= 1.10.1) or [npm](https://www.npmjs.com/) 


## Developing with `fake build --target run`. 
Make sure that the data directory is in the root of the main project, 
i.e. `src/Informedica.GenForm.Server/`. 
Go to that directory and start up with `dotnet watch run`.

In the browser 2 requests can be run:

1. `localhost:8080/test` Giving a test request and
2. `localhost:8080/request?bty=2017&btm=1&btd=1&wth=10&hgt=100&pgk=3689`