# Transparencia Financiera PR

-------- ~ Version en Español abajo ~ --------
------------------------------------------
-------- ~ Spanish Version Below ~ --------

## English

### Getting Started

This project is part of our commitment to improve the transparency of the Government of Puerto Rico. Currently, it provides the infrastructure to serve detailed information on the transactions of the Institute of Puerto Rican Culture and of the Puerto Rico Institute of Statistics in different years. It is our intention to serve as an example and to motivate all public entities of the Government of Puerto Rico to do the same.

### Prerequisites

First, you need to have the free software environment for statistical computing and graphics, R, installed in your machine. You can find the most recent version of R @ [https://cran.r-project.org/](https://cran.r-project.org/)

Once in R, you need to run the following lines to install the necessary packages to be able to install the project:

```R
install.packages(c("shiny", "tidyverse", "DT", "data.table", "rbokeh", "lubridate", "shinythemes"))
```

### Execute the App

To execute the app locally you have to clone the git repository and open the server.R file.

```git
git clone https://github.com/ian-flores/TransparenciaFinanciera
cd TransparenciaFinanciera
rstudio server.R
```

### Docker

You can also execute the Docker image which already includes the R packages versions and a Flask app to get the most recent data.

```bash
docker build -t transparencia . && docker run -d -p 80:80 transparencia
```

### Built With

R, R Studio and the following packages: 

* shiny: 1.2.0
* tidyverse: 1.2.1
* DT: 0.4
* data.table: 1.11.8 
* rbokeh: 0.5.0
* lubridate: 1.7.4
* shinythemes: 1.1.2

### Contributing

You can contribute by writing a pull request or contacting webmaster@estadisticas.pr

### Authors

This app was written by Ian Flores Siaca (https://github.com/ian-flores). 

## Spanish

### Comenzando

Transparencia Financiera es parte de nuestro compromiso con mejorar la transparencia del Gobierno de Puerto Rico. Al momento, provee la infraestructura para servir la información detallada de las transacciones del Instituto de Cultura Puertorriqueña y del Instituto de Estadísticas de Puerto Rico en distintos años. Esto con la intención de servir de ejemplo y motivar a todas las entidades públicas a hacer lo propio.

### Pre-requisitos

Primero, deberá instalar el ambiente de software para programacion estadistica y graficos, R, instalado en su computadora o servidor. Puede encontrar la version mas reciente de R @ [https://cran.r-project.org/](https://cran.r-project.org/)

Luego, dentro de R, necesita ejecutar las siguientes lineas para instalar los paquetes necesarios para poder instalar el proyecto: 

```R
install.packages(c("shiny", "tidyverse", "DT", "data.table", "rbokeh", "lubridate", "shinythemes"))
```

### Ejecutar la aplicacion 

Para ejecutar la aplicacion localmente tiene que clonar el repositorio y abrir el archivo server.R

```git
git clone https://github.com/ian-flores/TransparenciaFinanciera
cd TransparenciaFinanciera
rstudio server.R
```
### Docker

Tambien puede ejecutar la imagen Docker, la cual incluye las dependencias necesarias y una aplicacion de Flask para obtener los
datos mas recientes. 

```bash
docker build -t transparencia . && docker run -d -p 80:80 transparencia
```
### Herramientas de Desarrollo

R, Rstudio y los siguientes paquetes:

- shiny: 1.2.0
- tidyverse: 1.2.1
- DT: 0.4
- data.table: 1.11.8
- rbokeh: 0.5.0
- lubridate: 1.7.4
- shinythemes: 1.1.2

### Para Contribuir

Si quiere contibuir puede someter un pull request o escribir a webmaster@estadisticas.pr

### Autores

Este app fue escrito por Ian Flores Siaca (https://github.com/ian-flores)
