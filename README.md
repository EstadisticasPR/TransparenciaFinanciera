# Transparencia Financiera PR

sudo docker build -t transparencia . && sudo docker run -d -p 80:80 transparencia

## English

### Getting Started

This project is part of our commitment to improve the transparency of the Government of Puerto Rico. Currently, it provides the infrastructure to serve detailed information on the transactions of the Institute of Puerto Rican Culture and of the Puerto Rico Institute of Statistics in different years. It is our intention to serve as an example and to motivate all public entities of the Government of Puerto Rico to do the same.

### Prerequisites

First, you need to have the free software environment for statistical computing and graphics, R, installed in your machine. You can find the most recent version of R @ [https://cran.r-project.org/](https://cran.r-project.org/)

Once in R, you need to run the following lines to install the necessary packages to be able to install the project:

```R
install.packages(c("shiny", "shinyjs", "shinyalert", "shinydashboard", "tidyverse", "DT", "plotly"))
```

### Execute the App

to execute the app locally you just have to clone the git repository and open the server.R file or the ui.R file in RStdio and execute. 

### Built With

R, R Studio and the following packages: 

* shiny: 1.0.5
* shinyjs: 1.0
* shinyalert: 1.0
* shinydashborad: 0.7.0
* tidyverse: 1.2.1
* DT: 0.4
* plotly: 4.7.1

### Contributing

You can contribute by writing a pull request or contacting webmaster@estadisticas.pr

### Authors

This shinydashboard was written by Ian Flores Siaca (https://github.com/ian-flores). 

### License

## Spanish

### Comenzando

Transparencia Financiera es parte de nuestro compromiso con mejorar la transparencia del Gobierno de Puerto Rico. Al momento, provee la infraestructura para servir la información detallada de las transacciones del Instituto de Cultura Puertorriqueña y del Instituto de Estadísticas de Puerto Rico en distintos años. Esto con la intención de servir de ejemplo y motivar a todas las entidades públicas a hacer lo propio.

### Pre-requisitos

Primero, deberá instalar el ambiente de software para programacion estadistica y graficos, R, instalado en su computadora o servidor. Puede encontrar la version mas reciente de R @ [https://cran.r-project.org/](https://cran.r-project.org/)

Luego, dentro de R, necesita ejecutar las siguientes lineas para instalar los paquetes necesarios para poder instalar el proyecto: 

```R
install.packages(c("shiny", "shinyjs", "shinyalert", "shinydashboard", "tidyverse", "DT", "plotly"))
```

### Ejecutar la aplicacion 

Para ejecutar la aplicacion en un ambiente local 
### Herramientas de Desarrollo
### Para Contribuir
### Autores
### Licensia
