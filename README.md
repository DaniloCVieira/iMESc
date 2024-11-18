# iMESc: Interactive Machine Learning App for Environmental Sciences

iMESc is a state-of-the-art application designed to bring the power of machine learning to environmental science. Through its interactive interface, it facilitates a more intuitive and seamless experience for users to engage with complex data sets and analysis processes.

## Installation & Usage

### Option 1: Running iMESc in RStudio

#### Step 1: Install R and RStudio

If you haven't already, install R and RStudio by following the instructions on their respective websites.

#### Step 2: Open RStudio

Once R and RStudio are installed, open RStudio.

#### Step 3: Install Shiny Package

Install the shiny package if it's not already installed. You can do this by running the following command in the RStudio console:

``` r
install.packages('shiny')
```

#### Step 4: Run iMESc

To start using the iMESc app, run the following code in the RStudio console:

``` r
shiny::runGitHub('iMESc','DaniloCVieira', ref='main')
```

Note: When you use the iMESc app for the first time, it will automatically install all the necessary packages, which may take several minutes to complete. However, once the first installation is finished, subsequent access of the app will be much faster. If the required packages are not already loaded, they typically take several seconds to load. On the other hand, if the packages are already loaded, iMESc will start almost instantly.

### Option 2: Running iMESc with Docker

For a hassle-free setup, especially for users unfamiliar with RStudio, iMESc is available as a Docker container. The Docker image includes all dependencies, ensuring a consistent environment for running iMESc.

#### Prerequisites

-   Install Docker by following the instructions for your operating system on the [Docker website](https://www.docker.com/).

#### Run the Docker Image

``` bash
docker pull vieiradc/imesc
docker run -d -p 3838:3838 vieiradc/imesc
```

#### Access the App

Once the container is running, open your browser and navigate to:

```         
http://localhost:3838
```

## Desktop Version for Windows Users

If you are a Windows user, you can enhance your experience with the standalone iMESc desktop version. Visit the [iMESc-Desktop repository](https://github.com/DaniloCVieira/iMESc-Desktop) to find more details and download the installer.

## Tutorial

For a detailed walkthrough of the application and its features, visit the tutorial page: iMESc_help [<https://danilocvieira.github.io/iMESc_help/>]

## License

This project is licensed under the CC BY-NC-ND 4.0 license. To view a copy of this license, visit [CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode.txt).
