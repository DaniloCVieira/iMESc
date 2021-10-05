# iMESc

## Running offline 

### Follow the steps bellow to get started with " **iMESc**

Follow the steps bellow to get started with **iMESc**

**1.** Install R[https://cran.r-project.org/] and RStudio  [https://www.rstudio.com/products/rstudio/download/] if you haven't done so already;

**2.** If you are a Mac or Linux user please follow the commands in the section Shell commands before proceeding;

**3.** Initate a new script;

**4.** Install shiny package if it is not already installed;

**5.** Run the functions below;

 ```
 library(shiny)
  runGitHub("iMESc","DaniloCVieira", ref='main')
  ```
 
The app will automatically install the required packages and may take some time if this is your first time using the application. The next time it shouldn't take more than a few seconds.



### Shell commands: Mac OS and Linux systems
Mac and Linux Users may require some additional commands before installing or running the application for the first time.

#### If you are a Mac User:

**1.** Open a new terminal;

**2.** Type the following into your terminal:
 ```
 xcode-select -p it will open a prompt
  ```

**3.** Install the Command Line Tools from the prompt;

**4.** Try run the code bellow again:

 ```
 library(shiny)
  runGitHub("iMESc","DaniloCVieira", ref='main')
  ```
 
**5.** Click in Open in Browser on the top left corner of the window for better visualization;

For other issues please contact the author.

#### If you are a Linux User:

**1.** Open a new terminal;

**2.** Type the following into your terminal:
  ```
sudo apt install libgdal-dev
  ```
**3.** Once the above installation is completed type the next command into your terminal: 
  ```
sudo apt install libudunits2-dev
  ```
**4.** Try run the code bellow again:
 ```
 library(shiny)
  runGitHub("iMESc","DaniloCVieira", ref='main')
  ```
 
