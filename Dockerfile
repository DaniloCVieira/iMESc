# Base R Shiny image
FROM rocker/shiny

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install system libraries
RUN apt-get update && \
    apt-get install -y \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    cmake \
    tcl8.6-dev \
    tk8.6-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages in batches
RUN R -e "install.packages(c('pak','progress','rstudioapi'))"

RUN R -e "pak::pkg_install(c('shiny','plotly','aweSOM','ggraph','ggpubr','ggforce','mice','caret','leaflet','GGally','ggplot2','sf','dplyr','igraph','readr','gstat','factoextra','partykit','ade4','scales','party','randomForestExplainer','sortable','ggparty','automap','scatterpie','tibble','colourpicker','DT','pdp','shinyWidgets','e1071','sp','klaR','shinydashboardPlus','shinyTree','htmlwidgets','NCmisc','ggrepel','dendextend','shinycssloaders','readxl','vegan','plot3D','httr','gridExtra','gplots','NeuralNetTools','shinybusy','shinydashboard','raster','colorspace','kernlab','ggridges','shinyjs','reshape2','webshot','viridis','shinyBS','leaflet.minicharts','beepr','data.table','segRDA','indicspecies','permute','kohonen','ggnewscale','randomForest','reshape','jsonlite','gbRd','plyr','base64enc','RColorBrewer','wesanderson','corrplot','writexl','geodist','Metrics','colorRamps','waiter'),ask=F)"

RUN R -e "pak::pkg_install(c('gbm','deepnet','earth','nnet','rpart','monmlp','RSNNS','evtree'),ask=F)"

# Copy the Shiny app code
COPY app.R /home/shiny-app/app.R
COPY inst /home/shiny-app/inst
COPY www /home/shiny-app/www

# Expose the application port
EXPOSE 3838

# Run the R Shiny app
CMD ["Rscript", "-e", "message(paste('Access iMESc in','http://127.0.0.1:3838/')); shiny::runGitHub('imesc','DaniloCVieira','main', port = 3838, host = '0.0.0.0')"]

