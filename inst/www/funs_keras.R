
  to_binary<-function(df){
    bin<- kohonen::classvec2classmat(df[,1])
    rownames(bin)<-rownames(df)
    bin
  }
  getinputs<-function(input){
    lapply(1:input$nlay,function(i){
      data.frame(units=input[[paste0('units',i)]],
                 #input_shape=input[[paste0('input_shape',i)]],
                 activation=input[[paste0('activation',i)]]
      )
    })
  }
  is_scaled<-function(data){
    res<-list(scale=F,center=F)
    sc<-attr(data,"scale")
    if(!is.null(sc)){
      if(!is.null(sc$center)){
        res$center<-T
      }
      if(!is.null(sc$scale)){
        res$scale<-T
      }

    }
    res
  }


  plot_network<-function(model, pal=turbo) {
    req(!is.null(model))
    if(is.null(pal)){
      library(viridis)
      pal<-turbo
    }
    model_info_df<-extract_model_info(model)
    max_neurons<-max(as.numeric(model_info_df$Output_Shape)) # Encontra o número máximo de neurônios em uma camada
    layer_colors<-pal(nrow(model_info_df)) # Gera uma paleta de cores
    li<-lapply(nrow(model_info_df):1, function(i) {
      num_neurons<-as.numeric(model_info_df$Output_Shape[i])
      y_positions<-seq(from = (max_neurons - num_neurons) / 2 + 1,
                       to = (max_neurons + num_neurons) / 2,
                       length.out = num_neurons) # Centraliza no eixo Y

      do.call(rbind, lapply(y_positions, function(y_pos) {
        re<-model_info_df[i, ]
        x<-as.numeric(rownames(model_info_df)[i])

        re$x<-rev(1:nrow(model_info_df))[x]
        re$y<-y_pos
        re$col<-layer_colors[i]
        re
      }))
    })
    network_df<-do.call(rbind, li)
    # Plot da rede neural com as camadas centralizadas
    plot(network_df$x, network_df$y, ann=F, axes=F, xlim=c(0, max(network_df$x) + 1), ylim=c(0, max_neurons + 1), col=network_df$col, pch=16)
    title("Layers")
    li<-split(network_df, network_df$Layer)
    # Adicionar setas e cores para as conexões entre as camadas
    for(i in 1:(length(li) - 1)) {
      lai<-li[[i]]
      lai2<-li[[i + 1]]
      arrows(lai$x, lai$y, lai2$x, lai2$y, length = 0.1, col = lai$col[i])
    }

    text(1:length(li), rev(sapply(li,function(x) min(x$y)))-3,model_info_df$Layer)


  }
  extract_model_info<-function(model) {
    layers<-model$layers
    # Inverter a ordem das camadas para que a camada de entrada esteja no topo
    layers<-rev(layers)
    layer_info<-lapply(layers, function(layer) {
      # Formatar o 'Output Shape' para remover a primeira dimensão (None) que representa o batch size
      output_shape<-paste0(layer$output_shape[-1], collapse = "x")
      output_shape<-ifelse(output_shape == "", "1", output_shape)  # Caso especial para camadas de saída
      c(
        "Layer" = layer$name,
        "Output_Shape" = output_shape,
        "Param" = layer$count_params()
      )
    })
    # str(do.call(rbind, layer_info))
    data.frame(do.call(rbind, layer_info))

  }

  zip_keras<-function(pasta="temp_keras"){
    if (!requireNamespace("zip", quietly = TRUE)) {
      install.packages("zip")
    }
    arquivo_zip <- "keras_temp.zip"
    zip::zip(zipfile = arquivo_zip, files = list.files(pasta, full.names = TRUE, recursive = TRUE))

  }
  read_keras_zip<-function(){
    arquivo_zip <- "keras_temp.zip"
    bin<-readBin(arquivo_zip,"raw",n = file.info(arquivo_zip)$size)
    bin
  }
  keras_bin<-function(){
    zip_keras()
    bin_keras<-read_keras_zip()
    file.remove("keras_temp.zip")
    bin_keras
  }

  unzip_keras<-function(bin){
    pasta_temp_keras <- "temp_keras"
    # Crie a pasta temp_keras se ela não existir
    if (!file.exists(pasta_temp_keras)) {
      dir.create(pasta_temp_keras)
    }
    # Defina o nome do arquivo de destino na pasta temp_keras
    nome_arquivo_destino <- file.path(pasta_temp_keras, "temp.zip")
    writeBin(bin, nome_arquivo_destino)

    unzip(nome_arquivo_destino, exdir = ".")
    file.remove(nome_arquivo_destino)
  }
  plot_history_keras <- function(metrics) {
    data <- data.frame(metrics)
    data$epochs <- 1:nrow(data)

    # Transforme o data frame em um formato longo (tidy) usando melt
    data_long <- reshape2::melt(data, id.vars = "epochs", variable.name = "Metric", value.name = "Value")

    # Gráfico de linhas com facet_wrap
    p <- ggplot(data_long, aes(x = epochs, y = Value, color = Metric)) +
      geom_line(size = 1) +
      labs(
        x = "Epochs",
        y = "Value",
        color = "Metric"
      ) +
      scale_color_manual(values = turbo(length(unique(data_long$Metric)))) +
      facet_wrap(~ Metric, scales = "free_y", ncol = 1)  # Divide em várias facetas

    return(p)
  }



  plot_raster<-function(data, var=1){
    xyz<-data.frame(attr(data,"coords"), data[,var])
    r<-rst_xyz(xyz)
    base_shape<-attr(data,"base_shape")
    layer_shape=attr(data,"layer_shape")
    if(!is.null(base_shape)){
      if(!is.null(layer_shape)){
        crop_shape<-st_union(base_shape,layer_shape)}else{
          crop_shape<-base_shape
        }
      r<- mask(r,crop_shape)
    }

    base_shape<-st_combine(base_shape)
    r_df <- as.data.frame(rasterToPoints(r))
    #subtitle=paste0("r_train: ",round(r_train,3),"\n",'r_test: ',round(r_test,3))
    # Plotando o raster com ggplot2
    ggplot(base_shape) +
      geom_sf(fill="white")+
      geom_sf(data=layer_shape)+
      geom_raster(data = r_df, aes(x = x, y = y, fill = layer)) +
      scale_fill_viridis_c() +
      theme_minimal() +
      labs(title ='Spatial predictions',subtitle ="" ,fill = "Predictions", x = "Longitude", y = "Latitude")

  }

  makeUniqueCustom <- function(names) {
    # Initialize a new vector to store the unique names
    uniqueNames <- c()

    # Iterate over each name
    for (name in names) {
      # Count how many times the name has appeared so far
      count <- sum(uniqueNames == name)

      # If the name has not appeared, add it as is
      if (count == 0) {
        uniqueNames <- c(uniqueNames, name)
      } else {
        # If the name has appeared, append the count in parentheses
        uniqueNames <- c(uniqueNames, paste0(name, "(", count + 1, ")"))
      }
    }

    return(uniqueNames)
  }
  myDT<-function(df,  scrollY = "300px",info=T){
    dom=if(isFALSE(info)){
      't'
    } else{
      "lt"
    }
    DT::renderDataTable(
      df,
      extensions = c('FixedColumns',"FixedHeader"),
      options = list(
        dom=dom,
        pageLength = 15,
        info = FALSE,
        lengthMenu = list(c(15, -1), c( "15","All")),
        scrollX = F,
        scrollY = scrollY,
        fixedHeader=TRUE,
        fixedColumns = list(leftColumns = 1, rightColumns = 0)),
      rownames = TRUE,
      class ='cell-border compact stripe',
      editable=T)
  }
  rst_xyz <- function(xyz) {
    coords <- xyz[, c(1, 2)]
    z <- xyz[, 3]
    ymin <- length(unique(coords[, 2]))
    xmin <- length(unique(coords[, 1]))
    ncol <- round(sqrt(xmin))
    nrow <- round(sqrt(ymin))
    prop <- ncol/nrow
    ncol * nrow
    len <- length(z)
    x <- round(sqrt(len) * prop)
    y <- round(len / (sqrt(len) * prop))

    # Set up an 'empty' raster
    e <- extent(range(coords[, 1]), range(coords[, 2]))
    my_rst <- raster(e, ncol=x, nrow=y)
    projection(my_rst) <- CRS("+proj=longlat +datum=WGS84")
    my_rst <- rasterize(coords, my_rst, z, fun=mean)
    return(my_rst)
  }



