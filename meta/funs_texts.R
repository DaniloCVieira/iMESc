
# Mac: xcode-select --install
#"Ubutu:  sudo apt install libgdal-dev"
#"Ubutu:  sudo apt install libudunits2-dev"

datalist_render<-function(datalist=NULL,bagdata=F)
{
  data=datalist
  factors=attr(data,"factors")
  datalist_name=attr(data,"datalist")

  coords=attr(data,"coords")
  base_shape=attr(data,"base_shape")
  layer_shape=attr(data,"layer_shape")
  data.factors=attr(data,"data.factors")
  transf=attr(data,"transf")
  column(12,
         column(12,style="background: #e6e6e6ff;color: black;border: 1px dashed black;margin: 10px 10px 10px 10px",
                h5(strong("Datalist:"),if(length(datalist_name)>0) {em(datalist_name, style="color: #05668D")})),
         column(12,absolutePanel(
           style="border-left: 1px dashed black; margin-top: -10px", height = "300px",
         )),
         column(12,
                if(length(data)>0) {
                  splitLayout(
                    absolutePanel(style="; height:20px; border-bottom: 1px dashed black;", width='90px'),
                    column(12,style="color:  gray",
                           p(h5(p("Data-Attribute"), style="color: #05668D")),
                           p(em("n.obs:",nrow(data), ";","nvar-numeric:", ncol(data), )),
                           if(length(data.factors>0)){
                             em("nvar-factors:", ncol(data.factors))
                           },
                           if(isFALSE(bagdata)) {renderPrint(transf)}

                    ),
                    cellWidths = c("16%",'84%')
                  )}
         ),
         column(12,
                if(length(factors)>0) {
                  splitLayout(
                    absolutePanel(style="; height:20px; border-bottom: 1px dashed black;", width='90px'),
                    column(12,style="color:  gray",
                           (h5(p("Factor-Attribute"), style="color: #05668D")),
                           em("n.obs:",nrow(factors), ";", "nvar:", ncol(factors))
                    ),
                    cellWidths = c("16%",'84%')
                  )}
         ),
         column(12,
           if(length(coords)>0) {
             splitLayout(
               absolutePanel(style="; height:20px; border-bottom: 1px dashed black;", width='90px'),
               column(12,style="color:  gray",
                      p(h5(p("Coords-Attribute"), style="color: #05668D")),
                      em("n.obs:",nrow(coords), ";", "nvar:", ncol(coords))
               ),
               cellWidths = c("16%",'84%')
             )}
         ),
         column(12,
           if(length(base_shape)>0) {
             splitLayout(
               absolutePanel(style="; height:20px; border-bottom: 1px dashed black;", width='90px'),
               column(12,style="color:  gray",
                      p(h5(p("Base-shape-Attribute"), style="color: #05668D")),
                      em("sf object")
               ),
               cellWidths = c("16%",'84%')
             )}
         ),
         column(12,
           if(length(layer_shape)>0) {
             splitLayout(
               absolutePanel(style="; height:20px; border-bottom: 1px dashed black;", width='90px'),
               column(12,style="color:  gray",
                      p(h5(p("Layer-shape-Attribute"), style="color: #05668D")),
                      em("sf object")
               ),
               cellWidths = c("16%",'84%')
             )}
         )

  )

}



data_migrate<-function(data,newdata, newname){
  {
    attr(newdata, "data.factor")=attr(data,"data.factor")
    attr(newdata, "datalist")=newname
    attr(newdata, "filename")=attr(data, "filename")
    attr(newdata, "factors")=attr(data, "factors")[rownames(data),]
    attr(newdata, "coords")= attr(data,"coords")
    attr(newdata, "base_shape")= attr(data,"base_shape")
    attr(newdata, "layer_shape")=attr(data,"layer_shape")
    attr(newdata, "transf")=attr(data, "transf")
    return(newdata)
  }
}




textoffline<-function(...)({
  column(12,
        h4(strong("Running offline"), icon("fas fa-windows"), icon("fas fa-apple"), icon("fas fa-linux")),
        p("The last version of ",span('iMESc',style="font-family: 'Alata', sans-serif;")," is also available in ",a("GitHub",href="https://github.com/DaniloCVieira/iMESc")),
        p("Follow the steps bellow to get started with ",span('iMESc',style="font-family: 'Alata', sans-serif;"),":"),
       column(12,style="background: white",
              p(style="margin-top:20px; ",
                strong("1."),"Install",a("R", href="https://cran.r-project.org/")," and ",a("RStudio",href="https://www.rstudio.com/products/rstudio/download/")," if you haven't done so already;"),
              p(strong("2.")," If you are a Mac or Linux user, please follow the commands in the section ",strong("'Shell commands'")," before proceeding;"),
              p(strong("3.")," Initate a new script;"),
              p(strong("4.")," Install shiny package if it is not already installed;"),
              p(strong("5.")," Run the functions below;")),
        fluidRow(style="margin-left: 40px;margin-right: 40px;",
               column(12,style="background: white;font-family: Lucida Console;border: 1px solid SeaGreen; margin: 10 10 10 10",
                      p(style="margin-top:20px",
                        code(
                        'library(shiny)')),
                      p(code("runGitHub('iMESc','DaniloCVieira', ref='main')")))),
        p(style="margin-top:20px; ","The app will automatically install the required packages, and may take some time if this is your first time using the application. The next time, it shouldn't take more than a few seconds."),

        p(style="border-top: 1px solid",
          h4(style="margin-top:20px",
           strong("Shell commands: Mac OS and Linux systems"), icon("fas fa-apple"), icon("fas fa-linux"))),
        p("Mac and Linux Users may require some additional commands before installing or running the application for the first time;"),

       column(12,style="background: white; margin-bottom: 20px",
              p(style="margin-top:20px; ",
                icon("fas fa-apple"),
                strong("If you are a Mac User:"), style="color: SeaGreen"),
              p(style="margin-top:20px;",
                strong("1.")," Open a new terminal;"),
              p(strong("2.")," Type the following into your terminal:"),
              p(code("xcode-select -p"), "it will open a prompt"),
              p(strong("3.")," Install the Command Line Tools from the prompt;"),
              p(strong("4.")," Try run'", code("runGitHub('iMESc','DaniloCVieira', ref='main')"),"again"),
              em('For other issues, please contact the author.')
              ),

       column(12,style="background: white",
              p(style="margin-top:20px; margin-left: 10px;",
                icon("fas fa-linux"),
                strong("If you are a Linux User:"), style="color: SeaGreen"),
              p(strong("1.")," Open a new terminal;"),
              p(strong("2.")," Type the following into your terminal:"),
              p(code("sudo apt install libgdal-dev")),
              p(strong("3.")," Once the above installation is completed, type the next command into your terminal: "),
              p(code("sudo apt install libudunits2-dev")),
              p(strong("4.")," Try run'", code("runGitHub('iMESc','DaniloCVieira', ref='main')"),"again;"),
              em('For other issues, please contact the author.'))
  )


})



textpackages<-function(...)
{
  div(

    h5("Packages"),
    p('shinydashboard'),
    p('shinydashboardPlus'),
    p('shinyjs'),
    p('shiny'),
    p('readxl'),
    p('vegan'),
    p('caret'),
    p('viridisLite'),
    p('aweSOM'),
    p('sp'),
    p('raster'),
    p('rasterVis'),
    p('rgdal'),
    p('gstat'),
    p('ggspatial'),
    p('ggplot2'),
    p('sf'),
    p('class'),
    p('shinyWidgets'),
    p('randomForestExplainer')
  )
}


texttask<-function(...)
{
  div(
    h5("tasks:"),
    p("- define the final tabs,"),
    p("- set/define selector inputs,"),
    p("- implement help pages/tool tips"),
    p("- implement download results/Rfile,"),
    p("- implement more graphics options,"),
    p("- finish random forest, decision tree")

  )
}

textabout<-function(...)
{
  div(
    h4("Requirements"),
    h5(strong("data input")),
    p('...'),
    h5(strong("running the app in the R-shiny server")),
    p('...'),
    h5(strong("running the app offline")),
    p('...'),

  )
}


psummary<-function(data)
{
  nas=sum(is.na(unlist(data)))
  data<-na.omit(data)
  n=data.frame(rbind(Param=paste('Missing values:', nas)))
  a<-data.frame(rbind(Param=paste('nrow:', nrow(data)),paste('ncol:', ncol(data))))

  c<-data.frame(Param=
               c("max:", "min:", "mean:","median:","var:", "sd:"),
             Value=c(max(data), min(data), mean(unlist(data)), median(unlist(data)), var(unlist(data)), sd(unlist(data))))
  c$Value<-unlist(lapply(c[,2],round,3))
  ppsummary("-------------------")
  ppsummary(n)
  ppsummary("-------------------")
  ppsummary(a)
  ppsummary("-------------------")
  ppsummary(c)
  ppsummary("-------------------")
}

ppsummary <- function(m){
  write.table(format(m, justify="left", trim=T),
              row.names=F, col.names=F, quote=F)
}
textnclust<-function(...)
{
 div(
   h3("Determining the best number of clusters"),
   h4(strong("elbow")),
   p(" Generate elbow plot? **** this may take a while to run..."),
   br(),
   br(),
   h4(strong("rfloop")),
   p("Perform random forest analysis with increasing number of clusters"),
   h4(strong("cv")),
   p("number of folds for cross-validation"),
   h4(strong("repeats")),
   p("the number of complete sets of folds to compute")


 )
  }


texthc<-function(...){
  div(
    h4("Hierarchical clustering"),
    br(),


  )
}



textintro<-function(...)
{
  column(12,

        column(12,
               br(),
               h4(strong("Welcome to",span('iMESc',style="font-family: 'Alata', sans-serif;"),style="color: #05668D")),
               p("This app is intended to dynamically integrate machine learning techniques to explore multivariate data sets. The app runs Self-Organizing Maps (SOM), Hierarchical clustering (HC) and  Random Forest (RF) analysis. If desired, it plots the results into geographical maps and has multiple options to improve graph visualization."))


      )

}
textref<-function(...)
{
  div(
    h4(strong("References")),
    br(),
    br(),
    p("List of references")
  )
}
textProperty<-function(...){
  div(
    p("Show areas for high values (red) and low values (blue) for the selected variable")
  )
}

textauthors<-function(...)
{
  column(
    12,style="background: white; margin-left: 20px",
    h5(strong("Author")),
    column(12,
           p("Danilo C Vieira")),

    h5(strong("Contributors")),
    column(12,
           p("Juliane Castro"),
           p("Daiane Faller"),
           p("Fabiana S. Paula"),
           p("Dr. Gustavo Fonseca"))
  )

}

textpretreat<-function(...)
{
  div(
    h3("Pre-treatment"),
    br(),
    br(),
    h4(strong("Transformation")),
    p("Type of transformation"),
    h4(strong("Remove rares")),
    p("removes..."))
}

textsom<-function(...){
  div(h4("Self-organizing maps..."),
      p("Self-organizing maps (SOMs) is an unsupervised competitive learning method (Kohonen, 2001). The application of SOMs can be thought as a data reduction procedure which aggregates similar data in map units (Best-mathing units - BMUs). BMUs are not the means, or medians, of group of samples, but the results of a learning phase, so that data patterns and features are extracted by modeling, rather than by ‘simply’ averaging, or linearly projecting on reduced subspaces, data (Liu et al., 2006)"))

}

textscreeplot<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    strong(" A  key question is how many clusters (k) should we define?"),
    br(),
    p("The scree plot panel helps you to determine the optimal number of clusters.  It is a heuristic graphic method that consists of:"),
    p(strong("a)"), "plotting an internal measure of the clustering performace againt the number of clusters and"),
    p(strong("b)"),"inspecting the shape of the resulting  curve  in  order  to  detect  the  point  at  which  the curve changes drastically."),
    p("Generally, you want to choose a number of clusters so that adding another cluster doesn't improve much better the internal measure of the clustering performace (i.e the steep slope),"),
    p("The panel",code("Scree plot"),"implements two tools (",code("WSS")," and ",code("RFaccu"),") for creating a scree plot "),
    p(code("WSS"), "method uses the ratio of number of clusters vs. Within Sum of Squares (WSS)", style="margin-left: 10px;"),
    p(code("RFaccu"), "method uses the number of clusters vs. Accuracies from Random Forest analysis", style="margin-left: 10px;"),
    p("Finding the 'elbow' may not be intuitive. So, the user can apply the ",code("split moving window analysis"),", which may help to identify the break in the elbow relationship, The split moving window analysis toolbox appears once the scree plot is generated and helds more details about this analyis.")
  )

}

textclustering<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    br(),
                    p("This step is intend to cluster the map units into classes, allowing to find the structure underlying the values associated with the map units after training. At the end of this procedure each observation belongs to a map unit, and each map unit belongs to a class (cluster)."),
                    p("In the", code("Hierarchical clustering"),"panel you can define the number of clusters, perform the Hierarchical clustering analysis and then inspect the subpanels",code("Dendogram"), "and", code("BMU clusters"))


                    )))
}



temptext<-function(...)
{
  p(br(),"The app implements two tools to help the user define the number of clusters, both based on the elbow method. One using the ratio of Number of clusters vs. Total Sum of Squares, and the other looking at Accuracies vs. the number of clusters. F")
}

textupload<-function(...){
  as.character(

    "csv file where rows are the observations, columns are the samples.  The first column must contain the observation labels. Columns containing characters are initially omitted and can later be included as binary columns by factor level."

  )}

textlab<-function(...)
{
  as.character(
    "csv file containg the factors for your data, which will be used for labeling, grouping and viewing the results. It can contain as many factors as you want."
  )
}

textinput<-function(...){
  paste(
    "Create Datalists and upload your data and their attributes. All analyses available at ",em('menv')," will require a Datalist created by the user, either uploaded or using example data."


  )
}


texttransf<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                  p("This functionality uses the",code("decostand"),

                    "function from the", actionLink("vegan","vegan"),"package."),
                  p(icon("fas fa-exclamation-circle"),"The ",em('Transformation')," methods are described in the function's ", actionLink("decostand","help page"),),



                  ),
           column(12,
                  htmlOutput("decostand")
                  )))
}



textpreProcess<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("Pre-processing transformation (centering, scaling etc.) can be estimated from the training data and applied to any data set with the same variables."),
                    p("Please see ",strong("Detail", style="color: SeaGreen")," section in",actionLink("preProcessh","preProcess"),'page for more Details')



    ),
    column(12,
           htmlOutput("preProcesshelp")
    )))
}

textscale<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the",
                      actionLink("scalehelph","scale") ,
                      "function from the", "R base.")
    ),
    column(12,
           htmlOutput("scalehelp_out")
    )))
}


textmdshelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the",
                      code("metaMDS") ,
                      "function from the 'vegan' package"),
                    p(icon("fas fa-exclamation-circle"),code("distance")," argument is described  in 'Detail' section of the ",actionLink("mdshelph","help page")," of the function;")
    ),
    column(12,
           htmlOutput("mdshelphelp")
    )))
}
textpcahelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the"
                      ,actionLink("pcahelph","prcomp") ,
                      "function from R base")
    ),
    column(12,
           htmlOutput("pcahelphelp")
    )))
}

textvotes<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),



    fluidRow(column(12,
                    p("This functionality uses the ",code('NbClust')," function from the",
                      code("NbClust"),"package."),

                    p(icon("fas fa-exclamation-circle"), "Arguments are described 'in the ",actionLink("votesh","help page")," of the function;")

    ),
    column(12,
           htmlOutput("voteshelp")
    )))
}

textsomgrid<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the ",code('somgrid')," function from the",
                      actionLink("kohonen","kohonen"),"package."),
                    p(icon("fas fa-exclamation-circle"),"All arguments are passed to the function;"),
                    p(icon("fas fa-exclamation-circle"), "Arguments are described 'in the ",actionLink("somgridh","help page")," of the function;")
    ),
    column(12,
           htmlOutput("somgridhelp")
    )))
}

textsupersom<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the ",code('som')," function from the",
                    actionLink("kohonen","kohonen"),"package."),
                    p(icon("fas fa-exclamation-circle"), "arguments are described 'in the ",actionLink("supersomh","help page")," of the function;")
    ),
    column(12,
           htmlOutput("supersomhelp")
    )))
}


textmap<-function(...)
{
  paste0("To vizualize the ",em('BMUs')," on the map, choose ",code('bmu')," To visualize a ",em('variable'),"  on the map (from your uploaded and pretreated data), choose ",code("variable"),"  and then select the target variable")



}

textinterp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the",
                      actionLink("interph","idw") ,
                      "function from the 'gstat' package.")
    ),
    column(12,
           htmlOutput("interphelp")
    )))
}
texthclust<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the",
                      code("hclust") ,
                      "function from the 'stats'"),
                    p(icon("fas fa-exclamation-circle"),code("methods"),"are described 'Detail section of the ",actionLink("hclusth","help page")," of the function;")
    ),
    column(12,
           htmlOutput("hclusthelp")
    )))
}
textrf<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    "In essence the Random Forest is based on generating a large number of decision trees, each constructed using a different subset of your training set. These subsets are selected by sampling at random and with replacement from the original data set. The decision trees are then used to identify a classification consensus by selecting the most common output (mode).",
                    p("This functionality uses the",
                      code("train"),"and",code("trainControl"),
                      "functions from the 'caret' package"),

                    p(icon("fas fa-exclamation-circle"),"Please visit their respective help pages (",actionLink("rfh","train"),"and",actionLink("rfh2","trainControl"),") for more details about the parameters available in iMESc")
    ),
    column(12,
           htmlOutput("rfhelp")
    )))
}


textctree<-function(...)
{

  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    "Decision Trees are a non-parametric supervised learning method used for classification and regression. The goal is to create a model that predicts the value of a target variable by learning simple decision rules inferred from the data features.",
                    p("This functionality uses the",
                      code("ctree"),"and",code("ctree_control"),
                      "functions from the 'caret' package"),

                    p(icon("fas fa-exclamation-circle"),"Please visit their respective help pages (",actionLink("ctreeh","ctree"),"and",actionLink("ctreeh2","ctree_control"),") for more details about the parameters")
    ),
    column(12,
           htmlOutput("ctreehelp")
    )))

}

textseed<-function(...){
  "A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the analysis."
}
textinterpbmu<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the",
                      actionLink("interpbmuh","knn") ,
                      "function from the 'class' package.")
    ),
    column(12,
           htmlOutput("interpbmuhelp")
    )))
}

textres<-function(...)
{
  "The resolution of the interpolation"
}

textdisplayas<-function(...)
{

  paste0("The option ",code('discrete')," generate the map using only the provided coordinates, whereas the option ",code('interpolation')," provide some tools for generating an interpolated surface on the map.")





}

textvarfacmap<-function(...)
{
  div(

    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(
      column(12,
             h4("Variable factor map"),
             p("The chart is very similar to the variable factor map obtained from the principal component analysis (PCA). It calculates the weighted correlation for each variable using the coordinates (x, y) of the neurons and their weights (number of instances). The codebooks vectors of the cells correspond to an estimation of the conditional averages, calculating their variance for each variable is equivalent to estimating the between-node variance of the variable, and hence their relevance."),
             p("The ",code("most important correlations")," option returns",code("npic")," variables with the highest variance, whereas ",code("clock-wise correlations")," returns",code("npic")," variables with the highest correlation considering along the different directions of the codebook")
      )

    )
  )
}


textcoords<-function(...){
  paste(
    em('Required only for the spatial tools menu:'),
    "csv file with the longitudes and latitudes of the observations. The first column must contain the name of the observations. The second and third columns must contain the logitude and latitude respectively"


  )
}


textbase<-function(...)
{
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    "))
    ,column(12,
         "A shapefile (class sf)  that will be used as the interpolation area (e. g an oceanic basin shape)",
  "Currently, the application only supports shapefiles generated by ",code("sf")," package and saved without extension (in R). In R, load the ",code("sf")," package and use the ",actionLink("baseh",code("st_read"))," function:",
  verbatimTextOutput("codechunk_base"),
  column(12,"Upload the generated 'my_base_shape_shape' file"),
  column(12,
         htmlOutput("basehelp"))

  ))
}

codebase<-function(...)
{
  cat(c("library('df')","\n",
  "my_base_shape_shape<-st_read('directory_containing_the_shapefiles/shape_name.shp')","\n",
  "save(my_base_shape_shape,
       file='directory_to_save_the_base_shapeshape/my_base_shape_shape')"))
}

codelayer<-function(...)
{
  cat(c("library('df')","\n",
        "my_layer_shape_shape<-st_read('directory_containing_the_shapefiles/shape_name.shp')","\n",
        "save(my_layer_shape_shape,
       file='directory_to_save_the_layer_shapeshape/my_layer_shape_shape')"))
}


textlayer<-function(...)
{
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    "))
    ,column(12,
            "A shapefile (class sf) to be used as an additional layer (e.g. a continent shape)",
            "Currently, the application only supports shapefiles generated by ",code("sf")," package and saved without extension (in R). In R, load the ",code("sf")," package and use the ",actionLink("layerh",code("st_read"))," function:",
            verbatimTextOutput("codechunk_layer"),
            column(12,"Upload the generated 'my_layer_shape_shape' file"),
            column(12,
                   htmlOutput("layerhelp"))

    ))
}





textremove<-function(...)
{
  column(12,
         p(code('singleton'),"Remove species occurring only once.",icon("fas fa-exclamation-circle"), "Requires a counting data"),
         p(code("pctRare"),"Remove species with an abundance less than pctRare% of total abundance."),
         p(code("pctPrev"),"Remove species occurring in less than  pctPrev% of the samples")
  )

         }



texttraining<-function(...)
{div(
  br(),
  br(),
  h3("Parametrization"),
  br(),
  h4(strong("Topology")),
  p("choose between a hexagonal or rectangular topology"),
  h4(strong("xdim, ydim")),
  p("dimensions of the grid."),
  h4(strong("Distance method")),
  p("Distance functions to be used for the data"),
  h4(strong('rlen')),
  p("The number of times the complete data set will be presented to the network."),
  h4(strong("seed")),
  p("A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the som analysis.")
)}


text_som_fine_tuning<-function(...){

  div(

    h3('finetuning'),
    br(),
    br(),
    h4(strong('a1, a2')),
    p("learning rate, indicating the amount of change. Default is to decline linearly from 0.05 to 0.01 over rlen updates. Not used for the batch algorithm."),
    h4(strong('neigh.fct')),
    p("choose between bubble and gaussian neighbourhoods when training a SOM."),


    h4(strong('r1, r2')),
    p("the radius of the neighbourhood, either given as a single number or a vector (start, stop). If it is given as a single number the radius will change linearly from radius to zero; as soon as the neighbourhood gets smaller than one only the winning unit will be updated."),
    h4(strong('mode')),
    p("type of learning algorithm")
  )


  }

datastr<-function(data){
  data_structure<-list( n.obs=nrow(data),
                        n.vars=ncol(data),
                        type.vars=table(unlist(lapply(data,function(x) class(x)))))
  return(data_structure)
}



pfac<-function(data.factors)
{
  m<-matrix(c(3:(ncol(data.factors)+2)), ncol=2)
  m<-matrix(c(3:(length(as.vector(m))+2))
            ,ncol=2, byrow = T)
  m<-cbind(2,m)
  m<-(rbind(1,m))
  layout(m, heights = c(2,rep(5,nrow(m)-1)), widths = c(.1))
  par(mar=c(0,0,0,0))
  plot.new()
  text(.5,.5,"", cex=2)
  plot.new()
  text(.5,.5,"number of observations",srt=90, cex=2)
  for(i in 1:ncol(data.factors)){
    dt<-table(data.factors[i])
    par(mar=c(1,2,1,0.5))
    barplot(as.matrix(dt), beside=F,legend.text = names(dt), col=rainbow(length(dt),s = .3), main=colnames(data.factors)[i],las=1)
  }
  plofac<-recordPlot()
  return(plofac)

}

pmds<-function(mds_data,keytext=NULL,key=NULL,points=T, text=NULL,palette="black", cex.points=1, cex.text=1, pch=16, textcolor="gray")
{
 if(!is.null(key)) {
  colkey<-getcolhabs(palette, nlevels(key))
  col<-colkey[key]} else{col= getcolhabs(palette, nrow(mds_data$points)) }
  opar<-par(no.readonly=TRUE)
  layout(matrix(c(1,2), nrow=1),widths = c(100,20))
  par(mar=c(5,5,4,1))
  plot(mds_data$points, pch=pch,  las=1, type="n", main="Multidimensional scaling")
  legend("topr",legend=c(paste("Stress:",round(mds_data$stress,2)), paste0("Dissimilarity:", "'",mds_data$distmethod,"'")),cex=.8, bty="n")
  if(is.null(points)==F){ points(mds_data$points, pch=pch, col=col, cex=cex.points)}

  if(is.null(text)==F){ text(mds_data$points, pch=pch, col=textcolor, labels=keytext, cex=cex.text)}
  if(!is.null(key)){
    par(mar=c(0,0,0,0))
    plot.new()
    colkey<-getcolhabs(palette, nlevels(key))
    legend("center",pch=pch,col=colkey, legend=levels(key),  cex=.8,  bg="gray95", box.col="white",xpd=T, adj=0)
  }
  on.exit(par(opar),add=TRUE,after=FALSE)
return(mds_data)


}



ppca<-function(data,key=NULL,keytext=NULL,points=T, text=NULL,palette="black", cex.points=1, cex.text=1, pch=16,textcolor="gray")
{

  {
    X   = as.matrix(data)
    CEN = X
    PCA = prcomp(CEN)

    comps<-summary(PCA)

    exp_pc1<-paste("PC I (",round(comps$importance[2,1],2)*100,"%", ")", sep="")
    exp_pc2<-paste("PC II (",round(comps$importance[2,2],2)*100,"%", ")", sep="")

    choices = 1:2
    scale = 1
    scores= PCA$x
    lam = PCA$sdev[choices]
    n = nrow(scores)
    lam = lam * sqrt(n)
    x = t(t(scores[,choices])/ lam)
    y = t(t(PCA$rotation[,choices]) * lam)
    n = nrow(x)
    p = nrow(y)
    xlabs = 1L:n
    xlabs = as.character(xlabs)
    dimnames(x) = list(xlabs, dimnames(x)[[2L]])
    ylabs = dimnames(y)[[1L]]
    ylabs = as.character(ylabs)
    dimnames(y) <- list(ylabs, dimnames(y)[[2L]])
    unsigned.range = function(x) c(-abs(min(x, na.rm = TRUE)),
                                   abs(max(x, na.rm = TRUE)))
    rangx1 = unsigned.range(x[, 1L])
    rangx2 = unsigned.range(x[, 2L])
    rangy1 = unsigned.range(y[, 1L])
    rangy2 = unsigned.range(y[, 2L])
    xlim = ylim = rangx1 = rangx2 = range(rangx1, rangx2)
    ratio = max(rangy1/rangx1, rangy2/rangx2)



  }



  if(!is.null(key)) {
    colkey<-getcolhabs(palette, nlevels(key))
    col<-colkey[key]} else{col= getcolhabs(palette, nrow(x)) }
  opar<-par(no.readonly=TRUE)
  layout(matrix(c(1,2), nrow=1),widths = c(100,20))
  par(pty = "s",mar=c(5,5,5,1))
  plot(x, type = "n", xlim = xlim, ylim = ylim, las=1, xlab=exp_pc1, ylab=exp_pc2, main="Principal Component Analysis")
  if(is.null(points)==F){
    points(x, pch=pch, col=col, cex=cex.points)
  }
  if(is.null(text)==F){
    text(x, pch=pch, col=textcolor, labels=keytext, cex=cex.text)
  }

  {

    par(new = TRUE)
    xlim = xlim * ratio*2
    ylim = ylim * ratio
    plot(y, axes = FALSE, type = "n",
         xlim = xlim,
         ylim = ylim, xlab = "", ylab = "")
    axis(3,padj=1, tck=-0.01); axis(4, las=1)
    boxtext(x =y[,1], y = y[,2], labels = rownames(y), col.bg = adjustcolor("white", 0.2),  cex=1, border.bg  ="gray80", pos=3)

    #text(y, labels = ylabs, font=2, cex=.8, col=)
    arrow.len = 0.1
    arrows(0, 0, y[, 1L] * 0.8, y[, 2L] * 0.8,
           length = arrow.len, col = 2, lwd=1.5)

  }

  if(!is.null(key)) {
    par(mar=c(0,0,0,0))
    plot.new()
    colkey<-getcolhabs(palette, nlevels(key))
    legend("center",pch=pch,col=colkey, legend=levels(key),  cex=.8,  bg="gray95", box.col="white",xpd=T, adj=0)
  }

  on.exit(par(opar),add=TRUE,after=FALSE)
  return(PCA)
  }

str_factors<-function(factors, palette="viridis")
{


  m<-matrix(1:(ncol(factors)*3), ncol=3)
  m=m+3
  m<-rbind(1:3, m)
  layout(m, widths = c(.3,.3,.3))
  opar<-par(no.readonly=TRUE)
  plot.new()
  text(.5,.5,"Variable")
  plot.new()
  text(0.5,.5,"Levels")
  plot.new()
  text(.5,.5,"Distribution")





  for(i in 1: ncol(factors))
  {
    plot.new()
    text(.5,.5,colnames(factors)[i])
  }
  for(i in 1: ncol(factors))
  {
    plot.new()
    text(  seq(0.1,0.9, length.out=nlevels(factors[,i])),.5,summary(factors[,i]))
  }
  for(i in 1:ncol(factors))
  {

    barplot(as.matrix(table(x[,i])), horiz=T,ann=F, axes=F, col=getcolhabs("viridis", nlevels(x[,i])))

  }


  lapply(factors, function(x){})

}

str_numerics<-function(numerics)
{


  par(mar=c(0,0,0,0), cex=1.2)
  m<-matrix(1:(ncol(numerics)*3), ncol=3)
  m=m+3
  m<-rbind(1:3, m)
  layout(m, widths = c(.2,.35,.35))
  opar<-par(no.readonly=TRUE)
  plot.new()
  text(.5,.5,"Variable")
  plot.new()
  text(  seq(0.1,0.9, length.out=6),.5,c('Min.','1st Qu.','Median','Mean','3rd Qu. ',' Max.'))
  plot.new()
  text(.5,.5,"Histogram")





  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,colnames(numerics)[i])
  }
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(  seq(0.1,0.9, length.out=6),.5,round(summary(numerics[,i]),3))
  }



  lapply(numerics, function(x){
    hist(x, ann=F, axes=F)
  })

}

getclassmat<-function(data.factors)
{
  res<-lapply(data.factors,classvec2classmat)
  namesfac<-names(res)
  colnamesfac<-lapply(res,colnames)
  names<-list()
  for( i in 1:length(namesfac))
  {
    names[[i]]<-paste(namesfac[i], colnamesfac[[i]], sep="_")
  }
  classmat<-do.call("cbind",res)
  colnames(classmat)<-unlist(names)
  return(classmat)
}




getHelp <- function(fun){
  temp = tools::Rd2HTML(gbRd::Rd_fun(fun),out = tempfile("docs"))
  content = readr::read_file(temp)
  file.remove(temp)
  content
}

textrfloop<-function(...)
{
  div(
    p("Compute the accuracies using random forest algorithm for increasing number of clusters (k):"),
    p(style="margin-left: 5px;",code("*"),"For each k, calculate the accuracy;"),
    p(style="margin-left: 10px;",code("*"),"Plot the curve of accuracies according to the number of clusters k;"),
    p(style="margin-left: 5px;",code("*"),"The location of a bend (knee) in the plot is generally considered as an indicator of the appropriate number of clusters.")
  )
}
textelbow<-function(...)
{

  div(
    p("Compute the Within-Sum-of-Squares (WSS) for increasing number of clusters (k):"),
    p(style="margin-left: 5px;",code("*"),"For each k, calculate the WSS;"),
    p(style="margin-left: 5px;",code("*"),"Plot the curve of WSS according to the number of clusters k;"),
    p(style="margin-left: 5px;",code("*"),"The location of a bend (knee) in the plot is generally considered as an indicator of the appropriate number of clusters.")
  )
}





textsugtopohelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    'The number of map nodes and the side length ratio is performed with the following steps (Vesanto, 2000 ):',
                    column(12, style="margin-left: 10px; margin-top: 5px;",
                           p(strong("1."),"Determine the number of map nodes using the heuristic recommendation:",withMathJax(helpText(
                             "$$ M = 5{\\sqrt{N}}$$"
                           )),"where N is the number of observations in the input data set ( Vesanto, 2000 ),"),
                           p(strong("2."),"Determine the eigenvectors and eigenvalues in the data from the autocorrelation matrix,"),
                           p(strong("3."),"Set the ratio between the two sides of the grid equivalent to the ratio between the two largest eigenvalues, and "),
                           p(strong("4."),"Scale the side lengths so that their product (xdim * ydim) is as close as possible to the number of map units determined above."))
    )))
}




textsmw<-function(...)
{
  "Performs the split moving window analysis for several windows sizes, calculates the mean dissimilarity profiles, and considers as significant the dissimilarities that exceed mean plus one standard deviation. Click for more information"

}

textrfbphelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p(strong("Dissimilarity profile of the moving split-window analysis:")),
                    p(strong("1."),"Placing a window of even-numbered size at the beginning of the data series"),
                    div(style="margin-left: 5px;",
                        p(strong("2."),"splitting the window into two equal halves"),
                        p(strong("3."),"computing a dissimilarity  between the two halves"),
                        p(strong("4."),"shifting window one position along the series, and"),
                        p(strong("5.")," repeating the procedure till the end of the data series  (Cornelius & Reynolds 1991)")
                    )




    )
    ))
}


textbphelp<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p(strong("Dissimilarity profile of the moving split-window analysis:")),
                    p(strong("1."),"Placing a window of even-numbered size at the beginning of the data series"),
                    div(style="margin-left: 5px;",
                        p(strong("2."),"splitting the window into two equal halves"),
                        p(strong("3."),"computing a dissimilarity  between the two halves"),
                        p(strong("4."),"shifting window one position along the series, and"),
                        p(strong("5.")," repeating the procedure till the end of the data series  (Cornelius & Reynolds 1991)")
                    )




    )
    ))
}





divI<-function(abund,choices=c("N","S","margalef","D","H","J","Dom","Skewness")){
  res=list()
  if("N"%in%choices){res$N<-rowSums(abund)}
  if("S"%in%choices){res$S<-vegan::specnumber(abund)}
  if("margalef"%in%choices){res$margalef=(specnumber(abund)-1)/log(rowSums(abund))}
  if("D"%in%choices){res$D<-vegan::diversity(abund, index="simpson")}
  if("H"%in%choices){res$H<-vegan::diversity(abund)}
  if("J"%in%choices){
    H<-vegan::diversity(abund)
    S<-vegan::specnumber(abund)
    res$J <- H/log(S)}
  if("Dom"%in%choices){res$Dom<-apply(decostand(abund, "total"),1,sort,T)[1,]}
  if("Skewness"%in%choices){res$Skewness=apply(abund,1,skewness)}
  return(data.frame(do.call(cbind,res)))
}

