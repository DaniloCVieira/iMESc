




#' @noRd

imesc_spinner <- function(output_ui) {

  tagList(
    # Spinner and overlay
    tags$div(
      class = "imesc-spinner-container",
      tags$div(class = "imesc-loading-overlay"),
      tags$div(
        id = paste0(output_ui[[2]]$attribs$id, "-spinner"), # Ensure spinner is unique
        class = "imesc-spinner",
        tags$img(src = "imesc_logo.png", class = "spinner-image")
      )
    ),
    output_ui
  )
  }


#' @export
app_ui<-ui<-function(request) {

  tagList(
    includeCSS("inst/www/styles.css"),
    includeCSS("inst/www/styles2.css"),
    includeCSS("inst/www/styles3.css"),
    tags$head(
      tags$title("iMESc") # Define o título da janela do navegador
    ),
    tags$head(
      tags$link(rel = "icon", type = "image/x-icon", href = "imesc_logo.ico") # Define o favicon
    ),
    tags$head(
      tags$style(
        HTML("


        /* Spinner Style for Image */
        .load-container img {
  width: 40px; /* Adjust size as needed */
    height: 40px;
  animation: colorChange 2s linear infinite; /* Rotate the image */
        }

        #spinner {
          position: fixed;
          top: 50%;
          left: 50%;
          transform: translate(-50%, -50%);
          z-index: 1000;
          display: none; /* Initially hidden */
        }

        .colorChange-image {
          width: 100px; /* Adjust size as needed */
          height: 100px;
          animation: colorChange 2s linear infinite; /* Rotate the image */
        }
        .loadmess0{
         animation: loadmess 2s linear infinite; /* Rotate the image */
        }
        .loadmess{
         0% { filter: brightness(0.5); } /* Dimmed */
  50% { filter: brightness(1.5); } /* Bright */
  100% { filter: brightness(0.5); } /* Back to dimmed */
        }

      @keyframes colorChange {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}

        /* Dim the background while loading */
        .loading-overlay {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background-color: white;
          z-index: 999;
          display: none;
        }
      ")
      ),
      tags$script(
        HTML("
        document.addEventListener('DOMContentLoaded', function() {
          const spinner = document.getElementById('spinner');
          const overlay = document.getElementById('loading-overlay');

          // Show spinner and overlay while the app is loading
          spinner.style.display = 'block';
          overlay.style.display = 'block';

          // Hide spinner and overlay when shiny is fully loaded
          Shiny.addCustomMessageHandler('hideSpinner', function(message) {
            spinner.style.display = 'none';
            overlay.style.display = 'none';
          });
        });
      ")
      )
    ),
    tags$script(
      HTML("
    Shiny.addCustomMessageHandler('show_spinner', function(id) {
      const spinner = document.getElementById(id + '-spinner');
      const overlay = spinner.previousElementSibling;
      spinner.style.display = 'block';
      overlay.style.display = 'block';
    });

    Shiny.addCustomMessageHandler('hide_spinner', function(id) {
      const spinner = document.getElementById(id + '-spinner');
      const overlay = spinner.previousElementSibling;
      spinner.style.display = 'none';
      overlay.style.display = 'none';
    });
  ")
    ),
    tags$script(
      HTML("
    Shiny.addCustomMessageHandler('update_loading_message', function(message) {
      const messageDiv = document.getElementById('loading-message');
      messageDiv.innerHTML = message; // Allow inserting HTML content
    });
  ")
    ),




    tags$div(id = "loading-overlay", class = "loading-overlay"),
    tags$div(
      id = "spinner",
      tags$img(src = "imesc_logo.png", class = "colorChange-image"),
      div(
        strong("Loading iMESc:",style="color: royalblue;font-size: 14px;font-weight: bold;"),
        div(
          class = "loadmess0",
          id = "loading-message",
          "initializing environment...",
          style="display: inline-block;font-style: italic; color: SeaGreen",

        )
      )
    ),

    shinydashboardPlus::dashboardPage(
      skin = "blue",
      shinydashboardPlus::dashboardHeader(),
      shinydashboardPlus::dashboardSidebar(
        shinyjs::useShinyjs(),
        #use_cicerone(),
        #introjsUI(),
        width = "17%",

        shinydashboard::sidebarMenu(
          id = "tabs",
          div(id = "sidebar-main",

              # uiOutput("style_act"),
              div(style = "background-color:  #05668D; font-size: 13px",class = "sidebar-menu",
                  shinydashboard::menuItem(tabName = "menu_intro",
                                           selected =T,
                                           icon=icon(
                                             name = NULL,
                                             class="imesc-logo-icon"
                                           ),
                                           span(class="home",
                                                div(class="side_menu_imesc",

                                                    span(class="span_menu_imesc",
                                                         img(src = imesc_name,height = '20px',width = '70px'))
                                                )
                                           )),
                  shinydashboard::menuItem(tabName = "menu_upload",
                                           "Data Bank",
                                           icon=icon("fas fa-database")
                  ),
                  shinydashboard::menuItem(tabName = "menu_explore",
                                           "Descriptive tools",
                                           icon=icon("fas fa-binoculars")
                  ),
                  shinydashboard::menuItem(tabName = "menu_maps2",
                                           "Spatial Tools",
                                           icon=icon("fas fa-map")
                  ),
                  shinydashboard::menuItem(tabName = "menu_div",
                                           "Biodiversity tools",
                                           icon=icon(
                                             name = NULL,
                                             class="custom_side-icon div-icon"
                                           )
                  ),

                  shinydashboard::menuItem(expandedName="unsup_expand",
                                           "Unsupervised Algorithms",
                                           icon=icon(
                                             name = NULL,
                                             class="custom_side-icon unsup-icon"
                                           ),
                                           shinydashboard::menuSubItem(tabName = "menu_som",
                                                                       "Self-Organizing Maps",
                                                                       icon=icon("fas fa-braille")
                                           ),
                                           shinydashboard::menuSubItem(tabName = "menu_hc",
                                                                       "Hierarchical Clustering",
                                                                       icon=icon("fas fa-network-wired")
                                           ),
                                           shinydashboard::menuSubItem(tabName = "menu_kmeans",
                                                                       "K-means",
                                                                       icon=icon(
                                                                         name = NULL,
                                                                         class="custom_side-icon kmeans-icon"
                                                                       ))
                  ),
                  shinydashboard::menuItem(tabName = "menu_sl",
                                           "Supervised Algorithms",
                                           icon=icon(
                                             name = NULL,
                                             class="custom_side-icon sup-icon"
                                           )
                  ),
                  shinydashboard::menuItem(tabName ="menu_compare",
                                           "Compare Models",
                                           icon=icon(
                                             name = NULL,
                                             class="custom_side-icon comp-icon"
                                           )

                  )
                  # menuItem(icon=NULL,tabName ="menu_keras",div(class="needed",id="keras_menu",icon =NULL,div(class="side_menu",span(class="side_img"),span(class="span_menu","Keras")))),

              )
          )
        )
      ),
      shinydashboard::dashboardBody(


        tags$script(jscode_screen),
        tags$head(
          tags$link(
            rel = "stylesheet",
            href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"
          )
        ),
        tags$head( tags$script(src = "https://kit.fontawesome.com/7af7c9478d.js")),
        tags$head(tags$script(HTML("$(document).on('.dataTables_filter').addClass('pull-left');"))),
        tags$head(tags$script(HTML(js_getid))),
        tags$head(tags$script(HTML('
        $(document).on("keydown", function (e) {
        Shiny.onInputChange("lastkeypresscode", e.keyCode);
        });
        '))),

        tags$script("$(document).on('click', '.name-opt a', function (evt) {
  evt.preventDefault(); Shiny.setInputValue('name',this.dataset.name);
});"
        ),

        tags$script("$(document).on('shiny:connected', function(event) {
var myWidth = $(window).width();
Shiny.onInputChange('shiny_width',myWidth)
});"),

        tags$script("$(document).on('shiny:connected', function(event) {
var myHeight = $(window).height();
Shiny.onInputChange('shiny_height',myHeight)

});"),

        div(
          uiOutput('tools_upload'),
          shinybusy::add_busy_spinner(spin = "hollow-dots",height="20px", color="yellow",width="20px", margins = c(15, 700),position="top-left"),
          div(
            style="z-index: 9",
            div(
              id="imesc_main",class="needed",
              uiOutput("menutitle"),
              div(
                class="imesc-main",
                style=" position: absolute; right: 0px",
                tabsetPanel(
                  type ="hidden",
                  tabPanel(
                    title=NULL,
                    shinydashboard::tabItems(
                      shinydashboard::tabItem(
                        tabName = "menu_intro",
                        column(12,

                               uiOutput("menu_intro_out")
                        )
                      ),
                      shinydashboard::tabItem(
                        tabName = "menu_upload",
                        column(12,
                               uiOutput("validate_databank"),
                               div(id="module_databank",
                                 imesc_withSpinner(uiOutput('menu_bank_out'),caption="Data Bank")
                               )
                        )
                      ),
                      shinydashboard::tabItem(
                        tabName = "menu_explore",
                        column(12,
                               uiOutput("validate_desctools"),
                               div(
                                 id="module_desctools",
                                 imesc_withSpinner(uiOutput('menu_upload_out'),caption="Descriptive Tools")
                               )
                        )
                      ),
                      shinydashboard::tabItem(
                        tabName = "menu_div",
                        column(12,
                               uiOutput("validate_divtools"),
                               div(
                                 id="module_divtools",
                                 imesc_withSpinner(uiOutput('menu_div_out'),caption="Diversity Tools")
                               ))
                      ),
                      shinydashboard::tabItem(
                        tabName = "menu_maps2",
                        column(12,
                               uiOutput("validate_spatialtools"),
                               div(
                                 id="module_spatialtools",
                                 imesc_withSpinner(uiOutput('map_panels'),caption="Spatial Tools")
                               ),
                        )
                      ),
                      shinydashboard::tabItem(
                        tabName = "menu_som",
                        column(12,
                               uiOutput("validate_supersom"),
                               div(
                                 id="module_supersom",
                                 imesc_withSpinner(uiOutput('menu_som_out'),caption="Self-Organizing Maps")
                               ))
                      ),
                      shinydashboard::tabItem(
                        tabName = "menu_hc",
                        column(12,
                          uiOutput("validate_hc"),
                          div(
                            id="module_hc",
                            imesc_withSpinner(uiOutput('menu_hc_out'),caption="Hierarchical Clustering")
                          )

                        )
                      ),
                      shinydashboard::tabItem(
                        tabName = "menu_kmeans",
                        column(12,
                               uiOutput("validate_kmeans"),
                               div(
                                 id="module_kmeans",
                                 imesc_withSpinner(uiOutput('menu_kmeans_out'),caption="K-Means")
                               )
                        )
                      ),

                      shinydashboard::tabItem(
                        tabName = "menu_sl",
                        column(12,
                               uiOutput("validate_sl"),

                               div(
                                 id="module_sl",
                                 imesc_withSpinner(uiOutput("menu_sl_out"),caption="Supervised Algorithms")
                               )

                        )
                      ),

                      shinydashboard::tabItem(
                        tabName = "menu_compare",
                        uiOutput("validate_comp"),

                        div(
                          id="module_compare",
                          imesc_withSpinner(uiOutput('menu_compare_out'),caption="Compare Models")
                        )
                      ),

                      shinydashboard::tabItem(
                        tabName = "menu_keras",
                        #module_keras$ui("keras"),
                        imesc_withSpinner(uiOutput('menu_keras_out'))
                      )



                    )
                  )
                )

              )

            )
            ,

            div( tags$head(
              tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.15.4/css/all.css")
            ),

            class="preproc",
            uiOutput("preprocessing"),

            div(
              class="needed toolbar preproc_page tool_toggle disabled_toolbar",style="background: none",
              navbarPage(
                NULL,id="disable_tool",
                tabPanel(value='1',
                         div(class="dis_tool")),
                tabPanel(value='2',
                         div(class="dis_tool")),
                tabPanel(value='3',
                         div(class="dis_tool")),
                tabPanel(value='4',
                         div(class="dis_tool")),
                tabPanel(value='5',
                         div(class="dis_tool")),
                tabPanel(value='6',
                         div(class="dis_tool")),
                tabPanel(value='7',
                         div(class="dis_tool")),
                tabPanel(value='8',
                         div(class="dis_tool")
                ),

                tabPanel(value='9',
                         div(class="dis_tool")
                ),

                tabPanel(value='10',
                         div(class="tooltip_off",
                             actionButton("savepoint_out",class="tool",div(cogs_title="Savepoint",icon(verify_fa=FALSE,name=NULL,class="fas fa-thumbtack"))))
                )
              )

            ),
            shinyBS::bsTooltip("disable_tool", HTML("<span id=\\'offtip\\'><i class=\\'fas fa-ban\\'></i><span>Create a Datalist or load a savepoint to enable the pre-processing tools</span></span>")),

            # div(ban="\\f05e",class="mydiv",id="pp_block",div(class="mydiv2",style="color:gray; width: 100%; height:100%;"),style="position: fixed;  background: #9c9c9c40; right: 50px; width: 330px;top: 15px; height: 30px;z-index:9999"),


            pre_process$ui("preproc"),
            uiOutput('preprocess')
            )


          )
        ) ),
      footer=shinydashboardPlus::dashboardFooter(
        left=div(style="display: flex",
                 div(style="width: 17%; display: inline-block; text-align: center",
                     #HTML("<a href='https://danilocvieira.github.io/iMESc_help' target='_blank'>iMESc<sup>help!</sup></a>"),
                     actionLink('imesc_help_link_side',HTML("iMESc<sup>help!</sup>"))
                 ),

                 div(style="width: 80%; display: inline-block; text-align: center",span(em("Version:"),paste0(version,';'),em("Last update:"),last_update)),
                 div(style="width: 4%; display: inline-block; text-align: right",
                     uiOutput('footer_qsave'))
        )
      )),

    )
}
