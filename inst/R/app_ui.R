


#' @import shiny shinyBS shinybusy data.table
#' @importFrom shinydashboardPlus dashboardFooter dashboardSidebar
#' @importFrom shinydashboard sidebarMenu menuItem menuSubItem dashboardBody tabItems tabItem dashboardHeader

#' @importFrom shinyWidgets updatePickerInput pickerInput radioGroupButtons updateRadioGroupButtons dropdownButton tooltipOptions toggleDropdownButton switchInput
#' @importFrom shinyjs hide show hidden addClass removeClass toggle runjs toggleState extendShinyjs useShinyjs delay onclick
#roxygen2::roxygenise()

#' @noRd
library(shinyBS)

version<-"1.0.3"
#' @export
app_ui<-function(request) {

  tagList(

    includeCSS("inst/www/styles.css"),
    includeCSS("inst/www/styles2.css"),
    includeCSS("inst/www/styles3.css"),
    tags$header(tags$style(HTML(
      "




      "
    ))),


    shinydashboardPlus::dashboardPage(
      scrollToTop=T,
      skin = "blue",

      shinydashboardPlus::dashboardHeader(),
      dashboardSidebar(
        shinyjs::useShinyjs(),
        #use_cicerone(),
        #introjsUI(),
        width = "17%",

        sidebarMenu(
          id = "tabs",
          div(id = "sidebar-main",

              # uiOutput("style_act"),
              div(style = "background-color:  #05668D; font-size: 13px",class = "sidebar-menu",
                  menuItem(tabName = "menu_intro",
                           selected =T,
                           icon=icon(
                             name = NULL,
                             class="imesc-logo-icon"
                           ),
                           span(class="home",
                                div(class="side_menu_imesc",

                                    span(class="span_menu_imesc",
                                         img(src = imesc_icon,height = '20px',width = '70px'))
                                )
                           )),
                  menuItem(tabName = "menu_upload",
                           "Data Bank",
                           icon=icon("fas fa-database")
                  ),
                  menuItem(tabName = "menu_explore",
                           "Descriptive tools",
                           icon=icon("fas fa-binoculars")
                  ),
                  menuItem(tabName = "menu_maps2",
                           "Spatial Tools",
                           icon=icon("fas fa-map")
                  ),
                  menuItem(tabName = "menu_div",
                           "Biodiversity tools",
                           icon=icon(
                             name = NULL,
                             class="custom_side-icon div-icon"
                           )
                  ),

                  menuItem(expandedName="unsup_expand",
                           "Unsupervised Algorithms",
                           icon=icon(
                             name = NULL,
                             class="custom_side-icon unsup-icon"
                           ),
                           menuSubItem(tabName = "menu_som",
                                       "Self-Organizing Maps",
                                       icon=icon("fas fa-braille")
                           ),
                           menuSubItem(tabName = "menu_hc",
                                       "Hierarchical Clustering",
                                       icon=icon("fas fa-network-wired")
                           ),
                           menuSubItem(tabName = "menu_kmeans",
                                       "K-means",
                                       icon=icon(
                                         name = NULL,
                                         class="custom_side-icon kmeans-icon"
                                       ))
                  ),
                  menuItem(tabName = "menu_sl",
                           "Supervised Algorithms",
                           icon=icon(
                             name = NULL,
                             class="custom_side-icon sup-icon"
                           )
                  ),
                  menuItem(tabName ="menu_compare",
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
      dashboardBody(

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
    shinybusy::add_busy_spinner(spin = "hollow-dots",height="20px", color="yellow",width="20px", margins = c(15, 600),position="top-left"),
    div(
      div(
        div(style="z-index: 9",
            div(id="imesc_main",class="needed",
                uiOutput("menutitle"),
                div(class="imesc-main",
                    style="padding-top: 40px; position: absolute; right: 0px",
                    tabsetPanel(
                      type ="hidden",
                      tabPanel(
                        title=NULL,
                        tabItems(
                          tabItem(tabName = "menu_intro",
                                  column(12,uiOutput("menu_intro_out"))),
                          tabItem(tabName = "menu_upload",
                                  column(12,
                                         div(

                                           databank_module$ui("databank"),
                                           uiOutput('menu_bank_out')

                                         )

                                  )),
                          tabItem(tabName = "menu_explore",
                                  column(12,desctools$ui("module_desctools"),
                                         uiOutput('menu_upload_out'))),
                          tabItem(tabName = "menu_div",
                                  column(12,
                                         diversity_tool$ui("module_div"),
                                         uiOutput('menu_div_out'))),
                          tabItem(tabName = "menu_maps2",
                                  column(12,
                                         ll_data$ui("map_data"),
                                         uiOutput('map_header'),
                                         div(

                                           hidden(uiOutput("invalid_data_map")),
                                           llet$ui("llet")
                                         ))),
                          tabItem(tabName = "menu_som",
                                  column(12,

                                         uiOutput('menu_som_out'))),
                          tabItem(tabName = "menu_hc",
                                  column(12, hc_module$ui("hc"),
                                         uiOutput('menu_hc_out'))),
                          tabItem(tabName = "menu_kmeans",
                                  column(12,
                                         k_means_module$ui("module_kmeans"),
                                         uiOutput('menu_kmeans_out')
                                  )),

                          tabItem(tabName = "menu_sl",
                                  column(12,
                                         caret_models$ui("sl_models"),
                                         uiOutput("menu_sl_out")
                                  )),

                          tabItem(tabName = "menu_compare",
                                  module_compare$ui("module_comp"),
                                  uiOutput('menu_compare_out')),

                          tabItem(tabName = "menu_keras",
                                  #module_keras$ui("keras"),
                                  uiOutput('menu_keras_out'))


                        )
                      )
                    )
                )
            )
        ),
        tags$style(HTML("

                        #disable_tool > li:nth-child(-n+9) {
                        pointer-events: none;
                        }
                       .disabled_toolbar .navbar-default{
                        background: none

                       }
                       #main_panel > div > div.preproc > div.needed.toolbar.preproc_page.tool_toggle.disabled_toolbar > nav  {
background: transparent !important;
border: 0px
                       }
.disabled_toolbar .tooltip-inner {
background: none;
border: 0px;
color: gray;
font-style: italic;
background: #9c9c9c40;
max-width: 280px;
text-align: center
}
.disabled_toolbar .tooltip-inner i {
color: #a17f1a;
padding-right: 5px;
font-size: 14px

}


                       #main_panel > div > div.preproc > div.needed.toolbar.preproc_page.tool_toggle.disabled_toolbar > nav > div > div {

min-height:70px;

}
                        ")),
div( tags$head(
  tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.15.4/css/all.css")
),

class="preproc",
pre_process$ui("preproc"),
div(class="needed toolbar preproc_page tool_toggle disabled_toolbar",style="background: none",

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
                   actionButton("savepoint_out",class="tool",
                                div(cogs_title="Savepoint",
                                    icon(verify_fa=FALSE,name=NULL,class="fas fa-thumbtack"))
                   )
               )
      )
    )),
bsTooltip("disable_tool", HTML("<span id=\\'offtip\\'><i class=\\'fas fa-ban\\'></i><span>Create a Datalist or load a savepoint to enable the pre-processing tools</span></span>")),

# div(ban="\\f05e",class="mydiv",id="pp_block",div(class="mydiv2",style="color:gray; width: 100%; height:100%;"),style="position: fixed;  background: #9c9c9c40; right: 50px; width: 330px;top: 15px; height: 30px;z-index:9999"),

uiOutput('preprocess'))

      )
    )
) ),
footer=shinydashboardPlus::dashboardFooter(left=div(style="display: flex",
                                                    div(style="width: 17%; display: inline-block; text-align: center",
                                                        HTML("<a href='https://danilocvieira.github.io/iMESc_help' target='_blank'>iMESc<sup>help!</sup></a>")),
                                                    div(style="width: 80%; display: inline-block; text-align: center",span(em("Version:"),paste0(version,';'),em("Last update:"),last_update)),
                                                    div(style="width: 4%; display: inline-block; text-align: right",
                                                        uiOutput('footer_qsave'))
))))
}
