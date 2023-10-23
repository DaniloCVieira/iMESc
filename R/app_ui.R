#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.
##
#version<-packageVersion("imesc")
version<-"1.0.0"
app_ui <- function(request) {
  tagList(
    includeCSS("inst/app/www/styles.css"),
    tags$script(HTML("
  $(document).ready(function(){
    var sidebar = $('.sidebar');
    var expand = $('#sidebarItemExpanded > ul');

    function checkScrollPosition() {
      var contentHeight =  expand[0].scrollHeight;
      var viewHeight = sidebar.height();
      var scrollTop = sidebar.scrollTop();
      Shiny.setInputValue('sidebar_height', viewHeight);
      Shiny.setInputValue('sidebar_content_height', contentHeight);

      $('#scrollButton').toggle(scrollTop > 0);
      $('#scrollDownButton').toggle(viewHeight < contentHeight);
    }

    function checkElementExistence() {
      var elementExists = $('.sidebar-menu li.treeview.active').length > 0;
      Shiny.setInputValue('element_exists', elementExists);
    }

    var observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
        if (mutation.attributeName === 'class') {
          checkElementExistence();
        }
      });
    });

    var target = document.querySelector('.sidebar-menu');
    if (target) {
      observer.observe(target, {
        attributes: true,
        subtree: true // Permite observar as mutações nos descendentes do 'target'
      });
    }

    sidebar.scroll(checkScrollPosition);

    $(document).on('shiny:inputchanged', function(event) {
      if (event.name === 'element_exists') {
        checkScrollPosition();
      }
    });

 $('#scrollButton').click(function() {
  var currentScrollTop = sidebar.scrollTop();
  sidebar.animate({scrollTop: currentScrollTop - 40}, 'fast');
});

$('#scrollDownButton').click(function() {
  var currentScrollTop = sidebar.scrollTop();
  sidebar.animate({scrollTop: currentScrollTop + 40}, 'fast');
});
  });
"))

,



shinydashboardPlus::dashboardPage(
  scrollToTop=T,
  skin = "blue",

  shinydashboardPlus::dashboardHeader(controlbarIcon=NULL,disable=T),
  dashboardSidebar(
    actionLink("scrollButton", icon("fas fa-caret-up")),
    extendShinyjs(text = "shinyjs.collapse = function(boxid) {$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();}",functions = 'collapse'),
    useShinyjs(),
    #use_cicerone(),
    #introjsUI(),
    width = "16%",
    sidebarMenu(
      id = "tabs",
      div(id = "sidebar-main",
          uiOutput("style_act"),
          div(style = "background-color:  #05668D; font-size: 13px",class = "sidebar-menu",
              menuItem(tabName = "menu_intro",
                       div(class="home",
                           div(class="side_menu_imesc",
                               span(class="side_img_imesc",
                                    img(src = b64,height = '32px',width = '32px')),
                               span(class="span_menu_imesc",
                                    img(src = imesc_icon,height = '20px',width = '70px'))
                           )
                       ),
                       selected =T),


              menuItem(tabName = "menu_upload",
                       div(class="side_menu",
                           span(class="side_img",
                                icon("fas fa-database",style="height:20px;width: 20px")),
                           span(class="span_menu","Data Bank")
                       )),
              menuItem(tabName = "menu_explore",
                       div(class="side_menu",
                           span(class="side_img",
                                icon("fas fa-binoculars",style="height:20px;width: 20px")),
                           span(class="span_menu","Descriptive tools")
                       )),
              menuItem(tabName = "menu_maps",
                       div(class="side_menu",
                           span(class="side_img",
                                icon("fas fa-map",style="height:20px;width: 20px")),
                           span(class="span_menu","Spatial tools")
                       )),

              menuItem(tabName = "menu_div",
                       div(class="side_menu",
                           span(class="side_img",
                                img(src=div_icon,height='20px',width='20px')),
                           span(class="span_menu","Biodiversity tools")
                       )),



              menuItem(expandedName="unsup_expand",
                       div(class="needed",
                           icon =NULL,
                           div(class="side_menu",
                               span(class="side_img",img(src=unsup_icon,height='20',width='20')),
                               span(class="span_menu","Unsupervised Algorithms")
                           )
                       ),
                       menuSubItem(tabName = "menu_som",
                                   icon=NULL,
                                   div(class="side_menu",
                                       span(class="side_img",
                                            icon("fas fa-braille",style="height:20px;width: 20px")),
                                       span(class="span_menu","Self-Organizing Maps")
                                   )),
                       menuSubItem(tabName = "menu_hc",
                                   icon=NULL,
                                   div(class="side_menu",
                                       span(class="side_img",
                                            icon("fas fa-network-wired",style="height:20px;width: 20px")),
                                       span(class="span_menu","Hierarchical Clustering")
                                   )),
                       menuSubItem(tabName = "menu_kmeans",
                                   icon=NULL,
                                   div(class="side_menu",
                                       span(class="side_img",
                                            img(src=kmeans_icon,height='20px',width='20px')),
                                       span(class="span_menu","K-means")
                                   ))
              ),
              menuItem(expandedName="sup_expand",
                       icon=NULL,
                       div(class="needed",id="sup_menu",
                           icon =NULL,
                           div(class="side_menu",
                               span(class="side_img",img(src=sup_icon,height='20',width='20')),
                               span(class="span_menu","Supervised Algorithms")
                           )
                       ),

                       menuSubItem(tabName = "menu_nb",
                                   icon=NULL,
                                   div(class="side_menu",
                                       span(class="side_img",
                                            img(src=nb_icon,height='20px',width='20px')),
                                       span(class="span_menu","Naive Bayes")
                                   )),
                       menuSubItem(tabName = "menu_svm",
                                   icon=NULL,
                                   div(class="side_menu",
                                       span(class="side_img",
                                            img(src=svm_icon,height='20px',width='20px')),
                                       span(class="span_menu","Support Vector Machine")
                                   )),
                       menuSubItem(tabName = "menu_knn",
                                   icon=NULL,
                                   div(class="side_menu",
                                       span(class="side_img",
                                            img(src=knn_icon,height='20px',width='20px')),
                                       span(class="span_menu","K-nearest neighbor")
                                   )),
                       menuSubItem(tabName = "menu_rf",
                                   icon=NULL,
                                   div(class="side_menu",
                                       span(class="side_img",
                                            img(src=rf_icon,height='20px',width='20px')),
                                       span(class="span_menu","Random Forest")
                                   )),
                       menuSubItem(tabName = "menu_sgboost",
                                   icon=NULL,
                                   div(class="side_menu",
                                       span(class="side_img",
                                            img(src=sgboost_icon,height='20px',width='20px')),
                                       span(class="span_menu","Stochastic Gradient Boosting")
                                   )),
                       menuSubItem(tabName = "menu_som2",
                                   icon=NULL,
                                   div(class="side_menu",
                                       span(class="side_img",
                                            img(src=xyf_icon,height='20px',width='20px')),
                                       span(class="span_menu","Self-Organizing Maps")
                                   )),
                       menuSubItem(tabName = "menu_spd",
                                   icon=NULL,
                                   div(class="side_menu",
                                       span(class="side_img",
                                            icon("fas fa-binoculars",style="height:20px;width: 20px")),
                                       span(class="span_menu","Explore saved models")
                                   ))),
              menuItem(
                icon=NULL,
                tabName ="cc_tabname",
                expandedName="cc_expand",
                div(class="needed",id="ensemble_menu",
                    icon =NULL,
                    div(class="side_menu",
                        span(class="side_img",img(src=comp_icon,height='20',width='20')),
                        span(class="span_menu","Compare & Ensemble Models")
                    )
                ),
                menuSubItem(tabName = "menu_compare",
                            icon=NULL,
                            div(class="side_menu",
                                span(class="side_img",
                                     img(src=compare_icon,height='20px',width='20px')),
                                span(class="span_menu","Compare")
                            )),
                menuSubItem(tabName = "menu_ensemble",
                            icon=NULL,
                            div(class="side_menu",
                                span(class="side_img",
                                     img(src=ensemble_icon,height='20px',width='20px')),
                                span(class="span_menu","Ensemble")
                            ))
              ),
              #menuItem(div(img(src=sim_icon,height='20',width='20'),"Simulate"),tabName ="menu_sim"),
              # menuItem(div("teste",style = "margin-left: -8px; color: white; font-size: 14px; margin-top: -2px;margin-bottom: -2px;"),tabName = "menu_teste"),
              # uiOutput("testet"),



              actionLink("scrollDownButton", icon("fas fa-caret-down"))


          )
      )
    )


  ),


  dashboardBody(
    useShinyjs(),
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


    tags$style(".pretty.p-default input:checked~.state label:after {background-color: SeaGreen !important;}"),
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

  column(12,  style = "position: fixed; ",class="needed",id="header_app",' An Interactive Machine Learning App for Environmental Science',
  ),
  uiOutput('preprocess'),
  uiOutput('tools_upload'),


  add_busy_spinner(spin = "hollow-dots",height="20px", color="yellow",width="20px", margins = c(15, 600),position="top-left"),


  div(


    uiOutput("change_head_out"),
   # uiOutput("change_background"),
  ),
  div(id="main_panel",class="needed",
      div(
        #uiOutput('imesc_teste'),
        uiOutput("imesc_panel")
      )

  )
)

  ),
footer=dashboardFooter(left=div(style="display: flex",
                                div(style="width: 16%; display: inline-block; text-align: center",
                                    actionLink("imesc_help", span('iMESc help',icon("fas fa-question-circle")))
                                ),
                                div(style="width: 80%; display: inline-block; text-align: center",
                                    span(em("Version:"),paste0(version,';'),em("Last update:"),last_update)
                                ),
                                div(style="width: 4%; display: inline-block; text-align: right",
                                    uiOutput('footer_qsave')
                                )
)),
)



  )
}


