library(shinydashboard)
library(shinyjs)

timeoutSeconds <- 30*60

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions
function logout() {
Shiny.setInputValue('timeOut', '%ss')
}
function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)


#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboardPlus
#' @import shinydashboard
#' @noRd
shinyUI(
    tagList(

        #launchGA(),
        tags$script(inactivity),
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(text = "shinyjs.resetProstar = function() {history.go(0)}",
            functions = c("resetProstar")),
        
        #theme = "css/ceruleanProstar.css",
        #theme = shinythemes::shinytheme("cerulean"),
        
        titlePanel("", windowTitle = "Prostar"),
        div(id = "loading_page", mod_loading_page_ui('loadPage') ),
        shinyjs::hidden(
            div(id='main_page', mod_main_page_ui('mainPage'))
    )
    )
)

