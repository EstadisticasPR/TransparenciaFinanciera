## =============================== License ========================================
## ================================================================================
## This work is distributed under the CC0 1.0 Universal (CC0 1.0) license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)

## Generate popout icon, default gyphicon dependent on bootstrap
url_twitter_icon <-
  function() {
    HTML(
      paste0(
        "<script>var parent = document.getElementsByClassName('navbar-nav');parent[0].insertAdjacentHTML( 'afterend', ", 
        "'<ul class=\"nav navbar-nav navbar-right\">
        <li><a href=\"", "https://twitter.com/EstadisticasPR", "\" target=\"_blank\">","<span class=\"fab fa-twitter\">","</span>","<span>", "Twitter", "</span></a></li></ul>' );</script>"
      )
    )
  }

output$url_twitter_UI <- renderUI({
    url_twitter_icon()
})