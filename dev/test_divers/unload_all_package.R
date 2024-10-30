sessionInfo=sessionInfo()
pkg=c(names(sessionInfo$otherPkgs),names(sessionInfo$loadedOnly))
exception_pkg=c("Rcpp","compiler","rstudioapi","tools","fastmap")
pkg=setdiff(pkg,exception_pkg)

while(length(pkg)>0){
  lapply(pkg,function(pkg_name){
    print(pkg_name)
    
    try(unloadNamespace(pkg_name),silent=F)
    # library(shiny)
  })
  sessionInfo=sessionInfo()
  pkg=c(names(sessionInfo$otherPkgs),names(sessionInfo$loadedOnly))
  pkg=setdiff(pkg,exception_pkg)
}


