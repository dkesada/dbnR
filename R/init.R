.onLoad <- function(lib, pkg) {

}

.onAttach <- function(lib, pkg) {

}

.onUnload <- function (libpath) {
  library.dynam.unload("dbnR", libpath)
}
