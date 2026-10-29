.onAttach <- function(libname, pkgname) {
  pkg_version <- utils::packageVersion("tidyDCM")
  welcome_msg <- paste0("tidyDCM Package (version ", pkg_version, ")\n")

  # Check for development version (e.g., ends in .9001 or .9000)
  if (grepl("\\.9001$", as.character(pkg_version))) {
    welcome_msg <- paste0(
      welcome_msg,
      "Development version. Use with caution!"
    )
  }

  packageStartupMessage(welcome_msg)
}
