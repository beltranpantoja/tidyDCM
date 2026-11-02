.onAttach <- function(libname, pkgname) {
  pkg_version <- utils::packageVersion(pkgname)
  welcome_msg <- paste0("tidyDCM Package (version ", pkg_version, ")")

  if (grepl("\\.9[0-9]{3}$", as.character(pkg_version))) {
    built <- utils::packageDescription(pkgname)$Built
    built_date <- sub(".*; (\\d{4}-\\d{2}-\\d{2}).*", "\\1", built)
    welcome_msg <- paste0(
      welcome_msg, "\n",
      "Development version (installed ", built_date, "). Use with caution!"
    )
  }

  packageStartupMessage(welcome_msg)
}
