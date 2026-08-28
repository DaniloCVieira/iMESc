# =============================================================================
# iMESc package installer
# =============================================================================


# -----------------------------------------------------------------------------
# Detach a package if it is currently loaded
# -----------------------------------------------------------------------------

detach_package <- function(pkg) {

  try(
    {

      search_item <- paste0("package:", pkg)

      # Package attached with library() or require()
      if (search_item %in% search()) {

        detach(
          search_item,
          unload = TRUE,
          character.only = TRUE
        )

        # Namespace loaded as a dependency
      } else if (pkg %in% loadedNamespaces()) {

        unloadNamespace(pkg)

      }

    },
    silent = TRUE
  )

  invisible(NULL)
}


# -----------------------------------------------------------------------------
# Check Debian/Ubuntu system packages needed by source installs
# -----------------------------------------------------------------------------

check_debian_system_packages <- function() {

  if (!identical(Sys.info()[["sysname"]], "Linux")) {
    return(invisible(TRUE))
  }

  dpkg_query <- Sys.which("dpkg-query")

  if (identical(unname(dpkg_query), "")) {
    return(invisible(TRUE))
  }

  system_packages <- c(
    "build-essential",
    "gfortran",
    "cmake",
    "pkg-config",
    "libfontconfig1-dev",
    "libfreetype-dev",
    "libssl-dev",
    "libabsl-dev",
    "libnlopt-dev",
    "libgdal-dev",
    "gdal-bin",
    "libgeos-dev",
    "libproj-dev",
    "libsqlite3-dev",
    "libudunits2-dev",
    "libcurl4-openssl-dev",
    "libglpk-dev",
    "libmagick++-dev",
    "gsfonts",
    "libpng-dev",
    "zlib1g-dev",
    "pandoc"
  )

  installed <- vapply(
    system_packages,
    function(pkg) {

      result <- suppressWarnings(
        system2(
          dpkg_query,
          args = c("-s", pkg),
          stdout = TRUE,
          stderr = FALSE
        )
      )

      any(
        grepl(
          "^Status: install ok installed$",
          result
        )
      )
    },
    logical(1)
  )

  missing <- system_packages[!installed]

  if (length(missing) > 0L) {

    stop(
      paste0(
        "Some Debian/Ubuntu system packages required by iMESc are missing:\n",
        paste0("  - ", missing, collapse = "\n"),
        "\n\nRun this command in the server terminal, then start iMESc again:\n",
        "sudo apt update && sudo apt install -y \\\n  ",
        paste(missing, collapse = " \\\n  ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# Install required packages for iMESc
#
# Arguments:
#   lib  - Library where packages will be installed
#   repo - CRAN repository
# -----------------------------------------------------------------------------

install_imesc <- function(
    lib = .libPaths()[1],
    repo = "https://cloud.r-project.org"
) {

  # ---------------------------------------------------------------------------
  # Required packages and minimum versions
  # ---------------------------------------------------------------------------

  packages <- c(
    shiny = "1.9.1",
    plotly = "4.10.4",
    aweSOM = "1.3",
    ggraph = "2.2.1",
    ggpubr = "0.6.0",
    ggforce = "0.4.2",
    mice = "3.16.0",
    caret = "6.0.94",
    leaflet = "2.2.2",
    GGally = "2.2.1",
    ggplot2 = "3.5.1",
    sf = "1.0.19",
    dplyr = "1.1.4",
    igraph = "2.1.1",
    readr = "2.1.5",
    gstat = "2.1.2",
    factoextra = "1.0.7",
    partykit = "1.2.22",
    ade4 = "1.7.22",
    scales = "1.3.0",
    party = "1.3.17",
    randomForestExplainer = "0.10.1",
    sortable = "0.5.0",
    ggparty = "1.0.0",
    automap = "1.1.12",
    scatterpie = "0.2.4",
    tibble = "3.2.1",
    colourpicker = "1.3.0",
    DT = "0.33",
    pdp = "0.8.2",
    shinyWidgets = "0.8.7",
    e1071 = "1.7.16",
    sp = "2.1.4",
    klaR = "1.7.3",
    shinydashboardPlus = "2.0.5",
    shinyTree = "0.3.1",
    htmlwidgets = "1.6.4",
    NCmisc = "1.2.0",
    ggrepel = "0.9.6",
    dendextend = "1.19.0",
    shinycssloaders = "1.1.0",
    readxl = "1.4.3",
    vegan = "2.6.8",
    plot3D = "1.4.1",
    httr = "1.4.7",
    gridExtra = "2.3",
    gplots = "3.2.0",
    NeuralNetTools = "1.5.3",
    shinybusy = "0.3.3",
    shinydashboard = "0.7.2",
    raster = "3.6.30",
    kernlab = "0.9.33",
    ggridges = "0.5.6",
    shinyjs = "2.1.0",
    reshape2 = "1.4.4",
    webshot = "0.5.5",
    viridis = "0.6.5",
    shinyBS = "0.61.1",
    leaflet.minicharts = "0.6.2",
    beepr = "2.0",
    data.table = "1.16.2",
    segRDA = "1.0.2",
    indicspecies = "1.7.15",
    permute = "0.9.7",
    kohonen = "3.0.12",
    ggnewscale = "0.5.0",
    randomForest = "4.7.1.2",
    reshape = "0.8.9",
    jsonlite = "1.8.9",
    gbRd = "0.4.12",
    plyr = "1.8.9",
    base64enc = "0.1.3",
    RColorBrewer = "1.1.3",
    wesanderson = "0.3.7",
    corrplot = "0.95",
    writexl = "1.5.1",
    geodist = "0.1.0",
    Metrics = "0.1.4",
    progress = "1.2.3",
    rstudioapi = "0.17.1",
    colorRamps = "2.3.4",
    waiter = "0.2.5",
    gbm = "2.2.2",
    deepnet = "0.2.1",
    earth = "5.3.4",
    nnet = "7.3.19",
    rpart = "4.1.23",
    monmlp = "1.1.5",
    RSNNS = "0.4.17",
    evtree = "1.0.8",
    RANN = "2.6.2",
    colorspace = "2.1-3"
  )

  # ---------------------------------------------------------------------------
  # Validate library
  # ---------------------------------------------------------------------------

  if (length(lib) != 1L) {
    stop("'lib' must indicate a single library path.")
  }

  if (!dir.exists(lib)) {

    dir.create(
      lib,
      recursive = TRUE,
      showWarnings = FALSE
    )

  }

  if (file.access(lib, mode = 2) != 0) {

    stop(
      "The selected library is not writable:\n",
      lib,
      "\n\nSelect a user library or change its permissions."
    )

  }

  options(repos = c(CRAN = repo))
  dependency_types <- c("Depends", "Imports", "LinkingTo")

  message("Operating system: ", Sys.info()[["sysname"]])
  message("R version: ", getRversion())
  message("Installation library: ", normalizePath(lib))
  message("Checking iMESc packages...")
  check_debian_system_packages()


  # ---------------------------------------------------------------------------
  # Install pak separately
  # ---------------------------------------------------------------------------

  if (!requireNamespace("pak", quietly = TRUE)) {

    message("Installing pak...")

    install.packages(
      "pak",
      lib = lib,
      repos = repo,
      verbose = FALSE,
      quiet = TRUE
    )

  }

  if (!requireNamespace("pak", quietly = TRUE)) {

    stop(
      "The 'pak' package could not be installed or loaded."
    )

  }


  # ---------------------------------------------------------------------------
  # Check installed packages and versions
  # ---------------------------------------------------------------------------

  installed_info <- installed.packages(
    lib.loc = .libPaths()
  )

  installed_names <- rownames(installed_info)

  package_table <- data.frame(
    package = names(packages),
    required_version = unname(packages),
    installed = FALSE,
    installed_version = NA_character_,
    version_ok = FALSE,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(package_table))) {

    pkg <- package_table$package[i]

    if (pkg %in% installed_names) {

      current_version <- installed_info[pkg, "Version"]

      package_table$installed[i] <- TRUE
      package_table$installed_version[i] <- current_version

      package_table$version_ok[i] <-
        package_version(current_version) >=
        package_version(package_table$required_version[i])

    }

  }


  # ---------------------------------------------------------------------------
  # Identify missing or outdated packages
  # ---------------------------------------------------------------------------

  to_install <- package_table$package[
    !package_table$installed |
      !package_table$version_ok
  ]


  # ---------------------------------------------------------------------------
  # Finish if everything is already installed
  # ---------------------------------------------------------------------------

  if (length(to_install) == 0L) {

    message("All required iMESc packages are already installed.")

    return(
      invisible(package_table)
    )

  }


  message(
    length(to_install),
    " packages are missing or outdated:"
  )

  message(
    paste0(
      "  - ",
      paste(to_install, collapse = "\n  - ")
    )
  )


  # ---------------------------------------------------------------------------
  # Attempt to unload packages that will be updated
  # ---------------------------------------------------------------------------

  invisible(
    lapply(
      to_install,
      detach_package
    )
  )


  # ---------------------------------------------------------------------------
  # Windows progress bar
  # ---------------------------------------------------------------------------

  use_windows_progress <-
    identical(.Platform$OS.type, "windows") &&
    exists("pb", inherits = TRUE)

  if (use_windows_progress) {

    try(
      setWinProgressBar(
        get("pb", inherits = TRUE),
        0.9,
        label = paste0(
          "Installing ",
          length(to_install),
          " missing or outdated packages..."
        )
      ),
      silent = TRUE
    )

  }


  # ---------------------------------------------------------------------------
  # Install missing or outdated packages
  # ---------------------------------------------------------------------------

  installation_error <- NULL

  tryCatch(
    {

      pak::pkg_install(
        to_install,
        ask = FALSE,
        lib = lib,
        upgrade = FALSE,
        dependencies = dependency_types
      )

    },
    error = function(e) {

      installation_error <<- conditionMessage(e)

    }
  )


  # ---------------------------------------------------------------------------
  # Close Windows progress bar
  # ---------------------------------------------------------------------------

  if (use_windows_progress) {

    try(
      close(
        get("pb", inherits = TRUE)
      ),
      silent = TRUE
    )

  }


  if (!is.null(installation_error)) {

    warning(
      paste0(
        "An error occurred during package installation:\n",
        installation_error,
        "\n\nTrying base R installation as a fallback."
      ),
      call. = FALSE
    )

    tryCatch(
      {

        install.packages(
          to_install,
          lib = lib,
          repos = repo,
          dependencies = dependency_types,
          quiet = TRUE
        )

      },
      error = function(e) {

        warning(
          paste0(
            "Fallback installation also failed:\n",
            conditionMessage(e)
          ),
          call. = FALSE
        )

      }
    )

  }


  # ---------------------------------------------------------------------------
  # Final verification
  # ---------------------------------------------------------------------------

  final_installed_info <- installed.packages(
    lib.loc = .libPaths()
  )

  final_installed_names <- rownames(
    final_installed_info
  )

  final_table <- package_table

  for (i in seq_len(nrow(final_table))) {

    pkg <- final_table$package[i]

    if (pkg %in% final_installed_names) {

      current_version <- final_installed_info[pkg, "Version"]

      final_table$installed[i] <- TRUE
      final_table$installed_version[i] <- current_version

      final_table$version_ok[i] <-
        package_version(current_version) >=
        package_version(final_table$required_version[i])

    } else {

      final_table$installed[i] <- FALSE
      final_table$installed_version[i] <- NA_character_
      final_table$version_ok[i] <- FALSE

    }

  }


  # ---------------------------------------------------------------------------
  # Report installation result
  # ---------------------------------------------------------------------------

  failed <- final_table$package[
    !final_table$installed |
      !final_table$version_ok
  ]

  if (length(failed) > 0L) {

    warning(
      paste0(
        "The following packages were not installed correctly ",
        "or do not meet the minimum required version:\n",
        paste(failed, collapse = ", ")
      ),
      call. = FALSE
    )

  } else {

    message(
      "All iMESc packages were installed successfully."
    )

  }

  invisible(final_table)

}
