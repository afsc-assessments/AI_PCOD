# Shortened functions
.an <- function(x) {
  return(as.numeric(x))
}
.ac <- function(x) {
  return(as.character(x))
}
.af <- function(x) {
  return(as.factor(x))
}

#  =========================================================================== #

#' @title Get file names without extension
#'
#' @description This function returns a file name without its extension
#'
#' @param filenames Name of file
#'
#' @author Matthieu Veron
#  Contact: mveron@uw.edu
#'
#' @return character string: the file name
#'
get_nam <- function(filenames) {
  tmp <- c()
  for (i in 1:length(filenames))
    tmp <- c(tmp, unlist(strsplit(filenames, "\\.")[[i]][1]))
  return(tmp)
}

#  =========================================================================== #

#' Clean files in a folder
#'
#' @param path (character string) file path representing the root folder to be cleaned
#' @param names (vector of character strings) - Indicate the files in the root 
#' folder to be removed
#' @param verbose (logical) -  flag to print filenames being deleted
#
#' @author Matthieu Veron
#  Contact: mveron@uw.edu
#'
clean_files <- function(path = ".",
                        names = c("no_file"),
                        verbose = FALSE) {
  for (name in names) {
    fns = list.files(path = path,
                     pattern = glob2rx(name),
                     full.names = TRUE)
    
    if (length(fns) > 0) {
      if (verbose)
        cat("--Removing files:\n", paste0("\t", fns, "\n"))
      
      file.remove(fns)
      
    }
  }
}

#  =========================================================================== #

#' Clean files SS output files
#' @param path (character string) - file path to the folder that has to be cleaned
#' @param verbose (logical) -  flag to print filenames being deleted
#
#' @author Matthieu Veron
#  Contact: mveron@uw.edu
#'
clean_bat <- function(path = ".", verbose = TRUE) {
  # Remove the basic files that we don't want
  names = c("*.bar","*.eva","*.log","*.std","gradient.*",
            "*.r0*","*.p0*","*.b0*", "starter.ss_new", "forecast.ss_new","control.ss_new")
  clean_files(path=path,names=names,verbose=verbose)
  # Files we want to save
  filesSave <- c(
    "starter",
    "forecast",
    "Report",
    "Forecast-report",
    "echoinput",
    "ss_summary",
    "CompReport",
    "ss",
    "ss",
    "ss_win~4",
    "ss_win~4",
    "warning",
    "console.output",
    "data_echo.ss_new",
    "data.ss_new"
  )
  filesSave <- list.files(path)[get_nam(list.files(path)) %in% get_nam(filesSave)]
  # Get the names of the data and control files
  starternam <- filesSave[grepl(pattern = "starter.", x = filesSave)]
  starter <-
    r4ss::SS_readstarter(
      file = file.path(path, starternam,
                       fsep = .Platform$file.sep),
      verbose = verbose
    )
  TrueNam <- c(starter$datfile, starter$ctlfile)
  inputFiles <- list.files(path)[get_nam(list.files(path)) %in% get_nam(TrueNam)]
  for(i in 1:2){
    inputFiles[i] <- paste0(get_nam(inputFiles[i]), ".", unlist(strsplit(TrueNam[i], "\\."))[2])
  }
  filesSave <- c(filesSave, inputFiles)
  names <- list.files(path)[!get_nam(list.files(path)) %in% get_nam(filesSave)]
  # Remove files we don't want
  clean_files(path = path,
              names = names,
              verbose = verbose)
}

#  =========================================================================== #

#' Define the current OS
#'
#' @author Joshua A Zahner & Matthieu Veron
# Contacts: jzahner@uw.edu | mveron@uw.edu
#
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf['sysname']
    if (os == 'Darwin') {
      os <- "osx"
    } else if (os == "Windows") {
      if (grepl("64", sysinf["release"])) {
        os <- "win64"
      } else {
        os <- "win32"
      }
    }
  } else {
    ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}

#  =========================================================================== #

#' @title Set the path where the SS exe is stored
#' 
#' @param SS_version (character string) - The SS version currently supported are
#' either "3.30.21" or "3.24.U".
#' @param Exe_extra (character string) - Define the "extra" name of the executable.
#' The possible name extensions are: "fast", "safe", "opt" for the "3.24.U" SS version
#' and "opt" for the "3.30.21" version.
#' 
#' @author Matthieu Veron
# Contact: mveron@uw.edu
#
SSexe_path <- function(SS_version = "3.30.21",
                       Exe_extra = "") {
  .fsep <- .Platform$file.sep #easy for file.path() function
  
  # Get the OS
  os <- get_os()
  #Check the SS version
  if (!(SS_version %in% c("3.24.U", "3.30.21"))) {
    stop("Currently unsupported SS version. Please used either 3.24.U or 3.30.21.")
  }
  
  if (!Exe_extra %in% c("fast", "safe", "opt", "") &&
      SS_version == "3.24.U") {
    cat(
      Exe_extra,
      " is not available as extra name for the 3.24.U version of SS.
Please check the available executable for this version."
    )
    stop()
  } else if (!Exe_extra %in% c("opt", "")) {
    cat(
      Exe_extra,
      " is not available as extra name for the 3.30.21 version of SS.
Please check the available executable for this version."
    )
    stop()
  }
  
  version.components <- stringr::str_split(SS_version, "[.]")[[1]]
  major.version <- version.components[1]
  minor.version <- version.components[2]
  patch.number  <- version.components[3]
  
  # File name extension for the executable (depends on the OS)
  file.extension <- ""
  if (os %in% c("win32", "win64", "win")) {
    file.extension <- ".exe"
    # Assign name of the exe for windows OS
    if (.an(minor.version) >= 30) {
      os <- "win"
    }
  }
  
  # Extra name for the exe
  if (nchar(Exe_extra) > 0) {
    Exe_extra <- paste0("_", Exe_extra)
  }
  
  # Determine the exe path
  exe.dir <- file.path(here::here(), "Executables", fsep = .fsep)
  version.dir <-
    paste0("SS_V", major.version, "_", minor.version, "_", patch.number)
  
  if (minor.version >= 30) {
    ss.file <- paste0("ss", Exe_extra, "_", os,  file.extension)
  } else {
    ss.file <- paste0("ss_", os, Exe_extra, file.extension)
  }
  
  exe.path <- file.path(exe.dir, version.dir, ss.file, fsep = .fsep)
  if (!file.exists(exe.path)) {
    warning("Note that this executable does not exist.")
  }
  return(exe.path)
}

#  =========================================================================== #

#' Run SS
#'
#' @param SS_version (character string) - The SS version currently supported are
#' either "3.30.21" or "3.24.U".
#' @param Exe_extra (character string) - Define the "extra" name of the executable.
#' The possible name extensions are: "fast", "safe", "opt" for the "3.24.U" SS version
#' and "opt" for the "3.30.21" version.
#' @param base_path (character string) - root directory where the input file are
#' housed.
#' @param pathRun (character string) - path where the run has to be done. If
#' \code{NULL} (default), then the run will be done in the `run` folder housed
#' in the `base_path`. This `run` folder is created if it doesn't already exist.
#' @param copy_files (logical) - Do the iinput files have to be copied from the
#' `base_path` to the `pathRun` ?
#' @param extras (character string) - Additional ADMB command line arguments
#' passed to the executable, such as "-nohess".
#' @param cleanRun (logical) - Specify if the `pathRun` has to be cleaned after
#' the run. See the `clean_bat()` function.
#'
#' @author Matthieu Veron
#  Contact: mveron@uw.edu
#' 
#' @Details
#' By default, the function will save the output of the console in the
#' `console.output.txt` file and won't skip the run if the `pathRun` folder
#' already contain a `Report.sso` file.

run_SS <- function(SS_version = "3.30.21",
                   Exe_extra = "",
                   base_path = "",
                   pathRun = NULL,
                   copy_files = TRUE,
                   extras = NULL,
                   cleanRun = TRUE) {
  .fsep <- .Platform$file.sep #easy for file.path() function
  
  # Directory where the executable is stored
  pathExe <-
    SSexe_path(SS_version = SS_version, Exe_extra = Exe_extra)
  
  # Check the existence of pathRun and copy SS input files if needed
  if (!is.null(pathRun)) {
    if (!dir.exists(pathRun)) {
      stop("The 'pathRun' specified does not exist. Please check the directory you entered.")
    }
    # Copy SS input files
    if(copy_files){
      SS.files <- dir(base_path, pattern = "*.ss", ignore.case = TRUE, all.files = TRUE)
      file.copy(
        from = file.path(base_path, SS.files, fsep = .fsep),
        to = file.path(pathRun, SS.files, fsep = .fsep),
        overwrite = TRUE, copy.date = TRUE
      )
    }
  } else {
    pathRun <- file.path(base_path, "run", fsep = .fsep)
    if (!dir.exists(pathRun)) {
      dir.create(pathRun)
    }
    # Copy SS input files
    SS.files <- dir(base_path, "*.ss", ignore.case = TRUE, all.files = TRUE)
    file.copy(
      from = file.path(base_path, SS.files, fsep = .fsep),
      to = file.path(pathRun, SS.files, fsep = .fsep),
      overwrite = TRUE, copy.date = TRUE
    )
  }

  # run SS
  print(
    r4ss::run(
      dir = pathRun,
      extras = extras,
      exe = pathExe,
      verbose = TRUE,
      skipfinished = FALSE
    )
  )
  
  # run clean functions to delete files
  if (cleanRun)
    clean_bat(path = pathRun, verbose = FALSE)
}

#  =========================================================================== #

#' @title Run SS if data.ss_new/data_echo.ss_new is missing when reading .ctl file
#'
#' @description Function that runs SS to get the file needed to read in the control
#' file. This depends on the version of SS considered (see below - data.ss_new /
#' data_echo.ss_new)
#'
#'@param SS_version (character string) - The SS version currently supported are
#' either "3.30.21" or "3.24.U".
#' @param Exe_extra (character string) - Define the "extra" name of the executable.
#' The possible name extensions are: "fast", "safe", "opt" for the "3.24.U" SS version
#' and "opt" for the "3.30.21" version.
#' @param base_path (character string) - root directory where the input file are
#' housed.
#' @param pathRun (character string) - path where the run has to be done. If
#' \code{NULL} (default), then the run will be done in the `run` folder housed
#' in the `base_path`. This `run` folder is created if it doesn't already exist.
#' @param copy_files (logical) - Do the iinput files have to be copied from the
#' `base_path` to the `pathRun` ?
#' @param extras (character string) - Additional ADMB command line arguments
#' passed to the executable, such as "-nohess".
#' @param cleanRun (logical) - Specify if the `pathRun` has to be cleaned after
#' the run. See the `clean_bat()` function.
#'
#' @author Matthieu Veron
#  Contact: mveron@uw.edu
#' @Details
#' By default, the function will save the output of the console in the
#' `console.output.txt` file and won't skip the run if the `pathRun` folder
#' already contain a `Report.sso` file.
#'
RunSS_CtlFile <- function(SS_version = "3.30.21",
                          Exe_extra = "",
                          base_path = "",
                          pathRun = NULL,
                          copy_files = TRUE,
                          extras = NULL,
                          cleanRun = TRUE) {
  
  pathRuntmp <- file.path(base_path, 'run', fsep = .Platform$file.sep)
  fileneeded <- ifelse(test = SS_version == "3.24.U",
                       yes = "data.ss_new",
                       no = "data_echo.ss_new")
  if (!file.exists(file.path(pathRuntmp, fileneeded, fsep = .Platform$file.sep))) {
    run_SS(
      SS_version = SS_version,
      # version of SS
      Exe_extra = Exe_extra,
      # "extra" name of the executable
      base_path = base_path,
      # root directory where the input file are housed
      pathRun = pathRun,
      # A 'run' folder is created if needed (where output files will be stored)
      copy_files = copy_files,
      # copy the input files from the 23.sq.fix folder
      extras - extras,
      # Additional ADMB command line arguments
      cleanRun = cleanRun
      # clean the folder after the run
    )
  } else {
    cat("The ",
        fileneeded,
        " file is already available in the run directory (",
        pathRuntmp,
        ")", sep="")
  }
}
