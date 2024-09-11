#' Rename the Target-Related Columns
#'
#' @param inputNames A character vector of input_names
#' Note: The input names vector can be drugs or genes that are searched within in ilINCS to pull out drug signturess
#' @return A character vector of new names
# Note: This function output a list of lists. If the string "treatment" is contained within the input names, the following will be contained in the list
#` TargetSignature, Target, TargetCellLine, TargetTime, Similarity, SignatureDirection, pValue
# if "treatment" is not contained in inputNames then the followiing will be contained
# TargetSignature, Target, TargetCellLine, TargetTime, TargetConcentration, Similarity, SignatureDirection, and pValue.
targetRename <- function(inputNames) {
    if ("treatment" %in% inputNames) {
        c(
            "TargetSignature", "Target", "TargetCellLine",
            "TargetTime", "Similarity", "SignatureDirection", "pValue"
        )
    } else {
        c(
            "TargetSignature", "Target", "TargetCellLine",
            "TargetTime", "TargetConcentration", "Similarity",
            "SignatureDirection", "pValue"
        )
    }
}

#' Check if the library is valid
#'
#' @param lib a string of libraries
#' Note: This lib of strings comes from the validateLibraries function. 
#' @return a boolean
# The output of this string for each indiviudal input is a 0 for false and 1 for true. This is only to ensure the library itself exists.
.validateLibrary <- function(lib) {
    lib %in% c("CP", "KD", "OE")
}

#' Check if the libraries input are valid
#'
#' @param libs a character vector of libraries
#' #the character vectors must be "OE", "KD", or "CP". The purpose of this function is to ensure the that the input is one of these.
#' @return a boolean
# this return a 0 or a 1 to ensure if the result is true or false
# this function will go through each indiviudal element in libs using purrr:map_lgl and will run the individual .validateLibrary function to ensure all entires are
# of the stirng "CP", "KD", or "OE".
validateLibraries <- function(libs) {
    all(purrr::map_lgl(libs, .validateLibrary))
}

#' Stop if the libraries are invalid
#'
#' @param libs a character vector of libraries
#' this function will itterate through all the strings in libs and look to see if 'OE', 'KD', 'CP' is contained in all entries
#' @return a stop if the libraries are invalid
# if not all the entires is not 'OE', 'KD', and 'CP' - then the function 
#this function will run is if validatelbiariers stop and will indicate the one of the strings is not 'OE', 'KD', and 'CP'.
stopIfInvalidLibraries <- function(libs) {
    if (!validateLibraries(libs)) {
        stop("Both input and output libraries must be one of 'OE', 'KD', 'CP'")
    }
}

#' Load the correct metadata table
#'
#' @param lib a string. One of "OE", "KD" or "CP"
#'this takes either 'OE', 'KD', or 'CP' string and will create metadata table.
#' @return a tibble
# the tibble will be the meta-data table associated with the 'OE', 'KD' or 'CP'. If the string is not one of these - the string will return 'Invalid library'.
loadMetadata <- function(lib) {
    if (lib == "OE") {
        oeMetadata
    } else if (lib == "KD") {
        kdMetadata
    } else if (lib == "CP") {
        cpMetadata
    } else {
        stop("Invalid library")
    }
}
