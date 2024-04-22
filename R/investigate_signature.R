#' Investigate a given DGE dataset
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function takes a DGE Data frame and then
#' finds concordant signatures to that.
#' This generates an L1000 signature from the DGE
#' dataset and then uploads that signature to
#' iLINCS to find the relevant concordant (or discordant) signatures
#'
#' @param expr A dataframe that has differential gene expression analysis
#' @param output_lib The library to search
#' @param filter_threshold The Filtering threshold.
#' @param filter_prop The Filtering proportion.
#' @param similarity_threshold The Similarity Threshold
#' @param paired Logical. Whether to query iLINCS separately
#' for up and down regulated genes
#' @param output_cell_lines A character vector of cell lines
#' to restrict the output search to.
#' @param gene_column The name of the column that has gene symbols
#' @param logfc_column The name of the column that has log_2 fold-change values
#' @param pval_column  The name of the column that has p-values
#' @param source_name (Optional) An annotation column to identify
#' the signature by name
#' @param source_cell_line (Optional) An annotation column to specify
#' the cell line for the input data
#' @param source_time (Optional) An annotation column to specify the
#' time for the input data
#' @param source_concentration (Optional) An annotation column to specify
#' the concentration for the input data
#'
#' @return A tibble with the the similarity scores and signature metadata
#' @export
#'
#' @importFrom dplyr mutate select any_of
#' @importFrom rlang .data
#'
#' @examples
#' TRUE
investigateSignature <- function(expr,
                                 outputLib,
                                 filterThreshold = NULL,
                                 filterProp = NULL,
                                 similarityThreshold = 0.2,
                                 paired = TRUE,
                                 outputCellLines = NULL,
                                 geneColumn = "Symbol",
                                 logfcColumn = "logFC",
                                 pvalColumn = "PValue",
                                 sourceName = "Input",
                                 sourceCellLine = "NA",
                                 sourceTime = "NA",
                                 sourceConcentration = "NA") {
    libs <- c("OE", "KD", "CP")

    if (!outputLib %in% libs) {
        stop("Output library must be one of 'OE', 'KD', 'CP'")
    }

    if (missing(outputLib)) {
        stop("Please specify an output library")
    }

    exprSignature <- expr %>%
        prepareSignature(
            gene_column = geneColumn,
            logfc_column = logfcColumn,
            pval_column = pvalColumn
        )

    signatureId <- unique(exprSignature[["signatureID"]])

    if (paired) {
        filteredUp <- exprSignature %>%
            filterSignature(
                direction = "up",
                threshold = filterThreshold,
                prop = filterProp
            )

        filteredDown <- exprSignature %>%
            filterSignature(
                direction = "down",
                threshold = filterThreshold,
                prop = filterProp
            )

        concordantUp <- filteredUp %>%
            getConcordants(ilincs_library = outputLib, sig_direction = "Up")

        concordantDown <- filteredDown %>%
            getConcordants(ilincs_library = outputLib, sig_direction = "Down")


        consensusTargets <-
            consensusConcordants(
                concordantUp,
                concordantDown,
                paired = paired,
                cell_line = outputCellLines,
                cutoff = similarityThreshold
            )
    } else {
        filtered <- exprSignature %>%
            filterSignature(
                direction = "any",
                threshold = filterThreshold,
                prop = filterProp
            )

        concordants <- filtered %>%
            getConcordants(ilincs_library = outputLib)

        consensusTargets <-
            consensusConcordants(
                concordants,
                paired = paired,
                cell_line = outputCellLines,
                cutoff = similarityThreshold
            )
    }

    augmented <- consensusTargets %>%
        dplyr::mutate(
            SourceSignature = signatureId,
            Source = sourceName,
            SourceCellLine = sourceCellLine,
            SourceTime = sourceTime,
            InputSignatureDirection = sigDirection
        ) %>%
        dplyr::select(
            dplyr::any_of(c(
                "Source",
                "Target",
                "Similarity",
                "SourceSignature",
                "SourceCellLine",
                "InputSignatureDirection",
                "SourceConcentration",
                "SourceTime",
                "TargetSignature",
                "TargetCellLine",
                "TargetConcentration",
                "TargetTime"
            ))
        )

    augmented
}
