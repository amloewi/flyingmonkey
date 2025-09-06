#' Write a Flying Monkey File
#'
#' Creates a partially pre-filled file with variable information for the user\cr
#' to edit
#'
#' The format of the created monkeyfile will be:
#'
#' OLD_NAME:\cr
#' (the old name of the variable, ex: "What is your name?")\cr
#' NEW_NAME:\cr
#' (created blank, filled in manually, ex: "name". If unused, defaults to old_name)\cr
#' TYPE:\cr
#' (the inferred class/type, ex: 'numeric', 'character', 'factor')\cr
#' OLD_VALUES: (only created if the inferred type is NOT string or numeric)\cr
#' value_1\cr
#' value_2\cr
#' value_n\cr
#' NEW_VALUES: (only created if the inferred type is NOT string or numeric)\cr
#' (left blank; if only reordering is required, you can re-order the OLD_VALUES.\cr
#' The element of highest value should go at the top, eg. Big; Medium; Small\cr
#' NEW_VALUES allows you to rename all of the elements in the variable -- just\cr
#' make sure their order matches that of the possibly re-ordered OLD_VALUES)\cr
#' - - - - - (five dashes denotes the end of a variable)\cr
#'
#' @param df the data frame that provides the variable information for the file
#' @param filename the name of the variable spec file being written
#'
#' @return None
#'
#' @examples
#' write_monkeyfile(df, "specs") # => specs.txt file to be edited manually
#'
#' @export
write_monkeyfile <- function(df, filename) {

    bits <- strsplit(filename, '.', fixed=T)[[1]]
    if (rev(bits)[1]=="txt") {
        filename.txt <- filename
    } else {
        filename.txt <- paste0(filename, '.txt')
    }

    for (column in names(df)) {
        cat("OLD_NAME:",   file=filename.txt, sep="\n", append=T)
        cat(column,        file=filename.txt, sep="\n", append=T)
        cat("NEW_NAME:",   file=filename.txt, sep="\n", append=T)
        cat("\n",          file=filename.txt, sep="",   append=T)
        cat("TYPE:",       file=filename.txt, sep="\n", append=T)

        df[, column] <- type.convert(df[, column], as.is=T)

        if (is.numeric(df[, column])) {
            cat("numeric",       file=filename.txt, sep="\n", append=T)
            cat("-----",         file=filename.txt, sep="\n", append=T)
            next
        }
        if (is.character(df[, column])) {
            nvals <- length(unique(df[, column][df[, column]!=""]))
            if (nvals == 1) {
                df[, column] <- df[, column]!=""
                cat("logical",     file=filename.txt, sep="\n", append=T)
                cat("-----",       file=filename.txt, sep="\n", append=T)
                next
            }
            if (nvals > 10) {
                cat("character",   file=filename.txt, sep="\n", append=T)
                cat("-----",       file=filename.txt, sep="\n", append=T)
                next
            }
            # Could just use else but I think this is clearer
            if (nvals > 1 && nvals < 10) {
                cat("ordered",     file=filename.txt, sep="\n", append=T)
                cat("OLD_VALUES:", file=filename.txt, sep="\n", append=T)
                for (x in unique(df[, column])) {
                    cat(x, file=filename.txt, sep="\n", append=T)
                }
                cat("NEW_VALUES:", file=filename.txt, sep="\n", append=T)
                cat("-----",       file=filename.txt, sep="\n", append=T)
            }
        }

        # if (length(unique(df[, column])) > 10) { # what about numeric?
        #     cat("character", file=filename.txt, sep="\n", append=T)
        # } else {
        #     cat("factor",      file=filename.txt, sep="\n", append=T)
        #     cat("OLD_VALUES:", file=filename.txt, sep="\n", append=T)
        #     for (x in unique(df[, column])) {
        #         cat(x, file=filename.txt, sep="\n", append=T)
        #     }
        #     cat("NEW_VALUES:", file=filename.txt, sep="\n", append=T)
        #     # cat("\n",          file=filename.txt, sep="",   append=T)
        # }
        # cat("-----",       file=filename.txt, sep="\n", append=T)
        # # What a LUDICROUSLY stupid language this can be.
        # # Needed DOUBLE QUOTES; and refused to use sep="\n\n"
    }
}

#' Read a Flying Monkey File
#'
#' Reads a completed file, and uses it to transform the original data
#'
#' For monkeyfile format, see ?write_monkeyfile
#'
#' @param df_original the data frame that is to be transformed
#' @param filepath the path to the variable spec file being read in
#'
#' @return A data.frame, transformed using the information in the monkeyfile
#'
#' @examples
#' read_monkeyfile(df, "Documents/specs.txt")
#'
#' @export
read_monkeyfile <- function(df_original, filepath) {
    # Things I'll need to accommodate:
    # 1) blanks (in both the old and new, AND whether they were there or not before)
    # 2) values that weren't used in the answer (is that easy? Yes.)
    df <- df_original
    connection = file(filepath, "r")
    lines <- readLines(connection) # => vector of characters (each a line)
    close(connection)

    slugs <- list()
    slug  <- list()
    while ( length(lines) > 0 ) {
        if (lines[1]=="OLD_NAME:") {
            slug[['old_name']] <- lines[2]
            lines <- lines[-(1:2)]
        }
        if (lines[1]=="NEW_NAME:") {
            slug[['new_name']] <- lines[2]
            lines <- lines[-(1:2)]
            # LOOKS LIKE IT WILL HANG IF THIS DOESN'T EXIST;
            # NEED GENERAL ERROR HANDLING FOR SPOTTING BAD FORMATTING
            print(slug[['new_name']])
        }
        if (lines[1]=="TYPE:") {
            slug[['type']] <- lines[2]
            lines <- lines[-(1:2)]
        }
        if (lines[1]=="OLD_VALUES:") {
            lines <- lines[-1]
            i <- 1
            old_values <- c()
            while (lines[i]!="NEW_VALUES:") {
                old_values <- c(old_values, lines[i])
                i <- i + 1
            }
            lines <- lines[-(1:(i-1))]
        }
        if (lines[1]=="NEW_VALUES:") {
            lines <- lines[-1]
            i <- 1
            new_values <- c()
            while (lines[i]!="-----") {
                new_values <- c(new_values, lines[i])
                i <- i + 1
            }
            # print(lines)
            if (length(old_values)==length(new_values)) {
                names(old_values) <- new_values
                lines <- lines[-(1:(i-1))] # will cut '-----' if no NEW_VALUES
            } else {
                names(old_values) <- old_values
                # cut no new lines, because the top is '-----', handled below
            }
            slug[['values']] <- old_values

            # slugs[slug[["new_name"]]] <- slug
            # slug <- list()

        }
        if (lines[1]=="-----") {
            # print(slug)
            slugs[[slug[["new_name"]]]] <- slug
            slug <- list()
            lines <- lines[-1]
        }
    }

    #     return (slugs)

    for (slug in slugs) {
        names(df)[names(df)==slug[['old_name']]] <- slug[['new_name']]

        # Probably possible with just "infer.type" or whatever, but; 4 lines.
        if ('numeric' %in% slug[['type']]) {
            df[, slug[['new_name']]] <- as.numeric(df[, slug[['new_name']]])
        }
        if ('character' %in% slug['type']) {
            df[, slug[['new_name']]] <- as.character(df[, slug[['new_name']]])
        }
        if ('logical' %in% slug['type']) {
            df[, slug[['new_name']]] <- df[, slug[['new_name']]]!=""
                #as.logical(df[, slug[['new_name']]])
        }

        ## HOW TO HANDLE BLANKS?? What happens if you just ... remove all
        # of them, from the file? If they're still in the data, they'll
        # appear as NA, and still get counted -- just make sure you're
        # prepared to PLOT NA's. otherwise, I think ok.
        if ('ordered' %in% slug[['type']] || 'factor' %in% slug[['type']]) {
            df[, slug[['new_name']]] <- factor(df[, slug[['new_name']]],
                                               levels=(slug[['values']]),
                                               labels=(names(slug[['values']])),
                                               ordered=T)
        }
    }

    return(df)
}

#' Plot all Variables in a data.frame
#'
#' Makes plots for all variables in a data.frame, each formatted as appropriate
#' Plots numeric variables as histograms, and factors as barplots of tables.
#' Expects long names and many values for factors, so plots them sideways,
#' instead of the default, which has names at the bottom of the plot.
#'
#'
#' @param df The data.frame to be plotted
#'
#' @return None
#'
#' @examples
#' plot_dataframe(df)
#'
#' @export
plot_dataframe <- function(df, makepdf=F, filename="plot_dataframe") {
    ############
    old.par <- par()

    if (makepdf) {
        pdf(filename, width=8.5, height=11)
        par(mfrow=c(4, 3))
    }
    for (i in 1:ncol(df)) {
        if (all(is.na(df[, i]))) { next } # else error, kills the loop
        if ('numeric' %in% class(df[, i])) {
            par(mar=c(3, 5, 2, 2)) # adjust to fit name length?
            hist(df[, i], main=names(df)[i], border="white")
        }
        # I had 'character' in here before;
        if (any(c('factor') %in% class(df[, i])) || 'logical' %in% class(df[, i])) {
            if (length(unique(df[, i])) < 10) {
                # par(mar=c(3, 12, 2, 2)) # adjust to fit name length?
                # barplot(table(df[, i]), main=names(df)[i], las=1, horiz=T,
                #         border="white")
                sideways_plot(df[, i], header = names(df)[i])
            }
        }
        if ('logical' %in% class(df[, i])) {
            barplot(table(as.factor(df[, i])), main=names(df)[i], las=1, horiz=T,
                    border="white")
        } else {
            next
        }
    }
    if (makepdf) {
        dev.off()
    }
    par(old.par)
    ############
}


#' Plot one variable in an easy to read barchart
#'
#' With a single variable, makes a simple, cleaned-up, sideways-oriented
#' barchart ('barplot') to allow the value names to be read more easily.
#' With a variable and a boolean, plots overlaid distributions of
#' the variables that do/don't have a TRUE value, to allow for easy comparisons.
#'
#'
#' @param x The data to be plotted
#' @param y An optional boolean vector
#'
#' @return None
#'
#' @examples
#' sideways_plot(x=health, y=received_treatment)
#'
#' @export
sideways_plot <- function(x, y=NULL, header="", color="gray") {
    old.pars <- par(no.readonly = TRUE)
    spacing <- round(max(nchar(as.character(x)))/2)
    par(mar=c(3, spacing+3, 2, 2)) # adjust to fit name length? else, 12
    if (is.null(y)) {
        barplot(table(x), main=header, las=1, horiz=T, border="white", col=color)
    } else {
        # These are NORMALIZED, for comparison
        barplot(table(x[ y])/sum( y, na.rm=T),
                main=header, las=1, horiz=T,
                border="white",
                col=rgb(0, 0, 1, alpha=.3)) # blue
        barplot(table(x[!y])/sum(!y, na.rm=T),
                main=header, las=1, horiz=T,
                border="white",
                col=rgb(1, 0, 0, alpha=.3), # red
                add=T)
    }
    par(old.pars) # .pardefault)
}
