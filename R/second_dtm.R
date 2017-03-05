rE_dtm <- function(x, totdm = FALSE, re_control = list(wordLengths = c(1, 50))) {
    x <- NLP::content(x)
    nr <- length(x)
    lap <- lapply(x, EAch_tAblE)
    rm(x)
    lap_name <- lapply(lap, names)
    lapply_unique <- sort(unique(unlist(lap_name)))
    lap_match <- lapply(lap_name, match, table = lapply_unique)
    rm(lap_name)
    m <- matrix(0, nrow = nr, ncol = length(lapply_unique))
    for (i in 1:nr) {
        m[i, lap_match[[i]]] <- lap[[i]]
    }
    colnames(m) <- lapply_unique
    if ("" %in% lapply_unique) 
        m <- m[, -1]
    rm(lap, lapply_unique, lap_match)
    ## below is for control
    controllistname <- names(re_control)
    mcolname <- colnames(m)
    # for dic
    if ("dictionary" %in% controllistname & ncol(m) > 0) {
        dic <- intersect(mcolname, re_control$dictionary)
        if (length(dic) > 0) {
            m <- m[, dic, drop = FALSE]
            mcolname <- dic
            rm(dic)
        } else {
            m <- matrix(nrow = nrow(m), ncol = 0)
            message("Note: None of the words specified in dictionary exists.")
        }
    }
    # for word length
    if ("wordLengths" %in% controllistname & ncol(m) > 0) {
        term_nchar <- nchar(mcolname)
        if ("dictionary" %in% controllistname) {
            dic_nchar <- nchar(re_control$dictionary)
            WL <- c(1, max(dic_nchar))
            rm(dic_nchar)
        } else {
            WL <- re_control$wordLengths
        }
        qualify_length <- term_nchar >= WL[1] & term_nchar <= WL[2]
        rm(term_nchar, WL)
    } else {
        qualify_length <- rep(TRUE, length(mcolname))
    }
    # for bounds
    if ("bounds" %in% controllistname & ncol(m) > 0) {
        term_bound <- colSums(m)
        qualify_bound <- term_bound >= re_control$bounds[1] & term_bound <= re_control$bounds[2]
        rm(term_bound)
    } else {
        qualify_bound <- rep(TRUE, length(mcolname))
    }
	# for have
    if ("have" %in% controllistname & ncol(m) > 0) {
        term_have <- countHave(m)
        qualify_have <- term_have >= re_control$have[1] & term_have <= re_control$have[2]
        rm(term_have)
    } else {
        qualify_have <- rep(TRUE, length(mcolname))
    }	
	# lengths, bounds, have
    len_bou_have <- qualify_length & qualify_bound & qualify_have
    if (any(len_bou_have == TRUE) & ncol(m) > 0) {
        m <- m[, len_bou_have, drop = FALSE]
    } else {
        m <- matrix(nrow = nrow(m), ncol = 0)
    }
    rm(len_bou_have, qualify_length, qualify_bound, qualify_have)
	# do sth with 0 col
	if (ncol(m) == 0){
	  m <- matrix(0, ncol = 1, nrow = nrow(m))
	  colnames(m) <- "NA"
	}
	# for weighting
    if ("weighting" %in% controllistname) {
        FUN_weighting <- match.fun(re_control$weighting)
        m <- tm::as.DocumentTermMatrix(m, weighting = FUN_weighting)
    } else {
        m <- tm::as.DocumentTermMatrix(m, weighting = tm::weightTf)
    }
    if (totdm) 
        m <- t(m)
	return(m)
}

EAch_tAblE <- function(x) {
    x <- unlist(strsplit(x, "\\s+"))
    tx <- table(x)
    asnu <- as.numeric(tx)
    names(asnu) <- names(tx)
    return(asnu)
}

countHave <- function(a_m) {
    y <- rep(0, ncol(a_m))
	for (i in 1: ncol(a_m)){
		y[i] <- sum(a_m[, i] > 0)
	}
    return(y)
}
