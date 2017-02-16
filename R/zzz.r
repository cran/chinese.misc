.onLoad <- function(libname, pkgname) {
	env <- asNamespace(pkgname)
	assign('DEFAULT_cutter', jiebaR::worker(write = FALSE), envir = env)
	assign('DEFAULT_spliter', NLP::as.Token_Tokenizer(NLP::Regexp_Tokenizer("\\s", invert = TRUE)), envir = env)
	assign('DEFAULT_control1', list(wordLengths = c(1, 25), tokenizer = DEFAULT_spliter), envir = env)
	assign('DEFAULT_control2', list(wordLengths = c(2, 25), tokenizer = DEFAULT_spliter), envir = env)	
}
