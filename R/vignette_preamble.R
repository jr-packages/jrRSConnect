# Taken from knitr split_lines
split_lines = function(x) {
  if (length(grep("\n", x)) == 0L)
    return(x)
  x = gsub("\n$", "\n\n", x)
  x[x == ""] = "\n"
  unlist(strsplit(x, "\n"))
}


#' @importFrom stringi stri_wrap
#' @importFrom knitr knit_hooks
make_preamble = function() {
  
  knitr::opts_chunk$set(tidy = FALSE, echo = TRUE, highlight = TRUE, comment =
                        "#>", collapse = TRUE, cache = TRUE, fig.align =
                        "center", fig.width = 4, fig.height = 4, linewidth = 59,
                        fig.retina = 1, out.width = "100%", out.height = "100%",
                        dev = NULL, dpi = 192,  purl = FALSE,
                        dev.args = list(png = list(type = "cairo-png")))
  
  hook_output = knitr::knit_hooks$get("output")
  
  knitr::knit_hooks$set(output = function(x, options) {
      if (!is.null(n <- options$linewidth)) {
          x = split_lines(x)
          if (any(nchar(x) > n))
              x = stringi::stri_wrap(x, width = n, normalise = FALSE)
          need_comment = grep(pattern = comment, x, invert = TRUE)
          need_comment = need_comment[-length(need_comment)]
          x[need_comment] = paste(comment, x[need_comment])
          x = paste(x, collapse = "\n")
      }
      hook_output(x, options)
  })
  
  hook_old = knitr::knit_hooks$get("chunk")
  knitr::knit_hooks$set(chunk = function(x, options) {
      if (!is.null(options$code.cap)) {
          tex = paste0("\\marginpar{\\captionof{chunk}{", options$code.cap,
              "}")
          if (!is.null(options$label)) {
              tex = paste0(tex, "\\label{chk:", options$label,
                "}")
          }
          x = paste0(tex, "}", x)
      }
      hook_old(x, options)
  })
}
