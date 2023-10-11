# see https://babeheim.com/blog/2022-11-02-dynamic-latex/
prep_latex_variables <- function(named_list) {
     out <- character()
     for (i in 1:length(named_list)) {
          out[i] <- paste0("\\newcommand{\\", names(named_list)[i], "}{", named_list[[i]], "}")
     }
     return(out)
}

post <- extract.samples(m_raw_tc)
calcs <- list(
     mean_tc_hammer_weight = prettyNum(mean(exp(post$a_tc)), big.mark = ","), # assuming an `obs` data frame
     raw_tc_hammer_weight = prettyNum(mean(exp(post$a_raw)), big.mark = ",") # assuming an `obs` data frame
     
     #mean_tc_hammer_weight = prettyNum(nrow(obs), big.mark = ","), # assuming an `obs` data frame
     # nPpl = nrow(ppl),                            # assuming a `ppl` data frame
     # meanHeight = round(mean(obs$height), 1)
)

writeLines(prep_latex_variables(calcs), "carlson_ch1_selectivity/calcs.tex")

