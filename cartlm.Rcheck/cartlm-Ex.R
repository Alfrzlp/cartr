pkgname <- "cartlm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('cartlm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("cartLM")
### * cartLM

flush(stderr()); flush(stdout())

### Name: cartLM
### Title: CART with Linear Model
### Aliases: cartLM

### ** Examples

cartLM(len ~ ., data = ToothGrowth)



cleanEx()
nameEx("cartlm-package")
### * cartlm-package

flush(stderr()); flush(stdout())

### Name: cartlm-package
### Title: Classific
### Aliases: cartlm-package cartlm
### Keywords: package

### ** Examples

~~ simple examples of the most important functions ~~



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
