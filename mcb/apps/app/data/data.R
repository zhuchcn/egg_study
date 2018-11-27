# mcb = new.env()
# load("path/to/mcb_precalc.rda", envir = mcb)

bga = new.env()
lod("path/to/bga_precalc.rda", envir = bga)

# bac = new.evn()
# load("path/to/bac_precalc.rda", envir = bac)

data = list(
    data = list(
        
    ),
    lm = list(
        
    ),
    corr = list(
        
    )
)
save(data, "data.rda")