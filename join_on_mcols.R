library(plyranges)
library(DFplyr)

x <- data.frame(seqnames=1, start=1:10, width=1, foo=10:1) |> as_granges()

dat <- data.frame(foo=1:5, bar=11:15)

mcols(x) |> left_join(dat, by="foo")

mcols(x) |> as_tibble() |> left_join(dat)

