library(plyranges)
library(DFplyr)

x <- data.frame(seqnames=1, start=1:6, width=1, foo=6:1) |> as_granges()
dat <- data.frame(foo=c(1,1,2,2,3), bar=11:15)

mcols(x) |> left_join(dat, by="foo")
mcols(x) |> as_tibble() |> left_join(dat)
x |> join_mcols_left(dat, by="foo") # definition below

# dots passed to DFplyr's left_join
join_mcols_left <- function(x, y, ...) {
  x_id <- x %>%
    mutate(.id = factor(seq_along(.)))
  new_mcols <- x_id |>
    mcols() |>
    left_join(y, ...) |>
    arrange(.id)
  new_x <- x[as.integer(new_mcols$.id)]
  new_mcols <- new_mcols |> select(-.id)
  mcols(new_x) <- NULL
  mcols(new_x) <- new_mcols
  return(new_x)
}
