library(plyranges)

join_overlap_inner_add_distance <- function(x, y, ...) {
  mcols(y)$.y_start <- start(y)
  mcols(y)$.y_end <- end(y)
  olaps <- join_overlap_inner(x, y, ...)
  sx <- start(olaps)
  ex <- end(olaps)
  sy <- mcols(olaps)$.y_start
  ey <- mcols(olaps)$.y_end
  # if x is entirely left of y:
  distance <- ifelse(ex < sy,
                     sy - ex - 1,
  # else if x is entirely right of y
              ifelse(sx > ey,
                     sx - ey - 1,
                     0))
  mcols(olaps)$distance <- distance
  mcols(olaps)$.y_start <- NULL
  mcols(olaps)$.y_end <- NULL
  olaps
}

x <- data.frame(seqnames=1,
                start=sort(sample(19,4,FALSE)),
                width=2, id=1:4) |>
  as_granges()
y <- data.frame(seqnames=1,
                start=sort(sample(19,4,FALSE)),
                width=2, id=letters[1:4]) |>
  as_granges()

x |>
  join_overlap_inner_add_distance(y, maxgap=5L)
