context("scale_art")

describe("art_breaks", {
  it("returns breaks covering full range")
})

describe("scale_art", {
  x <- c(2, 54, 98, 1092, 10992, 88282, 991890)
  f <- scale_art()
  res <- f(x)

  it("Will return a function", {
    expect_is(f, "function")
  })

  it("Makes a function that produces a list of numeric breaks and character labels", {
    expect_is(res, "list")
    expect_is(res[["breaks"]], "numeric")
    expect_is(res[["labels"]], "character")
    expect_is(res[["rmse"]], "numeric")
    expect_length(res[["rmse"]], 1)
  })

  it("returns breaks and labels of equal size", {
    expect_length(res[["breaks"]], length(res[["labels"]]))
  })

  it("Handles presence of 0 with a warning, but no error", {
    x0 <- c(0, 2, 100, 1002)
    expect_warning(f(x0), "scale_art is only intended for values greater than 0.")
  })
})
