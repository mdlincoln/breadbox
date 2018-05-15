context("scale_art")

describe("scale_art", {
  it("Will return a function", {
    f <- scale_art()
    expect_is(f, "function")
  })

  it("Makes a function that produces a list of numeric breaks and character labels", {
    x <- c(54, 98, 1092, 10992, 991890)
    res <- f(x)
    expect_is(res, "list")
    expect_is(res[["breaks"]], "numeric")
    expect_is(res[["labels"]], "character")
    expect_is(res[["rmse"]], "numeric")
    expect_length(res[["rmse"]], 1)
  })

  it("returns breaks and labels of equal size", {
    expect_length(res[["breaks"]], length(res[["labels"]]))
  })
})
