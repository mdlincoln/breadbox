describe("ideal_breaks", {

  it("errors on bad range", {
    expect_error(ideal_breaks(1, 1:3))
    expect_error(ideal_breaks("a", 1:3))
    expect_error(ideal_breaks(1:2, 1:3))
  })

  it("errors on a bad candidate vector", {
    expect_error(ideal_breaks(1:2, letters))
    expect_error(ideal_breaks(1:2, c(1.2, NA_real_, 2.1)))
  })
})
