testServer(expr={
  session$setInputs(bank_button = 'click')
  session$setInputs(up_or_ex = 'use example data')
  session$setInputs(insert_next = "click")
  session$setInputs(insert_end = "click")
  expect_equal(class(getdatalist()[[1]]), "data.frame")
})
