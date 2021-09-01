app <- ShinyDriver$new("../../")
app$snapshotInit("brokenPEEK")

app$setInputs(pantherSN = "NA")
app$setInputs(thermocyclerSN = "NA")
app$uploadFile(peekFile = "C:/Users/tla0113/Downloads/peek (1).csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$snapshot()
