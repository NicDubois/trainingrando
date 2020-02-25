context("AddNewSubject")
library(trainingrando)

test_that("AddNewSubject output has the appropriate length and class", {
  myprogfact <- CreatePrgFact(progfactLabel = "Age Group",progfactLevels=c("0-18 years", "19-35 years","36-60 years","> 60 years"))
  myArms <- ArmLevelProg(data.frame("Name" = c('Placebo', 'Treatement1','Treatement2','Treatement3')))
  myRandoDataFrame <- AddFirstSubject(progfact=myprogfact,Arms=myArms)

  expect_equal(dim(myRandoDataFrame),c(1,3))
  expect_equal(class(myRandoDataFrame),c("TrainRandoSubj","data.frame"))
})
