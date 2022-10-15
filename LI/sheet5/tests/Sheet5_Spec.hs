import Test.HUnit
import Sheet5

powerEnumFromTests = TestList [
  "for powerEnumFrom 2 4," ~: [1,2,4,8,16] ~=? powerEnumFrom 2 4
  ]

filterAgeTests = TestList [
  "for filterAge ...," ~: ["Ana","Amadeu"] ~=? filterAge 2022 18 [("Ana", 2004), ("Rui", 2007), ("Amadeu", 2000)]
  ]

colaTests = TestList [
  "for cola ...," ~: "AnaRuiAmadeu" ~=? cola [("Ana", 3, 5), ("Rui", 7, 6), ("Amadeu", 5, 2)]
  ]

myGroupTests = TestList [
  "for myGroup ...," ~: [[1],[2,2],[3],[4,4,4],[5],[4]] ~=? myGroup [1,2,2,3,4,4,4,5,4]
  ]


testes_Sheet5 = TestList [
  powerEnumFromTests,
  filterAgeTests,
  colaTests,
  myGroupTests
  ]
