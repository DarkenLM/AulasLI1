module Aula5_spec where
import Test.HUnit
import Aula5 --(swapLines, areaQuad, allBut, Matriz)

testAQ1 = TestCase (assertEqual "for l = 2," 4 (areaQuad 2))
testAQ2 = TestCase (assertEqual "for l = 10," 100 (areaQuad 10))
testAQ3 = TestCase (assertEqual "for l = 2," 4 (areaQuad 2))
testAQ = TestList [
    TestLabel "areaQuad 2" testAQ1,
    TestLabel "areaQuad 2" testAQ1,
    TestLabel "areaQuad 2" testAQ1
    ]

testAB1 = TestCase (assertEqual "for l = [\"Foo\", \"Bar\", \"Baz\"], c = 'B'," ["Foo"] (allBut ["Foo", "Bar", "Baz"] 'B'))
testAB2 = 
    TestCase (assertEqual "for l = [\"Foo\", \"Bar\", \"Baz\"], c = 'F'," ["Bar", "Baz"] (allBut ["Foo", "Bar", "Baz"] 'F'))
testAB3 = 
    TestCase (
        assertEqual "for l = [\"Foo\", \"Bar\", \"Baz\"], c = 'A'," ["Foo", "Bar", "Baz"] (allBut ["Foo", "Bar", "Baz"] 'A')
        )
testAB = TestList [
    TestLabel "allBut [\"Foo\", \"Bar\", \"Baz\"] 'B')" testAB1,
    TestLabel "allBut [\"Foo\", \"Bar\", \"Baz\"] 'F')" testAB2,
    TestLabel "allBut [\"Foo\", \"Bar\", \"Baz\"] 'A')" testAB3
    ]

testSL1 = TestCase (assertEqual "for m = [[1,2],[3,4],[5,6]]," [[5,6],[3,4],[1,2]] (swapLines [[1,2],[3,4],[5,6]]))
testSL2 = TestCase (assertEqual "for m = [[1,2],[5,6]]," [[5,6],[1,2]] (swapLines [[1,2],[5,6]]))
testSL3 = TestCase (assertEqual "for m = [[1,2]]," [[1,2]] (swapLines [[1,2]]))
testSL4 = TestCase (assertEqual "for m = []," [] (swapLines []))
testSL = TestList [
    TestLabel "swapLines [[1,2],[3,4],[5,6]]" testSL1,
    TestLabel "swapLines [[1,2],[5,6]]" testSL2,
    TestLabel "swapLines [[1,2]]" testSL3,
    TestLabel "swapLines []" testSL4
    ]

testes_Aula4 = test [
    TestLabel "areaQuad" testAQ,
    TestLabel "allBut" testAB,
    TestLabel "swapLines" testSL
    ]