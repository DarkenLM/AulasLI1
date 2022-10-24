module MiniTeste_spec where
import Test.HUnit
import MiniTeste
testes = test [
    TestCase $ assertEqual "f [] = []" [] (f []),
    TestCase $ assertEqual "f [(0,1)] = []" [] (f [(0,1)]),
    TestCase $ assertEqual "f [(1,-1)] = [(1,-2)]" [(1,-2)] (f [(1,-1)]),
    TestCase $ assertEqual "f [(1,-2),(3,4),(5,0)] = [(1,-3),(3,3),(5,-1)]" [(1,-3),(3,3),(5,-1)] (f [(1,-2),(3,4),(5,0)]),
    TestCase $ assertEqual "f [(2,1),(1,5),(4,6)] = [(1,4),(4,5)]" [(1,4),(4,5)] (f [(2,1),(1,5),(4,6)])
    ]