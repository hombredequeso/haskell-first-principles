{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DotParser where

-- http://www.graphviz.org/doc/info/lang.html

import Data.Word
import Text.Trifecta
import Text.Parser.Token
import Text.Parser.Char

import Text.RawString.QQ

import Test.Hspec
import Test.QuickCheck hiding (Success, Failure, Result) -- Clashes with Text.Trifecta
import Control.Applicative

-- A few useful links:
--https://hackage.haskell.org/package/parsers-0.12.9/docs/src/Text.Parser.Combinators.html

validNonInitialCharParser :: Parser Char
validNonInitialCharParser = alphaNum <|> char '_'

validInitialCharParser :: Parser Char
validInitialCharParser = letter <|> char '_'

idParser :: Parser Id
idParser = token (Id <$> ((:) <$> validInitialCharParser <*> (many validNonInitialCharParser)))

attributeSeparatorParser :: Parser Char
attributeSeparatorParser = token(char ';' <|> char ',')


attributeParser :: Parser Attribute
attributeParser = Attribute <$> idParser <* token(char '=') <*> idParser

attributeListParser :: Parser [Attribute]
attributeListParser = bracketedSectionParser '[' ']' aListParser

aListParser :: Parser [Attribute]
aListParser = sepListParser attributeParser attributeSeparatorParser

-- Two pretty general functions, essentially repeats of 
--  https://hackage.haskell.org/package/parsers-0.12.9/docs/Text-Parser-Combinators.html
--      sepEndBy** functions
sepListParser :: Parser a -> Parser sep -> Parser [a]
sepListParser a sep = ( sepList1Parser a sep ) <|> (pure [])

sepList1Parser :: Parser a -> Parser sep -> Parser [a]
sepList1Parser a sep = (:) <$> a <*> ( ( sep *> (sepListParser a sep) ) <|> pure [] )

graphTypeParser :: Parser GraphType
graphTypeParser = ( string "graph" >> pure UndirectedGraph )
                     <|> ( string "digraph" >> pure DirectedGraph )

data Graph = Graph GraphType (Maybe Id) [ Statement ]
    deriving (Eq, Show)

data GraphType =
    UndirectedGraph |
        DirectedGraph
            deriving (Eq, Show)   

data Id = Id String
            deriving (Eq, Show)   

data Statement =
    EdgeStatement Edge [Attribute]
      | DirectedEdgeStatement DirectedEdge [Attribute]
      | NodeStatement Node
    deriving (Eq, Show)

data Node = Node Id
            deriving (Eq, Show)   
data Edge = Edge [Node]
            deriving (Eq, Show)   
data DirectedEdge = DirectedEdge [Node]
            deriving (Eq, Show)   

data Attribute = Attribute Id Id
                    deriving (Eq, Show)

nodeParser :: Parser Node
nodeParser = Node <$> idParser

undirectedEdgeTokenParser :: Parser ()
undirectedEdgeTokenParser = token (string "--") >> return ()

directedEdgeTokenParser :: Parser ()
directedEdgeTokenParser = token (string "->") >> return ()

sepBy2 :: Parser a -> Parser sep -> Parser [a] 
-- Simple to understand. But seems quite a naive implementation:
-- sepBy2 pa psep = pa >>= \a -> psep >> pa `sepBy1` psep >>= \as -> return (a:as)
-- Awesome: that's a wonder to behold (and it works)
-- Note that the later part could be replaced with sepBy1,
-- but this is the full thing in all its glory.
-- sepBy2 pa psep = (:) <$> pa <*> ((:) <$> (psep *> pa) <*> (many ( psep *> pa )))
sepBy2 pa psep = (:) <$> pa <*>  ( (:) <$> (psep *> pa) <*> (many ( psep *> pa )))

edgeParser :: Parser Edge
edgeParser = Edge <$> (token nodeParser) `sepBy2` undirectedEdgeTokenParser

directedEdgeParser :: Parser DirectedEdge
directedEdgeParser = DirectedEdge <$> ( (token nodeParser) `sepBy2` directedEdgeTokenParser ) 

bracketedSectionParser :: Char -> Char -> Parser a -> Parser a
bracketedSectionParser startChar endChar innerParser =
    ( token $ char startChar ) 
    *> innerParser 
    <* ( token $ char endChar )

statementParser :: Parser Statement
statementParser = try (EdgeStatement <$> edgeParser <*> pure [] )
                    <|> ( DirectedEdgeStatement <$> directedEdgeParser  <*> pure [])

statementListParser :: Parser [Statement]
statementListParser = between 
                        (token $ char '{') 
                        (token $ char '}') 
                        manyStatementsParser

statementEnd :: Parser ()
statementEnd = char ';' >> spaces

manyStatementsParser :: Parser [Statement]
manyStatementsParser = statementParser `sepEndBy` statementEnd

parseGraph:: Parser Graph
parseGraph = Graph <$> 
                ( whiteSpace *> graphTypeParser ) <*> 
                ( whiteSpace *> (optional idParser) ) <*> 
                ( whiteSpace *> statementListParser )

instance Eq a =>  Eq (Result  a) where
    (Success x) == (Success y) =  x == y
    (Failure x) == (Success y) = False
    (Success x) == (Failure y) = False
    (Failure x) == (Failure y) = True

main :: IO ()
main = hspec $ do

    describe "graph" $ do
        it "can make a basic empty graph" $ do

            let testGraph = Graph UndirectedGraph (Just $ Id "abc") []
            let (Graph graphType id statements) = testGraph
            graphType `shouldBe` UndirectedGraph



    let testParseGraph = parseString parseGraph mempty

    describe "parseGraph" $ do
        it "can parse a basic empty graph" $ do
            let basicEmptyGraph = [r|graph name {}|]
            let graph = testParseGraph basicEmptyGraph
            graph `shouldBe` ( Success $ Graph UndirectedGraph (Just $ Id "name") [] )


        it "can make a basic undirected graph with one edge" $ do
            let graphStr = [r|graph name { aa -- bb }|]
            let graph = testParseGraph graphStr

            let expectedResult = Graph 
                                UndirectedGraph 
                                (Just $ Id "name") 
                                [
                                    (EdgeStatement ( Edge [(Node (Id "aa")), (Node (Id "bb"))] ) [] )
                                ]
            graph `shouldBe` (Success expectedResult)

-- TODO: next test to try:
        -- it "can make a basic undirected graph with one edge and attributes" $ do
        --     let graphStr = [r|graph name { aa -- bb [mmm=nnn;ppp=qqq]}|]
        --     let graph = testParseGraph graphStr

        --     let expectedResult = Graph 
        --                         UndirectedGraph 
        --                         (Just $ Id "name") 
        --                         [
        --                             (EdgeStatement 
        --                                 ( Edge [(Node (Id "aa")), (Node (Id "bb"))] ) 
        --                                 [( Attribute (Id "mmm") ( Id "nnn" ) ), ( Attribute (Id "ppp" ) (Id "qqq" ) )] )
        --                         ]
        --     graph `shouldBe` (Success expectedResult)

        it "can parse a basic multiline graph with multiple edges" $ do
            let graphStr = [r|
graph {
    a -- b;
    b -- c;
    a -- c;
    d -- c;
    e -- c;
    e -- a;
}
|]

            let graph = testParseGraph graphStr

            let expectedResult = Graph 
                                UndirectedGraph 
                                (Nothing) 
                                [
                                    (EdgeStatement  ( Edge [ (Node (Id "a")), (Node (Id "b")) ] )  [] ),
                                    (EdgeStatement  ( Edge [ (Node (Id "b")), (Node (Id "c")) ] ) [] ),
                                    (EdgeStatement  ( Edge [ (Node (Id "a")), (Node (Id "c")) ] ) [] ),
                                    (EdgeStatement  ( Edge [ (Node (Id "d")), (Node (Id "c")) ] ) [] ),
                                    (EdgeStatement  ( Edge [ (Node (Id "e")), (Node (Id "c")) ] ) [] ),
                                    (EdgeStatement  ( Edge [ (Node (Id "e")), (Node (Id "a")) ] ) [] )
                                ]
            graph `shouldBe` (Success expectedResult)

        it "can parse a basic digraph" $ do
            let graphStr = [r|
digraph {
    a -> b;
    b -> c;
    c -> d;
    d -> a;
}
|]

            let graph = testParseGraph graphStr

            let expectedResult = Graph 
                                DirectedGraph 
                                (Nothing) 
                                [
                                    (DirectedEdgeStatement  ( DirectedEdge [ (Node (Id "a")), (Node (Id "b")) ] ) [] ),
                                    (DirectedEdgeStatement  ( DirectedEdge [ (Node (Id "b")), (Node (Id "c")) ] ) [] ),
                                    (DirectedEdgeStatement  ( DirectedEdge [ (Node (Id "c")), (Node (Id "d")) ] ) [] ),
                                    (DirectedEdgeStatement  ( DirectedEdge [ (Node (Id "d")), (Node (Id "a")) ] ) [] )
                                ]
            graph `shouldBe` (Success expectedResult)

    describe "graph components" $ do
        it "can parse an undirected edge" $ do
            let edge = parseString edgeParser mempty "aa -- bb"
            edge `shouldBe` (Success $ Edge [ (Node $ Id "aa"), (Node $ Id "bb") ])
    
    describe "manyStatementsParser" $ do
        it "can parse zero undirected edgeOps" $ do
            let statements  = ( parseString manyStatementsParser mempty "" ) :: Result [Statement]
            statements `shouldBe` (Success [])


        it "can parse one undirected edgeOps" $ do
            let statements  = ( parseString manyStatementsParser mempty "aa -- bb" ) :: Result [Statement]
            let expectedResult = 
                                [
                                    (EdgeStatement  ( Edge [ (Node (Id "aa")), (Node (Id "bb")) ] ) [] )
                                ]

            statements `shouldBe` (Success expectedResult)


        it "can parse one directed edgeOps" $ do
            let statements  = ( parseString manyStatementsParser mempty "aa -> bb" ) :: Result [Statement]
            let expectedResult = 
                                [
                                    (DirectedEdgeStatement  ( DirectedEdge [ (Node (Id "aa")), (Node (Id "bb")) ] ) [] )
                                ]

            statements `shouldBe` (Success expectedResult)


        it "can parse multiple undirected edgeOps" $ do
            let statements  = ( parseString manyStatementsParser mempty "aa -- bb;cc --dd" ) :: Result [Statement]
            let expectedResult = 
                                [
                                    (EdgeStatement  ( Edge [ (Node (Id "aa")), (Node (Id "bb")) ] ) [] ),
                                    (EdgeStatement  ( Edge [ (Node (Id "cc")), (Node (Id "dd")) ] ) [])
                                ]

            statements `shouldBe` (Success expectedResult)

        it "can parse multiple undirected edgeOps with a semicolon at end" $ do
            let statements  = ( parseString manyStatementsParser mempty "aa -- bb;cc --dd;" ) :: Result [Statement]
            let expectedResult = 
                                [
                                    (EdgeStatement  ( Edge [(Node (Id "aa")), (Node (Id "bb")) ] ) [] ),
                                    (EdgeStatement  ( Edge [(Node (Id "cc")), (Node (Id "dd")) ] ) [] )
                                ]

            statements `shouldBe` (Success expectedResult)

        it "can parse multiple undirected edgeOps, in a continuous sequence" $ do
            let statements  = ( parseString manyStatementsParser mempty "aa -- bb -- cc --dd;" ) :: Result [Statement]
            let expectedResult = 
                                [
                                    (EdgeStatement  ( Edge [(Node (Id "aa")), (Node (Id "bb")), (Node (Id "cc")), (Node (Id "dd")) ] ) [] )
                                ]

            statements `shouldBe` (Success expectedResult)


    describe "idParser" $ do
        let parseId = parseString idParser mempty
        it "can parse various types of ID's successfully" $ do
            (parseId "abc") `shouldBe` (Success $ Id "abc")
            (parseId "abcdefghijklmnopqrstuvwxyz") `shouldBe` 
                (Success $ Id "abcdefghijklmnopqrstuvwxyz")
            (parseId "ABCDEFGHIJKLMNOPQRSTUVWXYZ") `shouldBe` 
                (Success $ Id "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
            (parseId "a0123456789") `shouldBe` (Success $ Id "a0123456789")
            (parseId "_abc") `shouldBe` (Success $ Id "_abc")

        -- it "fails with invalid ID's" $ do
        --     (parseId "   ") `shouldBe` (Success $ Id "abc")

    describe "attributeListParser" $ do
        let parseAttributeList = parseString attributeListParser mempty
        it "can parse a one element attribute list" $ do
            let expectedResult = [Attribute ( Id "abc" ) (Id "xyz")]
            (parseAttributeList "[abc=xyz]") `shouldBe` (Success $ expectedResult)

        let expectedResultForVariations = 
                [
                Attribute ( Id "abc" ) (Id "xyz"),
                Attribute ( Id "mno" ) (Id "pqr")
                ]

        it "can parse a multi-element attribute list" $ do
            (parseAttributeList "[abc=xyz;mno=pqr]") `shouldBe` 
                (Success $ expectedResultForVariations)

        it "can parse a multi-element attribute list with whitespace" $ do
            (parseAttributeList "[abc=xyz ; mno = pqr]") `shouldBe` 
                (Success $ expectedResultForVariations)

        it "can parse a multi-element attribute list ending with separator" $ do
            (parseAttributeList "[abc=xyz;mno=pqr;]") `shouldBe` 
                (Success $ expectedResultForVariations)
