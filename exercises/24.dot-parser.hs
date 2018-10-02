{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DotParser where

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

data Id = Id String
            deriving (Eq, Show)   

validNonInitialCharParser :: Parser Char
validNonInitialCharParser = alphaNum <|> char '_'

validInitialCharParser :: Parser Char
validInitialCharParser = letter <|> char '_'

idParser :: Parser Id
--idParser = Id <$> ( token $ many validIdCharParser )
idParser = do
    a <- validInitialCharParser
    as <- many validNonInitialCharParser
    return (Id ( a:as ))

data GraphType =
    UndirectedGraph |
        DirectedGraph
            deriving (Eq, Show)   

graphTypeParser :: Parser GraphType
graphTypeParser = ( string "graph" >> pure UndirectedGraph )
                     <|> ( string "digraph" >> pure DirectedGraph )

data Graph = Graph GraphType (Maybe Id) [ Statement ]
    deriving (Eq, Show)

data Node = Node Id
            deriving (Eq, Show)   
data Edge = Edge Node Node
            deriving (Eq, Show)   

type EdgeStatement = [Node]

nodeParser :: Parser Node
nodeParser = Node <$> idParser

undirectedEdgeTokenParser :: Parser ()
undirectedEdgeTokenParser = token (string "--") >> return ()

edgeParser :: Parser Edge
edgeParser = 
    token nodeParser >>= \node1 ->
    undirectedEdgeTokenParser >>
    token nodeParser >>= \node2 ->
    return (Edge node1 node2)

data Statement =
    EdgeStatement Edge
      | NodeStatement Node
    deriving (Eq, Show)

bracketedSectionParser :: Char -> Char -> Parser a -> Parser a
bracketedSectionParser startChar endChar innerParser =
    ( token $ char startChar ) 
    *> innerParser 
    <* ( token $ char endChar )

statementParser :: Parser Statement
statementParser = EdgeStatement <$> edgeParser

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
parseGraph = do
    _ <- whiteSpace
    graphType <- graphTypeParser
    _ <- whiteSpace
    graphName <- optional idParser
    _ <- whiteSpace
    statementList <- statementListParser
    return $ Graph graphType graphName statementList 


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
                                    (EdgeStatement $ Edge (Node (Id "aa")) (Node (Id "bb")) )
                                ]
            graph `shouldBe` (Success expectedResult)

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
                                    (EdgeStatement $ Edge (Node (Id "a")) (Node (Id "b")) ),
                                    (EdgeStatement $ Edge (Node (Id "b")) (Node (Id "c")) ),
                                    (EdgeStatement $ Edge (Node (Id "a")) (Node (Id "c")) ),
                                    (EdgeStatement $ Edge (Node (Id "d")) (Node (Id "c")) ),
                                    (EdgeStatement $ Edge (Node (Id "e")) (Node (Id "c")) ),
                                    (EdgeStatement $ Edge (Node (Id "e")) (Node (Id "a")) )
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
                                    (EdgeStatement $ Edge (Node (Id "a")) (Node (Id "b")) ),
                                    (EdgeStatement $ Edge (Node (Id "b")) (Node (Id "c")) ),
                                    (EdgeStatement $ Edge (Node (Id "a")) (Node (Id "c")) ),
                                    (EdgeStatement $ Edge (Node (Id "d")) (Node (Id "c")) ),
                                    (EdgeStatement $ Edge (Node (Id "e")) (Node (Id "c")) ),
                                    (EdgeStatement $ Edge (Node (Id "e")) (Node (Id "a")) )
                                ]
            graph `shouldBe` (Success expectedResult)



        -- it "can make a basic undirected graph with multiple undirected edges" $ do
        --     let graphStr = [r|graph name { aa -- bb; cc -- dd }|]
        --     let graph = testParseGraph graphStr

        --     let expectedResult = Graph 
        --                         UndirectedGraph 
        --                         (Just $ Id "name") 
        --                         [
        --                             (EdgeStatement $ Edge (Node (Id "aa")) (Node (Id "bb")) ),
        --                             (EdgeStatement $ Edge (Node (Id "cc")) (Node (Id "dd")) )
        --                         ]
        --     graph `shouldBe` (Success expectedResult)

    describe "graph components" $ do
        it "can parse an undirected edge" $ do
            let edge = parseString edgeParser mempty "aa -- bb"
            edge `shouldBe` (Success $ Edge (Node $ Id "aa") (Node $ Id "bb"))
    
    describe "manyStatementsParser" $ do
        it "can parse zero undirected edgeOps" $ do
            let statements  = ( parseString manyStatementsParser mempty "" ) :: Result [Statement]
            statements `shouldBe` (Success [])

        -- TODO: deal with whitespace only issues
        -- it "can parse zero undirected edgeOps with whitespace string" $ do
        --     let statements  = ( parseString manyStatementsParser mempty " " ) :: Result [Statement]
        --     statements `shouldBe` (Success [])

        it "can parse one undirected edgeOps" $ do
            let statements  = ( parseString manyStatementsParser mempty "aa -- bb" ) :: Result [Statement]
            let expectedResult = 
                                [
                                    (EdgeStatement $ Edge (Node (Id "aa")) (Node (Id "bb")) )
                                ]

            statements `shouldBe` (Success expectedResult)


        it "can parse multiple undirected edgeOps" $ do
            let statements  = ( parseString manyStatementsParser mempty "aa -- bb;cc --dd" ) :: Result [Statement]
            let expectedResult = 
                                [
                                    (EdgeStatement $ Edge (Node (Id "aa")) (Node (Id "bb")) ),
                                    (EdgeStatement $ Edge (Node (Id "cc")) (Node (Id "dd")) )
                                ]

            statements `shouldBe` (Success expectedResult)

        it "can parse multiple undirected edgeOps with a semicolon at end" $ do
            let statements  = ( parseString manyStatementsParser mempty "aa -- bb;cc --dd;" ) :: Result [Statement]
            let expectedResult = 
                                [
                                    (EdgeStatement $ Edge (Node (Id "aa")) (Node (Id "bb")) ),
                                    (EdgeStatement $ Edge (Node (Id "cc")) (Node (Id "dd")) )
                                ]

            statements `shouldBe` (Success expectedResult)

    describe "idParser" $ do
        let parseId = parseString idParser mempty
        it "can parse various types of ID's successfully" $ do
            (parseId "abc") `shouldBe` (Success $ Id "abc")
            (parseId "abcdefghijklmnopqrstuvwxyz") `shouldBe` (Success $ Id "abcdefghijklmnopqrstuvwxyz")
            (parseId "ABCDEFGHIJKLMNOPQRSTUVWXYZ") `shouldBe` (Success $ Id "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
            (parseId "a0123456789") `shouldBe` (Success $ Id "a0123456789")
            (parseId "_abc") `shouldBe` (Success $ Id "_abc")

        -- it "fails with invalid ID's" $ do
        --     (parseId "   ") `shouldBe` (Success $ Id "abc")


