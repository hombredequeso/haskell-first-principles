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

data Id = Id String
            deriving (Eq, Show)   

idParser :: Parser Id
idParser = Id <$> many letter

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

statementParser :: Parser Statement
statementParser = EdgeStatement <$> edgeParser
statementListParser :: Parser [Statement]
statementListParser = do
    _ <- token $ char '{'
    statements <- many statementParser
    _ <- token $ char '}'
    return statements 

parseGraph:: Parser Graph
parseGraph = do
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

    describe "graph components" $ do
        it "can parse an undirected edge" $ do
            let edge = parseString edgeParser mempty "aa -- bb"
            edge `shouldBe` (Success $ Edge (Node $ Id "aa") (Node $ Id "bb"))


