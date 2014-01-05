module Main where

	-- Fighting Game command interpreter's main

	import Lexer 
	import Parser
	import Evaluator

	main = do
			(evaluate . parse . tokenize) ["F", "F", "F", "D", "DF", "LP", "B", "DB", "D", "DF", "F", "HP", "B", "D", "DB", "LK", "B", "B"]