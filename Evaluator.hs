module Evaluator
	   (
	   evaluate
	   ) where

	import Lexer
	import Parser

	-- in fighting game, every character got there own special moves depending on the command.
	-- In this module we could load a file of special command depending the character, 
	-- convert it into a Map, and try to find the corresponding attack, if it exist, with a command.

	evaluate :: [Expression] -> IO ()
	evaluate [] = do putStrLn "Done Evaluating"
	evaluate (x:exprs) = do 
				exprString <- evaluate' x
				putStrLn exprString
				evaluate exprs

	evaluate' :: Expression -> IO String
	evaluate' EmptyNode = do 
				return "Done"

	evaluate' (CommandNode cmd next) = do
						let cmdString = cmd2str cmd
						case next of
							EmptyNode -> return cmdString
							_ -> do 
								nextLine <- evaluate' next
								return ("" ++ cmdString ++ nextLine)

	evaluate' (KeyNode k) = do
					return $ key2str k

	evaluate' (DirectionNode dir next) = do
						let dirString = dir2str dir
						case next of
							EmptyNode -> return dirString
							_ -> do 
								nextLine <- evaluate' next
								return ("" ++ dirString ++ nextLine)

	cmd2str :: Command -> String
	cmd2str DashF 	= "Dash Forward"
	cmd2str DashB	= "Dash Backward"
	cmd2str QCF 	= "QCF"
	cmd2str QCB		= "QCB"
	cmd2str HCF		= "HCF"
	cmd2str HCB		= "HCB"
	cmd2str DPF		= "DPF"
	cmd2str DPB		= "DPB"

	key2str :: Key -> String
	key2str LP = "LP"
	key2str MP = "MP" 
	key2str HP = "HP"
	key2str LK = "LK"
	key2str MK = "MK"
	key2str HK = "HK"

	dir2str :: Direction -> String
	dir2str UB 	= "↖"
	dir2str U 	= "↑"
	dir2str UF	= "↗"
	dir2str F	= "→"
	dir2str DF 	= "↘"
	dir2str D 	= "↓"
	dir2str DB  = "↙"
	dir2str B 	= "←"
