module Parser(
  Command(..),
  Tree(..),
  Expression(..),
  parse
  ) where

	-- Here is the grammar the parser is following
	-- Note: No charge command(yet), only based on circle halfcircle etc

	-- Term stand for Terminal

	-- Exp -> Input
	--
	-- Input -> Key |
	--          Direction
	--
	-- Key 			-> Term
	--
	-- Direction 	-> Command Input | 
	--				   Input |
	--				   Term
	--			
	-- Command 		-> DashF |
	--				   DashB
	-- 			 	   QCF |
	-- 			  	   QCB |
	-- 			       HCF |
	-- 			       HCB |
	--			       DPF |
	--			       DPB |
	--
	-- DashF -> Forward Forward
	-- DashB -> Back Back
	-- QCF -> Down DownF Forward
	-- QCB -> Down DownB Back
	-- HCF -> Back DownB Down DownF Forward
	-- HCB -> Forward DownF Down DownB Back
	-- DPF -> Forward Down DownF
	-- DPB -> Back Down DownB

  import Lexer

  data Command  = DashF |
                  DashB |
                  QCF |
                  QCB |
                  HCF |
                  HCB |
                  DPF |
                  DPB	deriving (Show, Eq)

  data Tree = CommandNode Command Tree
			  | AttackNode Attack 
			  | DirectionNode Direction Tree
			  | EmptyNode deriving (Show, Eq)

  type Expression = Tree

  lookAhead :: [Token] -> Token
  lookAhead [] = TokEnd
  lookAhead (x:_) = x

  accept :: [Token] -> [Token]
  accept [] = error "No token to accept"
  accept (x:xs) = xs

  expression, input, direction, quarterCF, quarterCB, halfCF, halfCB, dragonF, dragonB :: [Token] -> (Tree, [Token])

  expression = input

  input toks = 
        case lookAhead toks of
            (TokAtt x) -> (AttackNode x, accept toks)
            (TokDir _) -> direction toks
            TokEnd ->  (EmptyNode, accept toks)

  direction (token1:toks1) = 
	
        case token1 of 

            (TokDir x) -> let (token2, toks2) = (lookAhead toks1, accept toks1)
                          in
                             case token2 of

                                 (TokDir y) -> case x of 

                                                    -- Forward can lead to 2 motions, dragonF and half circle B
                                                    F -> case y of

                                                             F -> let (nextNode, restToks) = input toks2
                                                                  in
                                                                     (CommandNode DashF nextNode, restToks)

                                                             D -> dragonF toks2

                                                             DF -> halfCB toks2

                                                             otherwise -> let (nextNode, restToks) = input toks2
                                                                          in
                                                                             (DirectionNode x (DirectionNode y nextNode), restToks)

                                                    -- Down leads to Quarter Circle F and B
                                                    D -> case y of

                                                             DF -> quarterCF toks2

                                                             DB -> quarterCB toks2

                                                             otherwise -> let (nextNode, restToks) = input toks2
                                                                          in
                                                                             (DirectionNode x (DirectionNode y nextNode), restToks)

                                                    B -> case y of

                                                             B -> let (nextNode, restToks) = input toks2
                                                                  in
                                                                     (CommandNode DashB nextNode, restToks)																

                                                             D -> dragonB toks2

                                                             DB -> halfCF toks2

                                                             otherwise -> let (nextNode, restToks) = input toks2
                                                                          in 
                                                                              (DirectionNode x (DirectionNode y nextNode), toks2)

                                                    otherwise -> let (nextNode, restToks) = input toks1
                                                                 in
                                                                    (DirectionNode x nextNode, restToks)

                                 (TokAtt k) -> (DirectionNode x (AttackNode k), toks2)

                                 TokEnd -> (DirectionNode x EmptyNode, []) 


            otherwise -> error "No direction, in direction"

  halfCF (token1:toks1) = 
        case token1 of 
            (TokDir x) -> let (token2, toks2) = (lookAhead toks1, accept toks1) 
                          in 
                             case x of

                                 D -> case token2 of 

                                          (TokDir y) -> case y of 

                                                            DF -> let (token3, toks3) = (lookAhead toks2, accept toks2)
                                                                  in 
                                                                     case token3 of 

                                                                         (TokDir z) -> let (node, nextToks) = input toks3 
                                                                                       in
                                                                                          case z of

                                                                                          F -> (CommandNode HCF node, nextToks)

                                                                                          otherwise -> (DirectionNode x (DirectionNode y (DirectionNode z node)), nextToks)

                                                                         (TokAtt k) -> (DirectionNode x (DirectionNode y (AttackNode k)), toks3)

                                                                         TokEnd     -> (DirectionNode x (DirectionNode y EmptyNode), [])

                                                            otherwise -> let (node, nextToks) = input toks2
                                                                         in
                                                                            (DirectionNode x (DirectionNode y node), nextToks)

                                          (TokAtt k) -> (DirectionNode x (AttackNode k), toks2)

                                          TokEnd     -> (DirectionNode x EmptyNode, [])

                                 otherwise -> let (node, nextToks) = input toks2
                                              in
                                                 (DirectionNode x node, nextToks)

            (TokAtt k) -> (AttackNode k, toks1)

            TokEnd     -> (EmptyNode, [])

  halfCB (token1:toks1) = 
        case token1 of 
            (TokDir x) -> let (token2, toks2) = (lookAhead toks1, accept toks1) 
                          in 
                             case x of

                                 D -> case token2 of 

                                          (TokDir y) -> case y of 

                                                            DB -> let (token3, toks3) = (lookAhead toks2, accept toks2)
                                                                  in 
                                                                     case token3 of 

                                                                         (TokDir z) -> let (node, nextToks) = input toks3 
                                                                                       in
                                                                                          case z of

                                                                                          B -> (CommandNode HCB node, nextToks)

                                                                                          otherwise -> (DirectionNode x (DirectionNode y (DirectionNode z node)), nextToks)

                                                                         (TokAtt k) -> (DirectionNode x (DirectionNode y (AttackNode k)), toks3)

                                                                         TokEnd     -> (DirectionNode x (DirectionNode y EmptyNode), [])

                                                            otherwise -> let (node, nextToks) = input toks2
                                                                         in
                                                                            (DirectionNode x (DirectionNode y node), nextToks)

                                          (TokAtt k) -> (DirectionNode x (AttackNode k), toks2)

                                          TokEnd     -> (DirectionNode x EmptyNode, [])

                                 otherwise -> let (node, nextToks) = input toks2
                                              in
                                                 (DirectionNode x node, nextToks)

            (TokAtt k) -> (AttackNode k, toks1)

            TokEnd     -> (EmptyNode, [])

  quarterCF (token:toks) = 
        case token of 
            (TokDir x) -> let (node, nextToks) = input toks
                          in
                             case x of
                                 F -> (CommandNode QCF node, nextToks)

                                 otherwise -> (DirectionNode x node, nextToks)

            (TokAtt k) -> (AttackNode k, toks)

            TokEnd     -> (EmptyNode, [])

  quarterCB (token:toks) = 
        case token of 
            (TokDir x) -> let (node, nextToks) = input toks
                          in
                             case x of
                                 B -> (CommandNode QCB node, nextToks)

                                 otherwise -> (DirectionNode x node, nextToks)

            (TokAtt k) -> (AttackNode k, toks)

            TokEnd     -> (EmptyNode, [])

  dragonF (token:toks) =
        case token of
            (TokDir x) -> let (node, nextToks) = input toks
                          in
                             case x of

                                 DF -> (CommandNode DPF node, nextToks)

                                 otherwise -> (DirectionNode x node, nextToks)

            (TokAtt k) -> (AttackNode k, toks)

            TokEnd     -> (EmptyNode, [])

  dragonB (token:toks) = 
        case token of
            (TokDir x) -> let (node, nextToks) = input toks
                          in
                             case x of

                                 DB -> (CommandNode DPB node, nextToks)

                                 otherwise -> (DirectionNode x node, nextToks)

            (TokAtt k) -> (AttackNode k, toks)

            TokEnd     -> (EmptyNode, [])

  parse :: [Token] -> [Expression]
  parse [] = []
  parse toks = 
		let (tree, toks') = expression toks
		in
			tree : parse toks'
