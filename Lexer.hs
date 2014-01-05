module Lexer (
	Direction(..),
	Key(..),
	Token (..),
	tokenize
	) where 

	data Direction = UB | U | UF | F | DF | D | DB | B deriving (Show, Eq)
	data Key = LP | MP | HP | LK | MK | HK deriving (Show, Eq)

	data Token = TokDir Direction | TokKey Key | TokEnd deriving (Show, Eq)

	tokenize :: [String] -> [Token]
	tokenize [] = TokEnd : []
	tokenize (x:xs) | isDirection x = TokDir (direction x) : tokenize xs
					| isKey x = TokKey (key x) : tokenize xs

	isDirection :: String -> Bool
	isDirection str = elem str ["UB", "U", "UF", "F", "DF", "D", "DB", "B"]

	isKey :: String -> Bool
	isKey str = elem str ["LP", "MP", "HP", "LK", "MK", "HK"]

	direction :: String -> Direction
	direction str | str == "UB" = UB
				  | str == "U" = U
				  | str == "UF" = UF
				  | str == "F" = F
				  | str == "DF" = DF
				  | str == "D" = D
				  | str == "DB" = DB
				  | str == "B" = B

	key :: String -> Key
	key str | str == "LP" = LP
			| str == "MP" = MP
			| str == "HP" = HP
			| str == "LK" = LK
			| str == "MK" = MK
			| str == "HK" = HK
