instance Monad Parser where
	return a	= Parser (\cs -> Empty (Ok a cs))
	(Parser p) >>= f = Parser (\cs -> case p cs of
		Empty reply1 -> case reply1 of
			Ok x cs' -> parse (f x) cs'
			Error -> Empty Error
		Consumed reply -> Consumed $ case (reply) of
			Ok x rest -> case parse (f x) rest of
				Consumed reply' -> reply'
				Empty reply' -> reply'
			Error -> Error)

instance MonadPlus Parser where
	mzero = Parser (\cs -> Empty Error)
	mplus (Parser p) (Parser q) = Parser (\cs -> case p cs of
		Empty Error -> q cs
		other -> other)
