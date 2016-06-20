{-

-- Doesn't work.

class Foo x y
instance Foo Int x
instance Foo x Int

-}


-- Works

class Foo x y
instance Foo Int x
instance Bar x y => Foo x y
class Bar x y
instance Bar x Int