module Ch28.Functor where

type Id = ()

type Component = ()

data Context = Context
  { nodes :: [(Id, Component)],
    visited :: [Id],
    position :: [Id]
  }

newtype Check a = Check
  { run :: Context -> Either String (Context, a)
  }

instance Functor Check where
  fmap f c =
    Check
      { run = \ctx -> fmap (\(c, a) -> (c, f a)) (run c ctx)
      }

instance Applicative Check where
  pure a =
    Check
      { run = \ctx -> Right (ctx, a)
      }
  f <*> c = undefined

instance Monad Check where
  fa >>= f = undefined

getCtx :: Check Context
getCtx =
  Check
    { run = \ctx -> Right (ctx, ctx)
    }

failCheck :: String -> Check a
failCheck msg =
  Check
    { run = \_ -> Left msg
    }

failCheck' :: String -> Check a
failCheck' msg = do
  ctx <- getCtx
  let msg' = show (position ctx) ++ msg
  failCheck msg'

vowels = ['a', 'e']

stops = ['c', 'b']

vsv = [(v1, s, v1) | v1 <- vowels, s <- stops, v2 <- vowels]
