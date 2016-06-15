{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

newtype GameMT a = GameMT {runGameMT :: ReaderT CmdOpts (StateT Game IO) a} 
  deriving (
  MonadState Game, 
  MonadReader CmdOpts, 
  MonadIO, 
  Monad,
  Applicative,
  Functor)

runGame :: GameMT a -> Game -> CmdOpts -> IO (a, Game)
runGame mt game cmdopts = flip runStateT game . flip runReaderT cmdopts $ runGameMT mt

type Tick = Integer

type CmdOpts = [String]

data Game = Game {
  players :: [Player],
  tickNumber :: Tick
  }

data Player = Player {
  score :: Integer,
  name :: String
  }

modifyGet :: MonadState s m => (s -> s) -> (s -> a) -> m a
modifyGet f g = state $ (g . f) &&& f 

advanceTick :: Game -> Game
advanceTick oldgame = oldgame {tickNumber = (+1) . tickNumber $ oldgame}

doTick :: ReaderT CmdOpts (StateT Game IO) ()
doTick = do
  tick <- modifyGet advanceTick tickNumber
  io . putStrLn $ "Starting Tick Number " ++ show tick
  when (tick `mod` 10 == 0) $ io . putStrLn $ "10th tick"
  
  where
    io = liftIO
