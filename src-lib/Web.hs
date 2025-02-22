{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Web where

import Control.Applicative qualified as Applicative (Alternative (..))
import Data.Map.Merge.Strict qualified as M
import Data.Map.Strict qualified as M
import Data.String (IsString (fromString))
import System.Directory
import System.FilePath
import Text.Blaze.Html.Renderer.Pretty qualified as HtmlString
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data DefaultMap k v
  = DefaultMap
  { dflt :: v
  , mapping :: M.Map k v
  }
  deriving (Show)

insert ::
  FilePath ->
  OrResource a ->
  DefaultMap FilePath (Maybe (OrResource a)) ->
  DefaultMap FilePath (Maybe (OrResource a))
insert resourcePath resourceOrData dm =
  DefaultMap
    { dflt = dm.dflt
    , mapping = M.insert resourcePath (Just resourceOrData) dm.mapping
    }

shift :: (Ord k) => (Maybe k -> k) -> v -> DefaultMap k v -> DefaultMap k v
shift kf f dm =
  DefaultMap
    { dflt = f
    , mapping =
        M.foldlWithKey
          (\acc k v -> M.insert (kf (Just k)) v acc)
          dm.mapping
          (M.singleton (kf Nothing) dm.dflt)
    }

union :: (Ord k) => (v -> v -> v) -> DefaultMap k v -> DefaultMap k v -> DefaultMap k v
union f dm1 dm2 =
  DefaultMap
    { dflt = f dm1.dflt dm2.dflt
    , mapping = M.unionWith f dm1.mapping dm2.mapping
    }

fold :: (c -> k -> v -> c) -> DefaultMap k v -> (v -> c) -> c
fold f dm accf = M.foldlWithKey f (accf dm.dflt) dm.mapping

instance Functor (DefaultMap k) where
  fmap f dm = DefaultMap (f dm.dflt) (fmap f dm.mapping)

instance (Semigroup v, Ord k) => Semigroup (DefaultMap k v) where
  dm1 <> dm2 =
    DefaultMap
      (dm1.dflt <> dm2.dflt)
      (dm1.mapping <> dm2.mapping)

instance (Monoid v, Ord k) => Monoid (DefaultMap k v) where
  mempty = DefaultMap mempty M.empty

instance (Ord k) => Applicative (DefaultMap k) where
  pure x = DefaultMap x M.empty

  liftA2 f m1 m2 =
    DefaultMap
      { dflt = f m1.dflt m2.dflt
      , mapping =
          M.merge
            (M.mapMaybeMissing $ \_ s1 -> Just $ f s1 m2.dflt)
            (M.mapMaybeMissing $ \_ s2 -> Just $ f m1.dflt s2)
            (M.zipWithMaybeMatched $ \_ s1 s2 -> Just $ f s1 s2)
            m1.mapping
            m2.mapping
      }

data OrResource a
  = Data a
  | ResourceRef String
  deriving (Show)

instance Applicative OrResource where
  pure = Data
  liftA2 f o1 o2 =
    case (o1, o2) of
      (Data d1, Data d2) -> Data $ f d1 d2
      _ -> undefined

instance Functor OrResource where
  fmap f = \case
    Data x -> Data $ f x
    ResourceRef r -> ResourceRef r

runDefaultMap :: Web a -> DefaultMap FilePath (Maybe (OrResource a))
runDefaultMap = \case
  Const x -> pure $ Just $ Data x
  Map f l' -> (fmap (fmap f)) <$> runDefaultMap l'
  Lift2 f l1 l2 -> (liftA2 (liftA2 f)) <$> runDefaultMap l1 <*> runDefaultMap l2
  Seg seg l' ->
    shift
      ( \case
          Nothing -> ""
          Just s -> seg </> s
      )
      Nothing
      (runDefaultMap l')
  Or l1 l2 ->
    let dm1 = runDefaultMap l1
        dm2 = runDefaultMap l2
     in union (Applicative.<|>) dm1 dm2
  Refer ref wFn ->
    let refLink = resolve ref
     in runDefaultMap (wFn refLink)
  Resource url wFn ->
    insert url (ResourceRef url) $ runDefaultMap (wFn url)

resolve' :: Web a -> String
resolve' = \case
  Const _ -> "index.html"
  Map _ l' -> resolve' l'
  Lift2 _ l1 l2 -> resolve' l1 Applicative.<|> resolve' l2
  Seg seg l' -> seg </> resolve' l'
  Or l1 l2 -> resolve' l1 Applicative.<|> resolve' l2
  Refer ref wFn -> resolve' $ wFn $ resolve' ref
  Resource url _ -> url

resolve :: Web a -> String
resolve = ("/" <>) . resolve'

mapOfDefaultMap :: DefaultMap FilePath (Maybe (OrResource String)) -> M.Map FilePath (OrResource String)
mapOfDefaultMap dm =
  fold
    ( \acc k v ->
        case v of
          Nothing -> acc
          Just s -> M.insert (k </> "index.html") s acc
    )
    dm
    ( \case
        Nothing -> M.empty
        Just s -> M.singleton "index.html" s
    )

write :: FilePath -> String -> IO ()
write file content = do
  let dir = takeDirectory file
  putStrLn $ "writing to: " <> file
  createDirectoryIfMissing True dir
  writeFile file content

renderMap :: FilePath -> M.Map String (OrResource String) -> IO ()
renderMap dir =
  mapM_
    ( \(filename, contentOrResource) ->
        case contentOrResource of
          Data content -> do
            putStrLn $ "Rendering " <> filename
            write (dir </> filename) content
          ResourceRef r -> do
            putStrLn $ "Copying resource " <> r
            let target = dir </> r
            putStrLn $ "Writing to " <> target
            createDirectoryIfMissing True dir
            copyFile r target
    )
    . M.toList

render :: (a -> String) -> FilePath -> Web a -> IO ()
render renderContent dir w = do
  removePathForcibly dir
  renderMap dir $ mapOfDefaultMap $ runDefaultMap $ fmap renderContent w

renderStr :: FilePath -> Web String -> IO ()
renderStr = render id

renderHtml :: FilePath -> Web H.Html -> IO ()
renderHtml = render HtmlString.renderHtml

data Web a where
  Const :: a -> Web a
  Map :: (a -> b) -> Web a -> Web b
  Lift2 :: (a -> b -> c) -> Web a -> Web b -> Web c
  Refer :: Web a -> (String -> Web a) -> Web a
  Resource :: String -> (String -> Web a) -> Web a
  Seg :: String -> Web a -> Web a
  Or :: Web a -> Web a -> Web a

refer :: Web a -> (String -> Web a) -> Web a
refer = Refer

resource :: String -> (String -> Web a) -> Web a
resource = Resource

instance Functor Web where
  fmap = Map

instance Applicative Web where
  pure = Const
  liftA2 = Lift2

(<|>) :: Web a -> Web a -> Web a
(<|>) = Or

home :: Web H.Html
home =
  resource "felix.jpg" $ \felixImg ->
    resource "lzszt.css" $ \lzsztStylesheet ->
      pure $
        H.docTypeHtml $ do
          H.html mempty H.! A.lang "en"
          H.head $ do
            H.meta H.! A.charset "UTF-8"
            H.meta
              H.! A.name "viewport"
              H.! A.content "width=device-width, inital-scale=1.0"
            H.title "Software Developer | Felix Leitz"
            H.link H.! A.rel "stylesheet" H.! A.href (fromString lzsztStylesheet)
          H.body $ do
            H.div H.! A.class_ "container" $ do
              H.img
                H.! A.src (fromString felixImg)
                H.! A.alt "Felix Leitz"
                H.! A.class_ "profile-img"
              H.h1 "Felix Leitz"
              H.div H.! A.class_ "about" $ do
                H.h2 "About Me"
                H.p "I am a software developer with a strong focus on functional programming using a variety of languages."
                H.p "Another focus besides developing softwaree is managing IT systems."

                H.p "I'm also an iSAQB certified trainer for Software Architecture."
              H.div H.! A.class_ "interests" $ do
                H.h2 "Special Interests"
                H.p "I have a deep interest in Haskell and leverage Nix for defining reproducible development environments and server configurations, ensuring efficiency and maintainability."
              H.div H.! A.class_ "experience" $ do
                H.h2 "Experience"
                H.p $ do
                  "I have a deep interest in Haskell and leverage Nix for defining reproducible development environments and server configurations, ensuring efficiency and maintainability."
              H.div H.! A.class_ "employer" $ do
                H.h2 "Current Employer"
                H.p $ do
                  "I currently work at "
                  H.a "Active Group GmbH"
                    H.! A.href "https://active-group.de"
                    H.! A.target "_blank"
                  " as a Software Architect."

              H.div H.! A.class_ "links" $ do
                H.a "GitHub"
                  H.! A.href "https://github.com/lzszt"
                  H.! A.target "_blank"
                H.a "Contact Me" H.! A.href "mailto:contact@lzszt.info"

lzsztInfo :: Web H.Html
lzsztInfo = home
