-- | Import/Export JSON support
{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings, FlexibleContexts, LambdaCase #-}
module Lamdu.Data.Export.JSON
    ( fileExportRepl, jsonExportRepl
    , fileExportAll
    , fileExportDef
    , fileImportAll
    ) where

-- TODO: Schema version? What granularity?

import qualified Control.Lens as Lens
import           Control.Lens.Operators hiding ((.=))
import           Control.Lens.Tuple
import           Control.Monad (unless)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import           Control.Monad.Trans.Writer (WriterT(..))
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import           Data.Binary (Binary)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Either.Combinators (swapEither)
import           Data.Foldable (traverse_)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.IRef (IRef)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.DbLayout (ViewM)
import qualified Lamdu.Data.DbLayout as DbLayout
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Export.JSON.Codec as Codec
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Expr.Nominal (Nominal)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.UniqueId (ToUUID)
import qualified Lamdu.Expr.Val as V
import           Lamdu.Expr.Val.Annotated (Val(..))

import           Prelude.Compat

type T = Transaction

data Visited = Visited
    { _visitedDefs :: Set V.Var
    , _visitedTags :: Set T.Tag
    , _visitedNominals :: Set T.NominalId
    }
Lens.makeLenses ''Visited

type Export = WriterT [Codec.Encoded] (StateT Visited (T ViewM))

runExport :: Export a -> T ViewM (a, Codec.Encoded)
runExport act =
    act
    & runWriterT
    <&> _2 %~ Codec.encodeArray
    & (`State.evalStateT` Visited mempty mempty mempty)

trans :: T ViewM a -> Export a
trans = lift . lift

withVisited :: Ord a => Lens.ALens' Visited (Set a) -> a -> Export () -> Export ()
withVisited l x act =
    do
        alreadyVisited <- Lens.use (Lens.cloneLens l . Lens.contains x)
        unless alreadyVisited $
            do
                Lens.assign (Lens.cloneLens l . Lens.contains x) True
                act

readAssocName :: Monad m => ToUUID a => a -> T m (Maybe String)
readAssocName x =
    do
        name <- Anchors.assocNameRef x & Transaction.getP
        return $
            if null name
            then Nothing
            else Just name

tell :: Codec.Encoded -> Export ()
tell = Writer.tell . (: [])

exportTag :: T.Tag -> Export ()
exportTag tag =
    do
        tagOrder <- Transaction.getP (Anchors.assocTagOrder tag) & trans
        mName <- readAssocName tag & trans
        Codec.encodeNamedTag (tagOrder, mName, tag) & tell
    & withVisited visitedTags tag

exportNominal :: T.NominalId -> Export ()
exportNominal nomId =
    do
        nominal <- trans (Load.nominal nomId)
        mName <- readAssocName nomId & trans
        Codec.encodeNamedNominal ((mName, nomId), nominal) & tell
        & withVisited visitedNominals nomId

exportSubexpr :: Val (ValI ViewM) -> Export ()
exportSubexpr (Val lamI (V.BLam (V.Lam lamVar _))) =
    do
        mName <- readAssocName lamVar & trans
        mParamList <- Transaction.getP (Anchors.assocFieldParamList lamI) & trans
        Codec.encodeNamedLamVar (mParamList, mName, valIToUUID lamI, lamVar) & tell
exportSubexpr _ = return ()

exportVal :: Val (ValI ViewM) -> Export ()
exportVal val =
    do
        val ^.. ExprLens.valGlobals mempty & traverse_ (exportDef . ExprIRef.defI)
        val ^.. ExprLens.valTags & traverse_ exportTag
        val ^.. ExprLens.valNominals & traverse_ exportNominal
        val ^.. ExprLens.subExprs & traverse_ exportSubexpr

valIToUUID :: ValI m -> UUID
valIToUUID = IRef.uuid . ExprIRef.unValI

exportDef :: ExprIRef.DefI ViewM -> Export ()
exportDef defI =
    do
        presentationMode <- Transaction.getP (Anchors.assocPresentationMode globalId) & trans
        mName <- readAssocName globalId & trans
        def <-
            Load.def defI & trans
            <&> Definition.defBody . Lens.mapped . Lens.mapped %~ Property.value
        traverse_ exportVal (def ^. Definition.defBody)
        let def' = def & Definition.defBody . Lens.mapped . Lens.mapped %~ valIToUUID
        (presentationMode, mName, globalId) <$ def' & Codec.encodeDef & tell
    & withVisited visitedDefs globalId
    where
        globalId = ExprIRef.globalId defI

exportRepl :: Export ()
exportRepl =
    do
        repl <-
            DbLayout.repl DbLayout.codeIRefs & Transaction.readIRef
            >>= ExprIRef.readVal & trans
        exportVal repl
        repl <&> valIToUUID & Codec.encodeRepl & tell

jsonExportRepl :: T ViewM Codec.Encoded
jsonExportRepl = runExport exportRepl <&> snd

fileExportRepl :: FilePath -> T ViewM (IO ())
fileExportRepl = export "repl" exportRepl

fileExportDef :: ExprIRef.DefI ViewM -> FilePath -> T ViewM (IO ())
fileExportDef defI =
    export ("def: " ++ show defI) (exportDef defI)

fileExportAll :: FilePath -> T ViewM (IO ())
fileExportAll =
    do
        exportSet DbLayout.globals exportDef
        exportSet DbLayout.tags exportTag
        exportSet DbLayout.tids exportNominal
        exportRepl
    & export "all"
    where
        exportSet indexIRef exportFunc =
            indexIRef DbLayout.codeIRefs & Transaction.readIRef & trans
            >>= traverse_ exportFunc

export :: String -> Export a -> FilePath -> T ViewM (IO ())
export msg act exportPath =
    runExport act
    <&> snd
    <&> \json ->
        do
            putStrLn $ "Exporting " ++ msg ++ " to " ++ show exportPath
            LBS.writeFile exportPath (AesonPretty.encodePretty json)

setName :: ToUUID a => a -> String -> T ViewM ()
setName x = Transaction.setP (Anchors.assocNameRef x)

writeValAt :: Monad m => Val (ValI m) -> T m (ValI m)
writeValAt (Val valI body) =
    do
        traverse writeValAt body >>= ExprIRef.writeValBody valI
        return valI

writeValAtUUID :: Monad m => Val UUID -> T m (ValI m)
writeValAtUUID val = val <&> IRef.unsafeFromUUID <&> ExprIRef.ValI & writeValAt

insertTo ::
    (Monad m, Ord a, Binary a) =>
    a -> (DbLayout.Code (IRef ViewM) ViewM -> IRef m (Set a)) -> T m ()
insertTo item setIRef =
    Transaction.readIRef iref
    <&> Set.insert item
    >>= Transaction.writeIRef iref
    where
        iref = setIRef DbLayout.codeIRefs

importDef :: Definition (Val UUID) (Anchors.PresentationMode, Maybe String, V.Var) -> T ViewM ()
importDef (Definition defBody (presentationMode, mName, globalId)) =
    do
        Transaction.setP (Anchors.assocPresentationMode globalId) presentationMode
        traverse_ (setName globalId) mName
        Lens.traverse writeValAtUUID defBody
            >>= Transaction.writeIRef defI
        defI `insertTo` DbLayout.globals
    where
        defI = ExprIRef.defI globalId

importRepl :: Val UUID -> T ViewM ()
importRepl val =
    writeValAtUUID val >>= Transaction.writeIRef (DbLayout.repl DbLayout.codeIRefs)

importTag :: (Int, Maybe String, T.Tag) -> T ViewM ()
importTag (tagOrder, mName, tag) =
    do
        Transaction.setP (Anchors.assocTagOrder tag) tagOrder
        traverse_ (setName tag) mName
        tag `insertTo` DbLayout.tags

importLamVar :: (Maybe Anchors.ParamList, Maybe String, UUID, V.Var) -> T ViewM ()
importLamVar (paramList, mName, lamUUID, var) =
    do
        Transaction.setP (Anchors.assocFieldParamList lamI) paramList
        traverse_ (setName var) mName
    where
        lamI = IRef.unsafeFromUUID lamUUID & ExprIRef.ValI

importNominal :: ((Maybe String, T.NominalId), Nominal) -> T ViewM ()
importNominal ((mName, nomId), nominal) =
    do
        traverse_ (setName nomId) mName
        Transaction.writeIRef (ExprIRef.nominalI nomId) nominal
        nomId `insertTo` DbLayout.tids

-- Like asum/msum but collects the errors
firstSuccess :: [Either err a] -> Either [err] a
firstSuccess = swapEither . sequence . fmap swapEither

importOne :: Codec.Encoded -> T ViewM ()
importOne json =
    firstSuccess
    [ try Codec.decodeDef importDef
    , try Codec.decodeRepl importRepl
    , try Codec.decodeNamedTag importTag
    , try Codec.decodeNamedNominal importNominal
    , try Codec.decodeNamedLamVar importLamVar
    ]
    & either parseFail return
    >>= id
    where
        parseFail errors =
            "Failed to parse: " ++ BSL8.unpack (AesonPretty.encodePretty json) ++
            "\n" ++ unlines errors
            & fail
        try decode imp = Codec.runDecoder decode json <&> imp

fileImportAll :: FilePath -> IO (T ViewM ())
fileImportAll importPath =
    do
        putStrLn $ "importing from: " ++ show importPath
        LBS.readFile importPath <&> Aeson.eitherDecode
            >>= either fail return
            <&> \json ->
                do
                    Codec.runDecoder Codec.decodeArray json
                    & either fail return
                    >>= traverse_ importOne
