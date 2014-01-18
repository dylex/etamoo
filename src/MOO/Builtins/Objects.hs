
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Objects ( builtins ) where

import Control.Concurrent.STM (STM, newTVar, readTVar, writeTVar)
import Control.Monad (when, unless, liftM, void, join)
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)

import qualified Data.HashMap.Strict as HM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import MOO.Builtins.Common
import MOO.Database
import MOO.Types
import MOO.Task
import MOO.Object
import MOO.Verb
import MOO.Network
import MOO.Unparser
import MOO.Parser
import {-# SOURCE #-} MOO.Compiler
import MOO.AST

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- | § 4.4.3 Manipulating Objects
builtins :: [BuiltinSpec]
builtins = [
    ("create"        , (bf_create        , Info 1 (Just 2) [TObj, TObj] TObj))
  , ("chparent"      , (bf_chparent      , Info 2 (Just 2) [TObj, TObj] TAny))
  , ("valid"         , (bf_valid         , Info 1 (Just 1) [TObj]       TInt))
  , ("parent"        , (bf_parent        , Info 1 (Just 1) [TObj]       TObj))
  , ("children"      , (bf_children      , Info 1 (Just 1) [TObj]       TLst))
  , ("recycle"       , (bf_recycle       , Info 1 (Just 1) [TObj]       TAny))
  , ("object_bytes"  , (bf_object_bytes  , Info 1 (Just 1) [TObj]       TInt))
  , ("max_object"    , (bf_max_object    , Info 0 (Just 0) []           TObj))

  , ("move"          , (bf_move          , Info 2 (Just 2) [TObj, TObj] TAny))
  , ("properties"    , (bf_properties    , Info 1 (Just 1) [TObj]       TLst))
  , ("property_info" , (bf_property_info , Info 2 (Just 2) [TObj, TStr] TLst))
  , ("set_property_info",
                    (bf_set_property_info, Info 3 (Just 3) [TObj, TStr,
                                                            TLst]       TAny))
  , ("add_property"  , (bf_add_property  , Info 4 (Just 4) [TObj, TStr,
                                                            TAny, TLst] TAny))
  , ("delete_property",
                      (bf_delete_property, Info 2 (Just 2) [TObj, TStr] TAny))
  , ("is_clear_property",
                    (bf_is_clear_property, Info 2 (Just 2) [TObj, TStr] TInt))
  , ("clear_property", (bf_clear_property, Info 2 (Just 2) [TObj, TStr] TAny))

  , ("verbs"         , (bf_verbs         , Info 1 (Just 1) [TObj]       TLst))
  , ("verb_info"     , (bf_verb_info     , Info 2 (Just 2) [TObj, TAny] TLst))
  , ("set_verb_info" , (bf_set_verb_info , Info 3 (Just 3) [TObj, TAny,
                                                            TLst]       TAny))
  , ("verb_args"     , (bf_verb_args     , Info 2 (Just 2) [TObj, TAny] TLst))
  , ("set_verb_args" , (bf_set_verb_args , Info 3 (Just 3) [TObj, TAny,
                                                            TLst]       TAny))
  , ("add_verb"      , (bf_add_verb      , Info 3 (Just 3) [TObj, TLst,
                                                            TLst]       TInt))
  , ("delete_verb"   , (bf_delete_verb   , Info 2 (Just 2) [TObj, TAny] TAny))
  , ("verb_code"     , (bf_verb_code     , Info 2 (Just 4) [TObj, TAny,
                                                            TAny, TAny] TLst))
  , ("set_verb_code" , (bf_set_verb_code , Info 3 (Just 3) [TObj, TAny,
                                                            TLst]       TLst))
  , ("disassemble"   , (bf_disassemble   , Info 2 (Just 2) [TObj, TAny] TLst))

  , ("players"       , (bf_players       , Info 0 (Just 0) []           TLst))
  , ("is_player"     , (bf_is_player     , Info 1 (Just 1) [TObj]       TInt))
  , ("set_player_flag",
                      (bf_set_player_flag, Info 2 (Just 2) [TObj, TAny] TAny))
  ]

-- § 4.4.3.1 Fundamental Operations on Objects

bf_create (Obj parent : optional) = do
  maybeParent <- case parent of
    -1  -> return Nothing
    oid -> checkFertile oid >> return (Just oid)

  db <- getDatabase
  let newOid = maxObject db + 1

  ownerOid <- case maybeOwner of
    Nothing         -> frame permissions
    Just (Obj (-1)) -> checkWizard         >> return newOid
    Just (Obj oid)  -> checkPermission oid >> return oid

  maybeQuota <- readProperty ownerOid "ownership_quota"
  case maybeQuota of
    Just (Int quota)
      | quota <= 0 -> raise E_QUOTA
      | otherwise  -> writeProperty ownerOid "ownership_quota" (Int $ quota - 1)
    _ -> return ()

  properties <- case maybeParent of
    Nothing  -> return $ objectProperties initObject
    Just oid -> do
      -- add to parent's set of children
      liftSTM $ modifyObject oid db $ \obj -> return $ addChild obj newOid

      -- properties inherited from parent
      Just parent <- getObject oid
      HM.fromList `liftM` mapM mkProperty (HM.toList $ objectProperties parent)

        where mkProperty (name, propTVar) = liftSTM $ do
                prop <- readTVar propTVar
                let prop' = prop {
                        propertyValue     = Nothing
                      , propertyInherited = True
                      , propertyOwner     = if propertyPermC prop
                                            then ownerOid
                                            else propertyOwner prop
                      }
                propTVar' <- newTVar prop'
                return (name, propTVar')

  let newObj = initObject {
          objectParent     = maybeParent
        , objectOwner      = ownerOid
        , objectProperties = properties
        }

  putDatabase =<< liftSTM (addObject newObj db)

  callFromFunc "create" 0 (newOid, "initialize") []
  return (Obj newOid)

  where (maybeOwner : _) = maybeDefaults optional

bf_chparent [Obj object, Obj new_parent] = notyet "chparent"

bf_valid [Obj object] = (truthValue . isJust) `liftM` getObject object

bf_parent [Obj object] = (Obj . getParent) `liftM` checkValid object

bf_children [Obj object] = (objectList . getChildren) `liftM` checkValid object

bf_recycle [Obj object] = notyet "recycle"

bf_object_bytes [Obj object] = do
  checkWizard
  obj <- checkValid object

  propertyBytes <- liftM storageBytes $ liftSTM $
                   mapM readTVar $ HM.elems (objectProperties obj)
  verbBytes     <- liftM storageBytes $ liftSTM $
                   mapM (readTVar . snd) $ objectVerbs obj

  return $ Int $ fromIntegral $ storageBytes obj + propertyBytes + verbBytes

bf_max_object [] = (Obj . maxObject) `liftM` getDatabase

-- § 4.4.3.2 Object Movement

bf_move [Obj what, Obj where_] = do
  what' <- checkValid what
  where' <- case where_ of
    -1  -> return Nothing
    oid -> Just `liftM` checkValid oid
  checkPermission (objectOwner what')

  when (isJust where') $ do
    accepted <- maybe False truthOf `liftM`
                callFromFunc "move" 0 (where_, "accept") [Obj what]
    unless accepted $ do
      wizard <- isWizard =<< frame permissions
      unless wizard $ raise E_NACC

  let newWhere = case where_ of
        -1  -> Nothing
        oid -> Just oid

  maybeWhat <- getObject what
  case maybeWhat of
    Nothing      -> return ()
    Just whatObj -> unless (objectLocation whatObj == newWhere) $ do
      maybeWhere <- getObject where_
      when (isNothing newWhere || isJust maybeWhere) $ do
        checkRecurse what where_

        let oldWhere = objectLocation whatObj
        db <- getDatabase

        liftSTM $ modifyObject what db $ \obj ->
          return obj { objectLocation = newWhere }
        case oldWhere of
          Nothing        -> return ()
          Just oldWhere' -> liftSTM $ modifyObject oldWhere' db $ \obj ->
            return obj { objectContents = IS.delete what (objectContents obj) }
        case newWhere of
          Nothing        -> return ()
          Just newWhere' -> liftSTM $ modifyObject newWhere' db $ \obj ->
            return obj { objectContents = IS.insert what (objectContents obj) }

        case oldWhere of
          Nothing        -> return ()
          Just oldWhere' ->
            void $ callFromFunc "move" 1 (oldWhere', "exitfunc") [Obj what]

        maybeWhat <- getObject what
        case maybeWhat of
          Nothing      -> return ()
          Just whatObj ->
            when (objectLocation whatObj == newWhere) $
            void $ callFromFunc "move" 2 (where_, "enterfunc") [Obj what]

  return nothing

  where checkRecurse what loc = do
          when (loc == what) $ raise E_RECMOVE
          maybeLoc <- getObject loc
          case join $ objectLocation `fmap` maybeLoc of
            Just oid -> checkRecurse what oid
            Nothing  -> return ()

-- § 4.4.3.3 Operations on Properties

bf_properties [Obj object] = do
  obj <- checkValid object
  unless (objectPermR obj) $ checkPermission (objectOwner obj)

  stringList `liftM` liftSTM (definedProperties obj)

bf_property_info [Obj object, Str prop_name] = do
  obj <- checkValid object
  prop <- getProperty obj prop_name
  unless (propertyPermR prop) $ checkPermission (propertyOwner prop)

  return $ fromList [Obj $ propertyOwner prop, Str $ perms prop]

  where perms prop = T.pack $ concat [['r' | propertyPermR prop],
                                      ['w' | propertyPermW prop],
                                      ['c' | propertyPermC prop]]

traverseDescendants :: (Object -> MOO a) -> ObjId -> MOO ()
traverseDescendants f oid = do
  Just obj <- getObject oid
  f obj
  mapM_ (traverseDescendants f) $ getChildren obj

modifyDescendants :: Database -> (Object -> STM Object) -> ObjId -> MOO ()
modifyDescendants db f oid = do
  liftSTM $ modifyObject oid db f
  Just obj <- getObject oid
  mapM_ (modifyDescendants db f) $ getChildren obj

{-# ANN module ("HLint: ignore Use String" :: String) #-}

checkPerms :: [Char] -> StrT -> MOO (Set Char)
checkPerms valid perms = do
  let permSet = S.fromList (T.unpack $ T.toCaseFold perms)
  unless (S.null $ permSet `S.difference` S.fromList valid) $ raise E_INVARG
  return permSet

bf_set_property_info [Obj object, Str prop_name, Lst info] = do
  (owner, perms, new_name) <- case V.toList info of
    [Obj owner, Str perms]               -> return (owner, perms, Nothing)
    [_        , _        ]               -> raise E_TYPE
    [Obj owner, Str perms, Str new_name] -> return (owner, perms, Just new_name)
    [_        , _        , _           ] -> raise E_TYPE
    _                                    -> raise E_INVARG
  permSet <- checkPerms "rwc" perms
  checkValid owner

  obj <- checkValid object
  prop <- getProperty obj prop_name
  unless (propertyPermW prop) $ checkPermission (propertyOwner prop)
  checkPermission owner

  let setInfo = modifyProperty obj prop_name $ \prop ->
        return prop {
            propertyOwner = owner
          , propertyPermR = 'r' `S.member` permSet
          , propertyPermW = 'w' `S.member` permSet
          , propertyPermC = 'c' `S.member` permSet
        }

  case new_name of
    Nothing      -> setInfo
    Just newName -> do
      let newName' = T.toCaseFold newName
          oldName' = T.toCaseFold prop_name

      unless (objectPermW obj) $ checkPermission (objectOwner obj)

      when (propertyInherited prop) $ raise E_INVARG
      unless (newName' == oldName') $ flip traverseDescendants object $ \obj ->
        when (isJust $ lookupPropertyRef obj newName') $ raise E_INVARG

      setInfo

      db <- getDatabase
      flip (modifyDescendants db) object $ \obj -> do
        let Just propTVar = lookupPropertyRef obj oldName'
        prop <- readTVar propTVar
        writeTVar propTVar $ prop { propertyName = newName }

        return obj { objectProperties =
                        HM.insert newName' propTVar $
                        HM.delete oldName' (objectProperties obj) }

  return nothing

bf_add_property [Obj object, Str prop_name, value, Lst info] = do
  (owner, perms) <- case V.toList info of
    [Obj owner, Str perms] -> return (owner, perms)
    [_        , _        ] -> raise E_TYPE
    _                      -> raise E_INVARG
  permSet <- checkPerms "rwc" perms
  checkValid owner

  obj <- checkValid object
  unless (objectPermW obj) $ checkPermission (objectOwner obj)
  checkPermission owner

  when (isBuiltinProperty name) $ raise E_INVARG
  flip traverseDescendants object $ \obj ->
    when (isJust $ lookupPropertyRef obj name) $ raise E_INVARG

  let definedProp = initProperty {
          propertyName      = prop_name
        , propertyValue     = Just value
        , propertyInherited = False
        , propertyOwner     = owner
        , propertyPermR     = 'r' `S.member` permSet
        , propertyPermW     = 'w' `S.member` permSet
        , propertyPermC     = 'c' `S.member` permSet
      }
      inheritedProp = definedProp {
          propertyInherited = True
        , propertyValue     = Nothing
       }
      addProperty prop obj = do
        propTVar <- newTVar prop
        return obj { objectProperties =
                        HM.insert name propTVar $ objectProperties obj }
      addInheritedProperty prop obj =
        flip addProperty obj $ if propertyPermC prop
                               then prop { propertyOwner = objectOwner obj }
                               else prop
  db <- getDatabase
  liftSTM $ modifyObject object db (addProperty definedProp)
  mapM_ (modifyDescendants db $
         addInheritedProperty inheritedProp) $ getChildren obj

  return nothing

  where name = T.toCaseFold prop_name

bf_delete_property [Obj object, Str prop_name] = do
  obj <- checkValid object
  unless (objectPermW obj) $ checkPermission (objectOwner obj)
  prop <- getProperty obj prop_name
  when (propertyInherited prop) $ raise E_PROPNF

  db <- getDatabase
  flip (modifyDescendants db) object $ \obj ->
    return obj { objectProperties = HM.delete name (objectProperties obj) }

  return nothing

  where name = T.toCaseFold prop_name

bf_is_clear_property [Obj object, Str prop_name] = do
  obj <- checkValid object
  if isBuiltinProperty prop_name
    then return $ truthValue False
    else do
      prop <- getProperty obj prop_name
      unless (propertyPermR prop) $ checkPermission (propertyOwner prop)

      return (truthValue $ isNothing $ propertyValue prop)

bf_clear_property [Obj object, Str prop_name] = do
  obj <- checkValid object
  if isBuiltinProperty prop_name
    then raise E_PERM
    else do
      modifyProperty obj prop_name $ \prop -> do
        unless (propertyPermW prop) $ checkPermission (propertyOwner prop)
        unless (propertyInherited prop) $ raise E_INVARG
        return prop { propertyValue = Nothing }

      return nothing

-- § 4.4.3.4 Operations on Verbs

bf_verbs [Obj object] = do
  obj <- checkValid object
  unless (objectPermR obj) $ checkPermission (objectOwner obj)

  stringList `liftM` liftSTM (definedVerbs obj)

bf_verb_info [Obj object, verb_desc] = do
  obj <- checkValid object
  verb <- getVerb obj verb_desc
  unless (verbPermR verb) $ checkPermission (verbOwner verb)

  return $ fromList
    [Obj $ verbOwner verb, Str $ perms verb, Str $ verbNames verb]

  where perms verb = T.pack $ concat [['r' | verbPermR verb],
                                      ['w' | verbPermW verb],
                                      ['x' | verbPermX verb],
                                      ['d' | verbPermD verb]]

verbInfo :: LstT -> MOO (ObjId, Set Char, StrT)
verbInfo info = do
  (owner, perms, names) <- case V.toList info of
    [Obj owner, Str perms, Str names] -> return (owner, perms, names)
    [_        , _        , _        ] -> raise E_TYPE
    _                                 -> raise E_INVARG
  permSet <- checkPerms "rwxd" perms
  checkValid owner
  when (null $ T.words names) $ raise E_INVARG

  return (owner, permSet, names)

bf_set_verb_info [Obj object, verb_desc, Lst info] = do
  (owner, permSet, names) <- verbInfo info

  obj <- checkValid object
  verb <- getVerb obj verb_desc
  unless (verbPermW verb) $ checkPermission (verbOwner verb)
  checkPermission owner

  let newNames = T.toCaseFold names
      oldNames = T.toCaseFold (verbNames verb)
  unless (newNames == oldNames || objectPermW obj) $
    checkPermission (objectOwner obj)

  modifyVerb (object, obj) verb_desc $ \verb ->
    return verb {
        verbNames = names
      , verbOwner = owner
      , verbPermR = 'r' `S.member` permSet
      , verbPermW = 'w' `S.member` permSet
      , verbPermX = 'x' `S.member` permSet
      , verbPermD = 'd' `S.member` permSet
    }

  return nothing

bf_verb_args [Obj object, verb_desc] = do
  obj <- checkValid object
  verb <- getVerb obj verb_desc
  unless (verbPermR verb) $ checkPermission (verbOwner verb)

  return $ stringList [dobj verb, prep verb, iobj verb]

  where dobj = obj2text  . verbDirectObject
        iobj = obj2text  . verbIndirectObject
        prep = prep2text . verbPreposition

verbArgs :: LstT -> MOO (ObjSpec, PrepSpec, ObjSpec)
verbArgs args = do
  (dobj, prep, iobj) <- case V.toList args of
    [Str dobj, Str prep, Str iobj] -> return (dobj, breakSlash prep, iobj)
      where breakSlash = fst . T.breakOn "/"
    [_       , _       , _       ] -> raise E_TYPE
    _                              -> raise E_INVARG
  dobj' <- maybe (raise E_INVARG) return $ text2obj  (T.toCaseFold dobj)
  prep' <- maybe (raise E_INVARG) return $ text2prep (T.toCaseFold prep)
  iobj' <- maybe (raise E_INVARG) return $ text2obj  (T.toCaseFold iobj)

  return (dobj', prep', iobj')

bf_set_verb_args [Obj object, verb_desc, Lst args] = do
  (dobj, prep, iobj) <- verbArgs args

  obj <- checkValid object
  verb <- getVerb obj verb_desc
  unless (verbPermW verb) $ checkPermission (verbOwner verb)

  modifyVerb (object, obj) verb_desc $ \verb ->
    return verb {
        verbDirectObject   = dobj
      , verbPreposition    = prep
      , verbIndirectObject = iobj
    }

  return nothing

bf_add_verb [Obj object, Lst info, Lst args] = do
  (owner, permSet, names) <- verbInfo info
  (dobj, prep, iobj)      <- verbArgs args

  obj <- checkValid object
  unless (objectPermW obj) $ checkPermission (objectOwner obj)
  checkPermission owner

  let definedVerb = initVerb {
          verbNames          = names
        , verbOwner          = owner
        , verbPermR          = 'r' `S.member` permSet
        , verbPermW          = 'w' `S.member` permSet
        , verbPermX          = 'x' `S.member` permSet
        , verbPermD          = 'd' `S.member` permSet
        , verbDirectObject   = dobj
        , verbPreposition    = prep
        , verbIndirectObject = iobj
      }

  db <- getDatabase
  liftSTM $ modifyObject object db $ addVerb definedVerb

  return $ Int $ fromIntegral $ length (objectVerbs obj) + 1

bf_delete_verb [Obj object, verb_desc] = do
  obj <- checkValid object
  getVerb obj verb_desc
  unless (objectPermW obj) $ checkPermission (objectOwner obj)

  case lookupVerbRef obj verb_desc of
    Nothing         -> raise E_VERBNF
    Just (index, _) -> do
      db <- getDatabase
      liftSTM $ modifyObject object db $ deleteVerb index

  return nothing

bf_verb_code (Obj object : verb_desc : optional) = do
  obj <- checkValid object
  verb <- getVerb obj verb_desc
  unless (verbPermR verb) $ checkPermission (verbOwner verb)
  checkProgrammer

  let code = init $ T.splitOn "\n" $
             unparse fully_paren indent (verbProgram verb)
  return (stringList code)

  where [fully_paren, indent] = booleanDefaults optional [False, True]

bf_set_verb_code [Obj object, verb_desc, Lst code] = do
  obj <- checkValid object
  verb <- getVerb obj verb_desc
  text <- (T.concat . ($ [])) `liftM` V.foldM addLine id code
  unless (verbPermW verb) $ checkPermission (verbOwner verb)
  checkProgrammer

  case parse text of
    Left errors   -> return $ fromListBy (Str . T.pack) errors
    Right program -> do
      modifyVerb (object, obj) verb_desc $ \verb ->
        return verb {
            verbProgram = program
          , verbCode    = compile program
        }
      return $ Lst V.empty

  where addLine :: ([StrT] -> [StrT]) -> Value -> MOO ([StrT] -> [StrT])
        addLine add (Str line) = return (add [line, "\n"] ++)
        addLine _    _         = raise E_INVARG

bf_disassemble [Obj object, verb_desc] = do
  obj <- checkValid object
  verb <- getVerb obj verb_desc
  unless (verbPermR verb) $ checkPermission (verbOwner verb)

  let Program statements = verbProgram verb
  return $ fromListBy (Str . T.pack . show) statements

-- § 4.4.3.5 Operations on Player Objects

bf_players [] = (objectList . allPlayers) `liftM` getDatabase

bf_is_player [Obj object] =
  (truthValue . objectIsPlayer) `liftM` checkValid object

bf_set_player_flag [Obj object, value] = do
  checkValid object
  checkWizard

  db <- getDatabase
  liftSTM $ modifyObject object db $
    \obj -> return obj { objectIsPlayer = isPlayer }
  putDatabase $ setPlayer isPlayer object db

  unless isPlayer $ bootPlayer object

  return nothing

  where isPlayer = truthOf value