{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Types2 (Type (..), Rep (..), Iso (..)) where

import Control.Monad
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import qualified Language.Haskell.TH as TH
import qualified TemplateHaskell.Compat.V0208 as THC

-- A simple representation of type structure

-- This version requires that isomorphic types be instances of
-- the Typeable class.

data Type a where
  TBase :: Typeable a => Type a
  TProd :: Type a -> Type b -> Type (a, b)
  TEither :: Type a -> Type b -> Type (Either a b)
  TIso :: Typeable a => Iso a b -> Type b -> Type a

-- two types are isomorphic when there are functions to
-- take the elements from one to the other. Furthermore
-- we should have both to . from == id  and from . to == id

data Iso a b = Iso
  { to :: a -> b,
    from :: b -> a
  }

-- The class of representable types, must also be Typeable.

class Typeable a => Rep a where
  rep :: Type a

instance Rep Int where rep = TBase

instance Rep Char where rep = TBase

instance Rep () where rep = TBase

instance
  (Rep a, Rep b) =>
  Rep (Either a b)
  where
  rep = TEither rep rep

instance
  (Rep a, Rep b) =>
  Rep (a, b)
  where
  rep = TProd rep rep

-- ---------------------------------------------------------------
-- -- Template Haskell code to automatically generate isomorphisms
-- -- for regular datatypes. Given a data type declaration, such as
-- --    data Maybe a = Nothing | Just a
-- -- the splice  $(represent ''Maybe) automatically includes the
-- -- definitions:
-- --    rMaybe :: forall a. Rep a => Type (Maybe a)
-- --    rMaybe = TIso (Iso ... ...) (TEither TUnit (TProd TUnit a))
-- --
-- --    instance Rep a => Rep (Maybe a) where rep = rMaybe
-- ---------------------------------------------------------------

-- -- Given information about a datatype declaration, compute the
-- -- value that represents
-- -- that type. i.e. for data Bool = True | False,
-- -- returns TEither TUnit TUnit.
-- structureTypeRep :: TypeInfo -> TH.Q TH.Exp
-- structureTypeRep typeInfo = structure
--   where
--     constrs :: [[FieldInfo]]
--     constrs = map constrFields (typeConstrs typeInfo)

--     doconstr :: [FieldInfo] -> TH.Q TH.Exp
--     doconstr [] = [|(rep :: Type ())|]
--     doconstr (t : ts) =
--       [|
--         TProd
--           $(return (repty (fieldType t)))
--           $(doconstr ts)
--         |]

--     structure :: TH.Q TH.Exp
--     structure =
--       foldr1
--         (\t tl -> [|TEither $(t) $(tl)|])
--         (map doconstr constrs)

-- -- the "from" component of the isomorphism
-- rfrom :: [ConstrInfo] -> TH.Q TH.Exp
-- rfrom constrs = do
--   let constrBr (sum, constr) = do
--         vars <- mapM (const (TH.newName "x")) (constrFields constr)
--         let topat =
--               foldr
--                 ( \v tl ->
--                     TH.InfixP
--                       (TH.VarP v)
--                       (TH.mkName "(,)")
--                       tl
--                 )
--                 (THC.conp '() [])
--                 vars
--             tobod =
--               foldl'
--                 (\tl v -> tl `TH.AppE` TH.VarE v)
--                 (TH.ConE (simpleName . constrName $ constr))
--                 vars
--         return (TH.Match (sum topat) (TH.NormalB tobod) [])
--   x <- TH.newName "x"
--   let leftpat p = THC.conp (TH.mkName "Left") [p]
--   let rightpat p = THC.conp (TH.mkName "Right") [p]
--   let pats = nth id constrs
--         where
--           nth g [] = []
--           nth g [x] = [g]
--           nth g (x : xs) = g . leftpat : nth (rightpat . g) xs
--   matchs <- mapM constrBr (zip pats constrs)
--   return (TH.LamE [TH.VarP x] (TH.CaseE (TH.VarE x) matchs))

-- -- the "to" component of the isomorphism
-- rto :: [ConstrInfo] -> TH.Q TH.Exp
-- rto constrs = do
--   x <- TH.newName "x"
--   let constrBr (constr, pats) = do
--         let nm = simpleName . constrName $ constr
--         vars <- mapM (const (TH.newName "x")) (constrFields constr)
--         let outpat = THC.conp nm (map TH.VarP vars)
--         let outbod =
--               pats
--                 ( foldr
--                     (\v tl -> THC.tupE [TH.VarE v, tl])
--                     (TH.ConE '())
--                     vars
--                 )
--         return (TH.Match outpat (TH.NormalB outbod) [])
--   let leftpat p = TH.ConE (TH.mkName "Left") `TH.AppE` p
--   let rightpat p = TH.ConE (TH.mkName "Right") `TH.AppE` p
--   let pats = nth id constrs
--         where
--           nth g [] = []
--           nth g [x] = [g]
--           nth g (x : xs) = g . leftpat : nth (rightpat . g) xs
--   matchs <- mapM constrBr (zip constrs pats)
--   return (TH.LamE [TH.VarP x] (TH.CaseE (TH.VarE x) matchs))

-- -- Create declarations for the representation for a
-- -- given type constructor
-- represent :: TH.Name -> TH.Q [TH.Dec]
-- represent name = do
--   TH.TyConI decl <- TH.reify name
--   let dInfo = typeInfo decl
--       paramNames = map tyVarBndrName (typeParams dInfo)
--       constrs = typeConstrs dInfo

--       -- name of the representation value (i.e. "rBool")
--       rTypeName = rName name

--       -- the type that we are representing, applied to its parameters.
--       ty =
--         foldl'
--           (\x p -> x `TH.AppT` TH.VarT p)
--           (TH.ConT name)
--           paramNames

--       ctx =
--         map
--           ( \p ->
--               THC.classP
--                 (TH.mkName "Rep")
--                 [TH.VarT p]
--           )
--           paramNames

--       -- signature of the representation type
--       rSig =
--         TH.SigD
--           rTypeName
--           ( TH.ForallT
--               (map TH.PlainTV paramNames)
--               ctx
--               ( TH.ConT (TH.mkName "Type")
--                   `TH.AppT` ty
--               )
--           )

--   body <-
--     [|
--       TIso
--         (Iso $(rto constrs) $(rfrom constrs))
--         $(structureTypeRep dInfo)
--       |]

--   let rType :: TH.Dec
--       rType = TH.ValD (TH.VarP rTypeName) (TH.NormalB body) []

--   let inst =
--         THC.instanceD
--           ctx
--           (TH.ConT (TH.mkName "Rep") `TH.AppT` ty)
--           [ TH.ValD
--               (TH.VarP (TH.mkName "rep"))
--               (TH.NormalB (TH.VarE rTypeName))
--               []
--           ]

--   return [rSig, rType, inst]

-- -- Access a representation value for a given type
-- --     i.e. "(rep :: Type Bool)"
-- repty :: TH.Type -> TH.Exp
-- repty ty =
--   TH.SigE
--     (TH.VarE (TH.mkName "rep"))
--     (TH.ConT ''Type `TH.AppT` ty)

-- -- Create the name of a representation value. i.e. "rBool"
-- rName :: TH.Name -> TH.Name
-- rName n =
--   case TH.nameBase n of
--     "(,,,,,,)" -> TH.mkName "rTup7"
--     "(,,,,,)" -> TH.mkName "rTup6"
--     "(,,,,)" -> TH.mkName "rTup5"
--     "(,,,)" -> TH.mkName "rTup4"
--     "(,,)" -> TH.mkName "rTup3"
--     "(,)" -> TH.mkName "rTup2"
--     c -> TH.mkName ("r" ++ c)

-- ---------------------------------------------
-- --- Helper functions
-- --- Puts the information from TH into a format that
-- --- is easier to work with.

-- data TypeInfo = TypeInfo
--   { typeName :: [THC.SpecificityTyVarBndr],
--     typeParams :: TH.Kind,
--     typeConstrs :: [ConstrInfo]
--   }

-- data ConstrInfo = ConstrInfo
--   { constrName :: TH.Name, -- careful, this is NOT
--   -- simplified; may need to
--   -- call simpleName first
--     constrBinders :: [THC.SpecificityTyVarBndr],
--     constrCxt :: TH.Cxt,
--     constrFields :: [FieldInfo],
--     isOnlyConstr :: Bool -- is this the only
--     -- constructor of its type?
--   }

-- mkConstr :: TH.Name -> ConstrInfo
-- mkConstr nm = ConstrInfo nm [] [] [] False

-- data FieldInfo = FieldInfo
--   { fieldName :: Maybe TH.Name,
--     fieldType :: TH.Type
--   }

-- typeInfo :: TH.Dec -> TypeInfo
-- typeInfo d = case d of
--   TH.DataD {} ->
--     TypeInfo (getName d) (paramsA d) (consA d)
--   TH.NewtypeD {} ->
--     TypeInfo (getName d) (paramsA d) (consA d)
--   _ -> error ("derive: not a data type declaration: " ++ show d)
--   where
--     getName (TH.DataD _ _ n _ _ _) = n
--     getName (TH.NewtypeD _ _ n _ _ _) = n
--     getName x =
--       error $ "Impossible! " ++ show x ++ " is neither data nor newtype"

--     paramsA (TH.DataD _ _ _ ps _ _) = fromJust ps
--     paramsA (TH.NewtypeD _ _ _ ps _ _) = fromJust ps

--     consA (TH.DataD _ _ _ _ cs _) = rememberOnly $ map conA cs
--     consA (TH.NewtypeD _ _ _ _ c _) = rememberOnly [conA c]

--     conA (TH.NormalC c xs) =
--       (mkConstr c)
--         { constrFields = map normalField xs
--         }
--     conA (TH.RecC c xs) =
--       (mkConstr c)
--         { constrFields = map recField xs
--         }
--     conA (TH.InfixC t1 c t2) =
--       (mkConstr c)
--         { constrFields = map normalField [t1, t2]
--         }
--     conA (TH.ForallC bdrs cx con) =
--       let c' = conA con
--        in c'
--             { constrBinders = bdrs ++ constrBinders c',
--               constrCxt = cx ++ constrCxt c'
--             }

--     normalField x =
--       FieldInfo
--         { fieldName = Nothing,
--           fieldType = snd x
--         }
--     recField (n, _, t) =
--       FieldInfo
--         { fieldName = Just $ simpleName n,
--           fieldType = t
--         }

-- rememberOnly :: [ConstrInfo] -> [ConstrInfo]
-- rememberOnly [con] = [con {isOnlyConstr = True}]
-- rememberOnly cons = cons

-- simpleName :: TH.Name -> TH.Name
-- simpleName nm =
--   let s = TH.nameBase nm
--    in case dropWhile (/= ':') s of
--         [] -> TH.mkName s
--         [_] -> TH.mkName s
--         _ : t -> TH.mkName t

-- tyVarBndrName :: THC.SpecificityTyVarBndr -> TH.Name
-- tyVarBndrName (TH.PlainTV n) = n
-- tyVarBndrName (TH.KindedTV n _) = n
