-- class HRL (n :: Nat) (as :: [*]) where
--     member        :: Proxy n -> TypeAt n as -> HyperRelation' as -> Bool
--     lookupIndices :: Proxy n -> TypeAt n as -> HyperRelation' as -> HS.HashSet Int

-- instance HRL n '[] where
--   member        Proxy _ EndHR = False
--   lookupIndices Proxy _ EndHR = HS.empty

-- instance (Eq a, Hashable a) => HRL 'Z (a ': as) where
--   member        Proxy a (m :<=>: _) = IM.elem a m
--   lookupIndices Proxy a (m :<=>: _) = IM.lookup a m

-- instance (Eq a, Hashable a, HRL n as) => HRL ('S n) (a ': as) where
--   member        Proxy x (_ :<=>: ms) = member (Proxy :: Proxy n) x ms
--   lookupIndices Proxy x (_ :<=>: ms) = lookupIndices (Proxy :: Proxy n) x ms

-- -- | Example usage: `lookup first rel hyrel` finds all the relations containing
-- --   `rel` in the first position.
-- lookup :: (HRL n as, HRC as, IsRelation a as) => Proxy n -> TypeAt n as -> HyperRelation' as -> [a]
-- lookup proxy x m = map fromRelation . catMaybes . map (\i -> lookupRelation i m) . HS.toList $ (lookupIndices proxy x m)
