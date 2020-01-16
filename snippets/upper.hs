(>) :: Tarp -> Tarp -> Maybe Bool
(>) T1 T2 = if isNothing $ T1 overlaps T2
            then Nothing
            else Just $ T1 isAbove T2
