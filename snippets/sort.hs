sort :: Eq a => [a] -> [a]
sort [] = []
sort ts = sort (>) (ts\\max) ++ max
  where max = [t | t<-ts, all (fromMaybe True . (>) t) $ ts\\[t]]
