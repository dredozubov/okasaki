module UnbalancedSet.Common where


class Ord e => UnbalancedSet t e where
  uEmpty :: t e
  uMember :: e -> t e -> Bool
  uInsert :: e -> t e -> t e
