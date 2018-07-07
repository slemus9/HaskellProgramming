data RecordProduct a b =
  RecordProduct {pfirst :: a, psecond :: b} deriving (Eq, Show)

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct {pfirst = 42, psecond = 0.00001}

data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving(Eq, Show)

data ProgrammingLanguage =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer {os :: OperatingSystem, lang :: ProgrammingLanguage} deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer {os = Mac, lang = Haskell}

feelingWizrardly :: Programmer
feelingWizrardly = Programmer {lang = Agda, os = GnuPlusLinux}


------------------------------------------------------
-- Exercise

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer { os = o, lang = l } | o <- allOperatingSystems, l <- allLanguages ]

-- map (\(x, y) -> Programmer {os = x, lang = y}) $ [(o, l) | o <- allOperatingSystems, l <- allLanguages]
------------------------------------------------------
