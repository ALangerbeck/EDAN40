indentString :: (Eq t, Num t) => t -> [Char]
indentString n =  case n of
        0 -> ""
        n -> "  " ++ indentString (n-1)