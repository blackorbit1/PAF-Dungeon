module Environnement where



data Entite = Vache {iden :: Int , pvie :: Int, clearanceLevel :: Int}
            | Joueur {iden :: Int , pvie :: Int, clearanceLevel :: Int}
    deriving (Eq)
