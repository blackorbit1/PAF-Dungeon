import Test.Hspec
import CarteSpec as CS
import EnvSpec as ES

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = hspec $ do
    -- carte
    CS.carteAllCoordsInBoundsSpec
    CS.carteAllCaseExistsSpec
    CS.carteEntranceExitSpec
    CS.carteSurroundedByWallsSpec
    CS.carteDoorsSurroundedByWallsSpec
    CS.carteCarteSpec

    ES.environnementAllCoordsPositive
    ES.environnementOneUncrossableMobPerCase
    ES.environnementPositiveStats
    ES.environnementUniqueIds


