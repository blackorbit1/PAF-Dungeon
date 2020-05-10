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

    CS.carteCoordSpec

    ES.environnementAllCoordsPositiveSpec
    ES.environnementOneUncrossableMobPerCaseSpec
    ES.environnementPositiveStatsSpec
    ES.environnementUniqueIdsSpec
    ES.environnementEnviSpec


