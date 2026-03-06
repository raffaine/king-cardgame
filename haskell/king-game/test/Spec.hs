import Test.Hspec
import KingTypes
import GameRules

import ServerLogic
import KingClient

main :: IO ()
main = hspec $ do
    ----------------------------------------------------------------------
    -- 1. GAME RULES & CONSTRAINTS
    ----------------------------------------------------------------------
    describe "GameRules.isValidPlay" $ do
        -- Base Game Constraints
        describe "Base Game Constraints (Following Suit)" $ do
            it "allows playing anything on an empty table" $ do
                -- assert BaseGame().hand_constraint([], ['2C', '3H'], '3H')
                isValidPlay RVaza [] [parseCard "2C", parseCard "3H"] (parseCard "3H") `shouldBe` True

            it "forces player to follow the table's led suit" $ do
                -- assert BaseGame().hand_constraint(['2C'], ['2H', '3C'], '3C')
                isValidPlay RVaza [parseCard "2C"] [parseCard "2H", parseCard "3C"] (parseCard "3C") `shouldBe` True
                
            it "prevents playing a different suit if the player has the led suit" $ do
                -- assert not BaseGame().hand_constraint(['2C'], ['2H', '3C'], '2H')
                isValidPlay RVaza [parseCard "2C"] [parseCard "2H", parseCard "3C"] (parseCard "2H") `shouldBe` False

            it "allows discarding if the player is void in the led suit" $ do
                -- assert BaseGame().hand_constraint(['AD', 'JD', 'QH'], ['2C', '3C'], '2C')
                isValidPlay RVaza [parseCard "AD", parseCard "JD", parseCard "QH"] [parseCard "2C", parseCard "3C"] (parseCard "2C") `shouldBe` True
        
        -- Rule-Specific Constraints
        -- Copas (No Hearts)
        describe "Copas (No Hearts) Constraints" $ do
            it "allows starting with a non-heart" $ do
                -- assert Copas().hand_constraint([], ['2C', '3H'], '2C')
                isValidPlay RCopas [] [parseCard "2C", parseCard "3H"] (parseCard "2C") `shouldBe` True

            it "prevents starting with a heart if other suits are available" $ do
                -- assert not Copas().hand_constraint([], ['2C', '3H'], '3H')
                isValidPlay RCopas [] [parseCard "2C", parseCard "3H"] (parseCard "3H") `shouldBe` False
        -- King (No King of Hearts)
        describe "King (No King of Hearts) Constraints" $ do
            it "forces the player to discard the King of Hearts when void in led suit" $ do
                -- assert King().hand_constraint(['2C'], ['2H', 'KH', '3D'], 'KH')
                isValidPlay RKing [parseCard "2C"] [parseCard "2H", parseCard "KH", parseCard "3D"] (parseCard "KH") `shouldBe` True
                isValidPlay RKing [parseCard "2C"] [parseCard "2H", parseCard "KH", parseCard "3D"] (parseCard "2H") `shouldBe` False

        -- Other rules have no additional constraints beyond the base game
        describe "Constraints for Vaza, Mulheres, Homens, 2Ultimas, and Positiva" $ do
            it "rely entirely on the general trick-taking constraints" $ do
                -- Mulheres: Must follow suit
                isValidPlay RMulheres [parseCard "2C"] [parseCard "2H", parseCard "3C"] (parseCard "3C") `shouldBe` True
                -- Homens: Cannot break suit if holding it
                isValidPlay RHomens [parseCard "2C"] [parseCard "2H", parseCard "3C"] (parseCard "2H") `shouldBe` False
                -- 2Ultimas: Can discard if void
                isValidPlay R2Ultimas [parseCard "2C"] [parseCard "2H", parseCard "3H"] (parseCard "3H") `shouldBe` True
                -- Positiva: No special play constraints, just follows suit
                isValidPlay RPositiva [parseCard "2C"] [parseCard "2H", parseCard "3C"] (parseCard "3C") `shouldBe` True
                isValidPlay RPositivaH [parseCard "2C"] [parseCard "2H", parseCard "3C"] (parseCard "3C") `shouldBe` True
    ----------------------------------------------------------------------
    -- 2. TRICK EVALUATION & SCORING
    ----------------------------------------------------------------------
    describe "GameRules.evaluateTrick" $ do
        -- Base Game Logic
        describe "Trick Winner Logic (Base Game)" $ do
            it "awards the trick to the highest card of the led suit" $ do
                -- assert Vaza().play_round(['2H', '5H', 'AS', '4H']) == (1, -25) 
                -- Note: Python test says -25, but class Vaza says score = -20. We will use -20.
                evaluateTrick RVaza 1 [parseCard "2H", parseCard "5H", parseCard "AS", parseCard "4H"] `shouldBe` (1, -20)
            
            it "ignores higher cards of non-led suits (no trumps)" $ do
                -- assert Vaza().play_round(['TD', '5C', 'AS', '4H']) == (0, -25)
                evaluateTrick RVaza 1 [parseCard "TD", parseCard "5C", parseCard "AS", parseCard "4H"] `shouldBe` (0, -20)

        describe "Scoring Rules" $ do
            it "Mulheres (No Queens) scores -50 for each Queen" $ do
                -- assert Mulheres().play_round(['5H', 'QS', 'JS', '2H']) == (0, -50)
                evaluateTrick RMulheres 1 [parseCard "5H", parseCard "QS", parseCard "JS", parseCard "2H"] `shouldBe` (0, -50)
                -- assert Mulheres().play_round(['QD', 'QS', 'QC', 'QH']) == (0, -200)
                evaluateTrick RMulheres 1 [parseCard "QD", parseCard "QS", parseCard "QC", parseCard "QH"] `shouldBe` (0, -200)

            it "Homens (No Kings or Jacks) scores -30 for each King or Jack" $ do
                -- assert Homens().play_round(['TC', 'KS', 'JC', '5H']) == (2, -60)
                evaluateTrick RHomens 1 [parseCard "TC", parseCard "KS", parseCard "JC", parseCard "5H"] `shouldBe` (2, -60)

            it "Copas (No Hearts) scores -20 for each Heart" $ do
                -- assert Copas().play_round(['5H', 'TH', 'AD', '2H']) == (1, -60)
                evaluateTrick RCopas 1 [parseCard "5H", parseCard "TH", parseCard "AD", parseCard "2H"] `shouldBe` (1, -60)

            it "King (No King of Hearts) scores -160 if the King of Hearts is present" $ do
                -- assert King().play_round(['AH', '6C', 'KH', 'TH']) == (0, -160)
                evaluateTrick RKing 1 [parseCard "AH", parseCard "6C", parseCard "KH", parseCard "TH"] `shouldBe` (0, -160)
                evaluateTrick RKing 1 [parseCard "5H", parseCard "AS", parseCard "AC", parseCard "TH"] `shouldBe` (3, 0)

            it "DuasUltimas scores 0 for rounds 1-11, and -90 for rounds 12 and 13" $ do
                -- Python checks for round > 10 (0-indexed) so rounds 11 and 12. 
                -- In 1-indexed, that's rounds 12 and 13.
                evaluateTrick R2Ultimas 10 [parseCard "5H", parseCard "KS", parseCard "AC", parseCard "TH"] `shouldBe` (3, 0)
                evaluateTrick R2Ultimas 12 [parseCard "5H", parseCard "QS", parseCard "JS", parseCard "2H"] `shouldBe` (0, -90)
                evaluateTrick R2Ultimas 13 [parseCard "5D", parseCard "QD", parseCard "JS", parseCard "2H"] `shouldBe` (1, -90)

        describe "Positiva & Trumps" $ do
            it "scores +25 per trick and awards the trick to the highest trump if played" $ do
                -- assert Positiva('H').play_round(['5D', '6H', 'AD', 'TD']) == (1, 25)
                evaluateTrick RPositivaH 1 [parseCard "5D", parseCard "6H", parseCard "AD", parseCard "TD"] `shouldBe` (1, 25)
                
            it "awards the trick to the highest led suit if no trump is played" $ do
                -- assert Positiva('H').play_round(['5C', '2S', 'AC', 'TC']) == (2, 25)
                evaluateTrick RPositivaH 1 [parseCard "5C", parseCard "2S", parseCard "AC", parseCard "TC"] `shouldBe` (2, 25)
    ----------------------------------------------------------------------
    -- 3. CLIENT SERIALIZATION
    ----------------------------------------------------------------------
    describe "KingClient Serialization" $ do
        it "correctly parses wire strings into KingRules" $ do
            readRule "VAZA" `shouldBe` RVaza
            readRule "2ULTIMAS" `shouldBe` R2Ultimas
            readRule "POSITIVAH" `shouldBe` RPositivaH
      
        it "safely falls back to RPositiva for unknown rules" $ do
            readRule "INVALID_JUNK" `shouldBe` RPositiva
      
        it "correctly formats KingRules back into wire strings" $ do
            show RVaza `shouldBe` "VAZA"
            show RKing `shouldBe` "KING"
    ----------------------------------------------------------------------
    -- 4. CLIENT STATE TRANSITIONS
    ----------------------------------------------------------------------
    describe "KingClient State Transitions" $ do
        let dummyPlayer = Player "Alice" "channel-123"
            initGame = mkGame dummyPlayer "Table1" "secret-456"

        it "initializes the game state correctly" $ do
            user (player initGame) `shouldBe` "Alice"
            name (kingTable initGame) `shouldBe` "Table1"

        it "adds players to the table" $ do
            let gameWithPlayers = setPlayers initGame ["Alice", "Bob", "Charlie", "Dave"]
            players (kingTable gameWithPlayers) `shouldBe` ["Alice", "Bob", "Charlie", "Dave"]

        it "starts a hand and sets the active turn" $ do
            let gameWithPlayers = setPlayers initGame ["Alice", "Bob", "Charlie", "Dave"]
                -- "Bob" is the starter. Bob is at index 1.
                gameHandStarted = startHand gameWithPlayers "Bob" [RVaza, RCopas]
        
            activeTurn gameHandStarted `shouldBe` 1
            length (gameHands gameHandStarted) `shouldBe` 1

        it "removes a card from the player's hand when they play it" $ do
            let gameWithPlayers = setPlayers initGame ["Alice", "Bob", "Charlie", "Dave"]
                gameHandStarted = startHand gameWithPlayers "Alice" [RVaza]
                gameWithCards   = setupCards gameHandStarted ["2H", "3C", "AH"]
                
                -- Alice plays the 3 of Clubs
                gameAfterPlay   = playCard gameWithCards "3C"
                
            -- The card should be removed from her hand
            roundCards gameAfterPlay `shouldBe` ["2H", "AH"]
            
            -- The card should be added to the current round's table
            let currentTable = curRound (head (gameHands gameAfterPlay))
            currentTable `shouldBe` ["3C"]

    describe "KingClient: Identity and Turn Management" $ do
        let pAlice = Player "Alice" "channel-123"
            initGame = mkGame pAlice "Table1" "secret-456"
            gameWPlayers = setPlayers initGame ["Alice", "Bob", "Charlie", "Dave"]

        it "moveTurn accurately resolves player names to their index" $ do
            let turnBob = moveTurn gameWPlayers "Bob"
            activeTurn turnBob `shouldBe` 1
        
            let turnDave = moveTurn turnBob "Dave"
            activeTurn turnDave `shouldBe` 3

        it "playCard removes the card from the local hand ONLY if it's the local player's turn" $ do
            -- The server says Bob is starting the hand
            let started = startHand gameWPlayers "Bob" [RVaza] 
                -- Alice gets her cards
                withCards = setupCards started ["2H", "3C", "AH"]
            
            -- The server says Bob played "3C". 
            -- (Even if Alice happens to have that card, her hand shouldn't lose it because she isn't Bob).
            let afterBobPlays = playCard withCards "3C"
            
            -- Alice's hand is untouched
            roundCards afterBobPlays `shouldBe` ["2H", "3C", "AH"]
            -- But the table should show Bob's card
            curRound (head (gameHands afterBobPlays)) `shouldBe` ["3C"]

            -- Now the server says it is Alice's turn
            let aliceTurn = moveTurn afterBobPlays "Alice"
                afterAlicePlays = playCard aliceTurn "AH"

            -- Now Alice's hand SHOULD lose the card
            roundCards afterAlicePlays `shouldBe` ["2H", "3C"]
            -- And the table should have both
            curRound (head (gameHands afterAlicePlays)) `shouldBe` ["3C", "AH"]


    describe "KingClient: Hand Lifecycle" $ do
        let pAlice = Player "Alice" "ch1"
            gameWPlayers = setPlayers (mkGame pAlice "T1" "s") ["Alice", "Bob", "Charlie", "Dave"]

        it "startHand pushes a new hand with pending rules (Left) and sets the starter" $ do
            let started = startHand gameWPlayers "Charlie" [RVaza, RKing]
            
            activeTurn started `shouldBe` 2
            length (gameHands started) `shouldBe` 1
            handRule (head (gameHands started)) `shouldBe` Left [RVaza, RKing]

        it "setHandRule commits the pending rules to a chosen rule (Right)" $ do
            let started = startHand gameWPlayers "Charlie" [RVaza, RKing]
                committed = setHandRule started RKing
                
            handRule (head (gameHands committed)) `shouldBe` Right RKing


    describe "KingClient: Score Accumulation (endRound)" $ do
        let pAlice = Player "Alice" "ch1"
            gameWPlayers = setPlayers (mkGame pAlice "T1" "s") ["Alice", "Bob", "Charlie", "Dave"]
            started = startHand gameWPlayers "Alice" [RVaza]
        
            -- Simulate a table with 4 cards played
            tableFull = started { gameHands = [ (head (gameHands started)) { curRound = ["2H", "3H", "4H", "5H"] } ] }

        it "clears the table and attributes the score to the winning player's index" $ do
            -- The server declares Charlie (index 2) won the trick and took -20 points
            let postRound = endRound tableFull "Charlie" (-20)
                currentHand = head (gameHands postRound)
            
            -- The table cards should be swept away
            curRound currentHand `shouldBe` []
            
            -- The score should be appended to the score matrix. 
            -- replaceNth 2 (-20) [0,0,0,0] -> [0, 0, -20, 0]
            handScore currentHand `shouldBe` [[0, 0, -20, 0]]

        it "stacks multiple rounds of scoring correctly" $ do
            let r1 = endRound tableFull "Charlie" (-20)
                -- Simulate more cards being played and round 2 ending
                r2 = endRound r1 "Bob" (-50)
                currentHand = head (gameHands r2)
            
            -- We should have two score arrays in the history now
            handScore currentHand `shouldBe` [[0, 0, -20, 0], [0, -50, 0, 0]]

            -- Add this import at the top of Spec.hs

    ----------------------------------------------------------------------
    -- 5. PURE SERVER LOGIC
    ----------------------------------------------------------------------
    describe "ServerLogic: Identity and Table Management" $ do
        let emptyCtx = emptyServerContext

        describe "AUTHORIZE command" $ do
            it "authorizes a new user and returns a generated channel ID" $ do
                -- We inject "chan-123" to act as our predictable UUID
                let (reply, bcast, ctx') = handleCommand emptyCtx "chan-123" ["AUTHORIZE", "Alice", "password123"]
                
                reply `shouldBe` "chan-123"
                bcast `shouldBe` Nothing
                hasUser ctx' "Alice" `shouldBe` True

        describe "TABLE command (Create Table)" $ do
            let (_, _, ctxAlice) = handleCommand emptyCtx "chan-123" ["AUTHORIZE", "Alice", "pass"]

            it "creates a new table and returns the table ID if the user is authorized" $ do
                let (reply, bcast, ctxTable) = handleCommand ctxAlice "table-456" ["TABLE", "Alice", "chan-123"]
                
                reply `shouldBe` "table-456"
                bcast `shouldBe` Nothing
                hasTable ctxTable "table-456" `shouldBe` True

            it "returns an ERROR if the user provides the wrong channel ID" $ do
                let (reply, bcast, ctxTable) = handleCommand ctxAlice "table-456" ["TABLE", "Alice", "wrong-channel"]
                
                reply `shouldBe` "ERROR User not authorized"
                hasTable ctxTable "table-456" `shouldBe` False

        describe "JOIN command and Game Start Broadcast" $ do
            let ctx0 = emptyCtx                
                -- 1. Alice Auths, Creates Table, AND Joins (Python's create_table doesn't auto-join)
                (_, _, ctxA)      = handleCommand ctx0 "chan-123" ["AUTHORIZE", "Alice", "pass"]
                (_, _, ctxT1)     = handleCommand ctxA "table-456" ["TABLE", "Alice", "chan-123"]
                (_, _, ctxA_Join) = handleCommand ctxT1 "sec-A" ["JOIN", "Alice", "chan-123", "table-456"]

                -- 2. Bob Auths & Joins
                (_, _, ctxB)      = handleCommand ctxA_Join "chan-bob" ["AUTHORIZE", "Bob", "pass"]
                (_, _, ctxB_Join) = handleCommand ctxB "sec-B" ["JOIN", "Bob", "chan-bob", "table-456"]

                -- 3. Cat Auths & Joins
                (_, _, ctxC)      = handleCommand ctxB_Join "chan-cat" ["AUTHORIZE", "Cat", "pass"]
                (_, _, ctxC_Join) = handleCommand ctxC "sec-C" ["JOIN", "Cat", "chan-cat", "table-456"]

                -- 4. Dave Auths
                (_, _, ctxD)      = handleCommand ctxC_Join "chan-dave" ["AUTHORIZE", "Dave", "pass"]

            it "allows a second authorized user to join an existing table and returns a secret without broadcasting" $ do
                -- We test Bob joining against Alice's context
                let (reply, bcast, _) = handleCommand ctxB "sec-B" ["JOIN", "Bob", "chan-bob", "table-456"]
                
                reply `shouldBe` "sec-B"
                bcast `shouldBe` Nothing

            it "allows a third authorized user to join an existing table and returns a secret without broadcasting" $ do
                -- We test Cat joining against Alice's context
                let (reply, bcast, _) = handleCommand ctxC "sec-C" ["JOIN", "Cat", "chan-cat", "table-456"]
                
                reply `shouldBe` "sec-C"
                bcast `shouldBe` Nothing
                
            it "returns an ERROR if the table does not exist" $ do
                let (reply, _, _) = handleCommand ctxB "sec-B" ["JOIN", "Bob", "chan-bob", "ghost-table"]
                reply `shouldBe` "ERROR Table does not exist"

            it "allows a fourth user to join, returns a secret, and BROADCASTS the game start" $ do
                -- Dave joins a table that already has Alice, Bob, and Cat
                let (reply, bcast, _) = handleCommand ctxD "sec-D" ["JOIN", "Dave", "chan-dave", "table-456"]
                
                reply `shouldBe` "sec-D"
                -- The broadcast matches Python's '%s START %s' format
                bcast `shouldBe` Just "table-456 START Alice Bob Cat Dave"
            
            it "returns an ERROR if a fifth user tries to join a full table" $ do
                -- First, we officially add Dave to the timeline to fill the table
                let (_, _, ctxD_Join) = handleCommand ctxD "sec-D" ["JOIN", "Dave", "chan-dave", "table-456"]
                    -- Now Eve tries to crash the party
                    (_, _, ctxE)      = handleCommand ctxD_Join "chan-eve" ["AUTHORIZE", "Eve", "pass"]
                    (reply, _, _)     = handleCommand ctxE "sec-E" ["JOIN", "Eve", "chan-eve", "table-456"]
                
                reply `shouldBe` "ERROR Table is already full"
