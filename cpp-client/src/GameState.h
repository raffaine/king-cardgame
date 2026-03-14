#pragma once
#include <vector>
#include <string>
#include <array>
#include <functional>

#include "Card.h"
#include "Font.h"

// ---------------------------------------------------------
// --- CORE ENUMS ---
// ---------------------------------------------------------

enum class GamePhase {
    NOT_STARTED,        // Initial state, no table is available
    INITIALIZING,       // Spawning bots, waiting for server table confirmation
    WAITING_FOR_RULE,   // Another player is choosing the hand's rule
    CHOOSE_RULE,        // Our turn to pick the rule from available options
    AUCTION_BID,        // Positiva: Player has to offer his Bid
    AUCTION_BIDDING,    // Positiva: Players are bidding for the right to choose Trump
    AUCTION_DECISION,   // Positiva: Auctioneer decides to accept the highest bid or keep it
    TRUMP_WAIT_CHOICE,  // Positiva: Another player is choosing the Trump suit
    TRUMP_CHOICE,       // Positiva: Player chooses the Trump suit
    TRICK_PLAYING,      // Main gameplay loop (playing cards into the center)
};

// ---------------------------------------------------------
// --- DATA STRUCTURES ---
// ---------------------------------------------------------

struct Player {
    std::string name;
    std::vector<int> scores; // Grows by 1 entry after every hand
};

struct AuctionData {
    int currentHighestBid = 0;
    int highestBidderIndex = -1; // Which of the 4 players holds the bid
    int auctioneerIndex = -1;    // The player who called Positiva
};

// ---------------------------------------------------------
// --- THE GLOBAL GAME STATE ---
// ---------------------------------------------------------

class Scene;

class GameState {
public:
    // Phase Tracking
    GamePhase currentPhase = GamePhase::INITIALIZING;

    // Player Data (Always 4 players in King)
    std::array<Player, 4> players;
    
    // Table Positioning
    int ourPlayerIndex = -1;       // Who are we? (0-3)
    int startingPlayerIndex = 0;   // Who goes first this hand?
    int currentPlayerTurn = -1;    // Whose turn is it right now?

    // Hand Data
    std::vector<Card> cardsOnHand;           // The 1 to 13 cards player holds for this hand
    std::vector<std::string> availableRules; // Populated when action is kRule
    std::string activeRule = "";             // The rule currently being played

    // Positiva Specifics
    AuctionData auctionState;
    Suit activeTrump = Suit::NO_TRUMP;

    // Trick Data
    std::vector<Card> cardsOnTable;  // The 0 to 4 cards currently played in this trick

    // Resource Cache
    uint32_t whiteTextureId = 0; // Guaranteed by the renderer initialization
    uint32_t cardTextureId = 0;
    uint32_t fontTextureId = 0;
    Font* mainFont = nullptr;    // Pointer so scenes can generate text vertices

    // The Event Bus    
    // Allows Scenes to send actions back to Haskell
    std::function<void(const std::string&)> submitAction;
    // Allows Scenes to ask the Application to feed them queued commands
    std::function<void(Scene*)> pollCommand;
    // Fetch the players Hand
    std::function<std::vector<std::string>()> getPlayerHand;
    // Fetch the available Rules
    std::function<std::vector<std::string>()> getAvailableRules;
};
