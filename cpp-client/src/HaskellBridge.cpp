#include "HaskellBridge.h"
#include "Logger.h"
#include <iostream>
#include <sstream>
#include <cstdlib>
#include <stdexcept>

// The raw Haskell C-API headers are isolated entirely to this file
#include "HsFFI.h"
#include "ClientFFI_stub.h"

// Static member initialization for the C-callback routing
static HaskellBridge* gBridge = nullptr;

// The raw C function that Haskell will actually call
extern "C" void c_callback_wrapper(char* action) {
    Logger::log(LogLevel::VERBOSE, "[HaskellBridge] Haskell passed an action: ", action);
    if (gBridge) {
        gBridge->pushAction(action);
    }
}

HaskellBridge::HaskellBridge(int argc, char* argv[]) {
    hs_init(nullptr, nullptr);
    Logger::log(LogLevel::VERBOSE, "[HaskellBridge] RTS Initialized.");
    gBridge = this;
}

HaskellBridge::~HaskellBridge() {
    gBridge = nullptr;
    hs_exit();
    Logger::log(LogLevel::VERBOSE, "[HaskellBridge] RTS Shutdown.");
}

void HaskellBridge::connectToServer(const std::string& subUrl, const std::string& pushUrl, 
                                    const std::string& player, const std::string& password) {
    start_client(
        (char*)subUrl.c_str(),
        (char*)pushUrl.c_str(),
        (char*)player.c_str(),
        (char*)password.c_str(),
        (HsFunPtr)c_callback_wrapper
    );
    Logger::log(LogLevel::INFO, "[HaskellBridge] Client started for player: ", player);
}

void HaskellBridge::pushAction(const char* action) {
    // Lock the mutex so the main thread can't read while we write
    std::lock_guard<std::mutex> lock(queueMutex);
    
    // Copy the C-string into a safe C++ string and queue it
    actionQueue.push(std::string(action));
}

bool HaskellBridge::pollAction(std::string& outAction) {
    // Lock the mutex so the background thread can't write while we read
    std::lock_guard<std::mutex> lock(queueMutex);
    
    if (actionQueue.empty()) {
        return false;
    }
    
    outAction = actionQueue.front();
    actionQueue.pop();
    return true;
}

void HaskellBridge::submitAction(const std::string& reply) {
    // This maps to your submitActionFFI exported from Haskell
    submit_action((void*)reply.c_str()); 
}

std::vector<std::string> HaskellBridge::getPlayerHand() {
    // 1. Call the generated FFI stub
    char* c_hand = (char*)get_player_hand();
    
    // 2. Safely copy the data into a C++ string
    std::string handStr(c_hand);
    
    // 3. CRITICAL: Free the memory allocated by Haskell's newCString!
    free(c_hand); 
    
    // 4. Split the space-separated string into a vector of cards
    std::vector<std::string> cards;
    std::istringstream iss(handStr);
    std::string card;
    while (iss >> card) {
        cards.push_back(card);
    }
    
    return cards;
}

std::vector<std::string> HaskellBridge::getAvailableRules() {
    char* c_rules = (char*)get_available_rules();
    std::string rulesStr(c_rules);
    free(c_rules); // CRITICAL: Prevent the memory leak!
    
    std::vector<std::string> rules;
    std::istringstream iss(rulesStr);
    std::string rule;
    while (iss >> rule) {
        rules.push_back(rule);
    }
    return rules;
}
