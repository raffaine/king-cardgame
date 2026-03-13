#include "HaskellBridge.h"
#include "Logger.h"
#include <iostream>
#include <stdexcept>

// The raw Haskell C-API headers are isolated entirely to this file
#include "HsFFI.h"
#include "ClientFFI_stub.h"

// Static member initialization for the C-callback routing
std::function<void(const std::string&)> HaskellBridge::currentCallback = nullptr;

// The raw C function that Haskell will actually call
extern "C" void c_callback_wrapper(char* action_type) {
    HaskellBridge::runCallback(std::string(action_type));
}

HaskellBridge::HaskellBridge(int argc, char* argv[]) {
    hs_init(&argc, &argv);
    Logger::log(LogLevel::VERBOSE, "[HaskellBridge] RTS Initialized.");
}

HaskellBridge::~HaskellBridge() {
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

void HaskellBridge::setActionCallback(std::function<void(const std::string&)> callback) {
    currentCallback = callback;
}

/* static */ void HaskellBridge::runCallback(const std::string& arg) {
    if (currentCallback) {
        currentCallback(arg);
    }
}
