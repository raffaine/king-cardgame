#include "BaseGameScene.h"
#include <SDL2/SDL_vulkan.h>

#include "InGameScene.h"
#include "RuleSelectionScene.h"

#include "SceneManager.h"
#include "Logger.h"

BaseGameScene::BaseGameScene(SceneManager* mgr, GameState* st) : Scene(mgr, st) {}

void BaseGameScene::render(VulkanRenderer* renderer, Camera& camera) {
    std::vector<RenderBatch> batches;

    // Maybe render the Score, this is more of a fallback though

    int width, height;
    SDL_Vulkan_GetDrawableSize(renderer->getWindow(), &width, &height);
    glm::mat4 viewProj = camera.getViewProjectionMatrix((float)width, (float)height);

    renderer->drawFrame(batches, viewProj);
}

void BaseGameScene::update(float deltaTime) {
    // Tell the GameState Event Bus to feed us any pending network commands
    if (state->pollCommand) {
        state->pollCommand(this);
    }
}

void BaseGameScene::processCommand(const std::string& cmd) {
    if (cmd == "KRule") {
        Logger::log(LogLevel::INFO, "[BaseScene] Server requested Rule Selection!");

        // Fetch the strings from Haskell (e.g., "2H", "10S")
        std::vector<std::string> rawCards = state->getPlayerHand();
        
        // Clear the old hand
        state->cardsOnHand.clear();
        
        // Let's fan them out horizontally at the bottom of the screen
        float spacing = 0.18f; // Tighter overlap so 13 cards fit nicely
        float totalWidth = (rawCards.size() - 1) * spacing;
        float startX = -(totalWidth / 2.0f); // Perfectly center the layout
        float yPos = 0.7f; // Pull them up into the visible camera area

        for (const auto& cardId : rawCards) {
            // The Card constructor instantly parses the string and sets its own UVs!
            state->cardsOnHand.emplace_back(cardId, startX, yPos);
            startX += spacing;
        }

        state->currentPhase = GamePhase::CHOOSE_RULE;
        state->submitAction("GAME");
        manager->changeScene<RuleSelectionScene>();
        // The scene will now know to draw the rule selection buttons
    } 
    else if (cmd == "KPlay") {
        Logger::log(LogLevel::INFO, "[BaseScene] Server requested Play Selection!");
        state->currentPhase = GamePhase::TRICK_PLAYING;
        manager->changeScene<InGameScene>();
    }
    else if (cmd == "KBid") {
        state->currentPhase = GamePhase::AUCTION_BIDDING;
    }
    // ... Handle other commands ...
}
