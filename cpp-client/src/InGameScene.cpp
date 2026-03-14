#include "InGameScene.h"
#include <SDL2/SDL_vulkan.h>
#include "Logger.h"

InGameScene::InGameScene(SceneManager* mgr, GameState* st) : BaseGameScene(mgr, st) {}

void InGameScene::onEnter() {
    Logger::log(LogLevel::INFO, "[Scene Transition] Entered In-Game Scene.");
}

void InGameScene::handleInput(const SDL_Event& event) {
    // Specific logic for clicking cards during the main game phase
    
    if ((event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_RETURN) ||
        (event.type == SDL_MOUSEBUTTONDOWN)) {
        for (const auto& card : state->cardsOnHand) {
            Logger::log(LogLevel::INFO, "Hand: ", card.id);
        }
    }
}

void InGameScene::render(VulkanRenderer* renderer, Camera& camera) {
    std::vector<RenderBatch> batches;

    // --- BATCH 1: CARDS ---
    RenderBatch cardBatch;
    cardBatch.textureId = state->cardTextureId; // Use the ID from GameState!

    for (const auto& card : state->cardsOnHand) {
        card.addVertices(cardBatch.vertices);
    }
    for (const auto& card : state->cardsOnTable) {
        card.addVertices(cardBatch.vertices);
    }
    
    // Only submit the batch if there's actually something to draw
    if (!cardBatch.vertices.empty()) {
        batches.push_back(cardBatch);
    }

    int width, height;
    SDL_Vulkan_GetDrawableSize(renderer->getWindow(), &width, &height);
    glm::mat4 viewProj = camera.getViewProjectionMatrix((float)width, (float)height);
    
    // Pass the grouped batches to the new rendering pipeline!
    renderer->drawFrame(batches, viewProj);
}

void InGameScene::update(float deltaTime) {
    BaseGameScene::update(deltaTime);
}

void InGameScene::processCommand(const std::string& cmd) {    
    if (cmd == "KPlay") {
        state->currentPhase = GamePhase::TRICK_PLAYING;
        
        return;
    }
    // ... Handle other commands ...
    BaseGameScene::processCommand(cmd);
}
