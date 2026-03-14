#include "RuleSelectionScene.h"
#include <SDL2/SDL_vulkan.h>

#include "Logger.h"

RuleSelectionScene::RuleSelectionScene(SceneManager* mgr, GameState* st) 
    : BaseGameScene(mgr, st) {}

void RuleSelectionScene::onEnter() {
    Logger::log(LogLevel::INFO, "[Scene Transition] Entered Rule Selection GUI");
    
    // Fetch the list of rules from Haskell!
    if (state->getAvailableRules) {
        availableRules = state->getAvailableRules();
        
        for (const auto& rule : availableRules) {
            Logger::log(LogLevel::INFO, "Available Rule: ", rule);
        }
    }
}

void RuleSelectionScene::handleInput(const SDL_Event& event) {
    // We will build the AABB mouse click detection here next!
}

void RuleSelectionScene::render(VulkanRenderer* renderer, Camera& camera) {
    std::vector<RenderBatch> batches;

    // --- BATCH 1: CARDS ---
    RenderBatch cardBatch;
    cardBatch.textureId = state->cardTextureId; // Use the ID from GameState!

    for (const auto& card : state->cardsOnHand) {
        card.addVertices(cardBatch.vertices);
    }
    
    // Only submit the batch if there's actually something to draw
    if (!cardBatch.vertices.empty()) {
        batches.push_back(cardBatch);
    }

    // --- SUBMIT TO VULKAN ---
    int width, height;
    SDL_Vulkan_GetDrawableSize(renderer->getWindow(), &width, &height);
    glm::mat4 viewProj = camera.getViewProjectionMatrix((float)width, (float)height);
    
    // Pass the grouped batches to the new rendering pipeline!
    renderer->drawFrame(batches, viewProj);
}
