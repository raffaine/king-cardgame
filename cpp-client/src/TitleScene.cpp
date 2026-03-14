#include "TitleScene.h"
#include <SDL2/SDL_vulkan.h>

#include "SceneManager.h"
#include "BaseGameScene.h" // We will transition to this!
#include "Logger.h"

#include <memory>
#include <thread>
#include <chrono>

TitleScene::TitleScene(SceneManager* mgr, GameState* st, std::function<void()> onStartCallback) 
    : Scene(mgr, st), onStart(onStartCallback) {}

void TitleScene::onEnter() {
    Logger::log(LogLevel::INFO, "Entered Title Scene. Press ENTER or Click to Start.");
    // (Later, we will draw the game logo here)
}

void TitleScene::onExit() {
    Logger::log(LogLevel::INFO, "Exiting Title Scene. Spawning Haskell backend...");
    // (Later, this is where we will trigger the OS process to launch king-server)
}

void TitleScene::handleInput(const SDL_Event& event) {
    if ((event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_RETURN) ||
        (event.type == SDL_MOUSEBUTTONDOWN)) {
        Logger::log(LogLevel::INFO, "Starting Match Orchestration...");

        // Launch the Game
        if (onStart) {
            onStart();
        }

        // Move the C++ State Machine forward
        state->currentPhase = GamePhase::INITIALIZING;
        
        // Transition to Next Scene
        manager->changeScene<BaseGameScene>();
    }
}

void TitleScene::update(float deltaTime) {
    // Idle animation logic for the title screen goes here
}

void TitleScene::render(VulkanRenderer* renderer, Camera& camera) {
    // For now, we just pass an empty vertex array so it draws the blank green screen
    std::vector<RenderBatch> emptyBatch;
    
    int width, height;
    SDL_Vulkan_GetDrawableSize(renderer->getWindow(), &width, &height);
    glm::mat4 viewProj = camera.getViewProjectionMatrix((float)width, (float)height);

    renderer->drawFrame(emptyBatch, viewProj);
}
