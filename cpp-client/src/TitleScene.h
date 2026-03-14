#pragma once
#include "Scene.h"
#include <functional>

class TitleScene : public Scene {
public:
    TitleScene(SceneManager* mgr, GameState* st, std::function<void()> onStartCallback);
    
    void onEnter() override;
    void onExit() override;
    void handleInput(const SDL_Event& event) override;
    void update(float deltaTime) override;
    void render(VulkanRenderer* renderer, Camera& camera) override;
    
private:
    std::function<void()> onStart; // Store the callback
};
