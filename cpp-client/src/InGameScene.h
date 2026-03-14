#pragma once
#include "BaseGameScene.h"

class InGameScene : public BaseGameScene {
public:
    InGameScene(SceneManager* mgr, GameState* st);
    
    void onEnter() override;
    void handleInput(const SDL_Event& event) override;
    void render(VulkanRenderer* renderer, Camera& camera) override;
    
    void update(float deltaTime) override;
    void processCommand(const std::string& cmd) override;
};
