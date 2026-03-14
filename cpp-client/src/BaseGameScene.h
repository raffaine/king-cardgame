#pragma once
#include "Scene.h"

class BaseGameScene : public Scene {
public:
    BaseGameScene(SceneManager* mgr, GameState* st);
    virtual ~BaseGameScene() = default;

    // Implement the update loop here to parse Haskell messages for ALL child scenes    
    virtual void handleInput(const SDL_Event&) override {};
    virtual void update(float deltaTime) override;    
    virtual void render(VulkanRenderer* renderer, Camera& camera) override;

    // Implement the base processing to handle scene transitions
    virtual void processCommand(const std::string& cmd) override;
};
