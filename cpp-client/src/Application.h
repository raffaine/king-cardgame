#pragma once
#include "VulkanRenderer.h"
#include "HaskellBridge.h"
#include <memory>

class Application {
public:
    Application(int argc, char* argv[]);
    void run();

private:
    std::unique_ptr<HaskellBridge> bridge;
    std::unique_ptr<VulkanRenderer> renderer;
    bool isRunning;

    void handleEvents();
    void onHaskellAction(const std::string& action);
};
