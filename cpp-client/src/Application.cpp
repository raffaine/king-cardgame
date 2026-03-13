#include "Application.h"
#include "Logger.h"
#include <iostream>

// Helper to generate the 6 vertices for a single card
void addCardToBatch(std::vector<Vertex>& batch, float x, float y, int column, int row) {
    // Card dimensions in Vulkan normalized coordinates
    float width = 0.32f;
    float height = 0.5f;

    // UV calculations based on the 13x5 atlas
    float uvWidth = 1.0f / 13.0f;
    float uvHeight = 1.0f / 5.0f;

    float uStart = column * uvWidth;
    float uEnd = (column + 1) * uvWidth;
    float vStart = row * uvHeight;
    float vEnd = (row + 1) * uvHeight;

    // Triangle 1
    batch.push_back({{x - width, y - height}, {uStart, vStart}});
    batch.push_back({{x + width, y - height}, {uEnd,   vStart}});
    batch.push_back({{x + width, y + height}, {uEnd,   vEnd}});

    // Triangle 2
    batch.push_back({{x + width, y + height}, {uEnd,   vEnd}});
    batch.push_back({{x - width, y + height}, {uStart, vEnd}});
    batch.push_back({{x - width, y - height}, {uStart, vStart}});
}

Application::Application(int argc, char* argv[]) : isRunning(false) {
    // 1. Boot the Haskell Engine
    bridge = std::make_unique<HaskellBridge>(argc, argv);
    
    // Route Haskell events to our C++ class method
    bridge->setActionCallback([this](const std::string& action) {
        this->onHaskellAction(action);
    });

    bridge->connectToServer("tcp://127.0.0.1:5555", "tcp://127.0.0.1:5556", "Alice", "pass");

    // 2. Boot the GPU Renderer
    renderer = std::make_unique<VulkanRenderer>("King Game", 1280, 720);
}

void Application::run() {
    isRunning = true;
    Logger::log(LogLevel::VERBOSE, "[Application] Entering main loop.");

    while (isRunning) {
        handleEvents();
        
        // 1. Create our empty batch for this frame
        std::vector<Vertex> currentFrameVertices;

        // 2. Add the Ace of Spades on the left (Row 3, Col 0)
        addCardToBatch(currentFrameVertices, -0.4f, 0.0f, 0, 3);

        // 3. Add the King of Hearts on the right (Row 2, Col 12)
        addCardToBatch(currentFrameVertices, 0.4f, 0.0f, 12, 2);

        // 4. Send the batch to the GPU!
        renderer->drawFrame(currentFrameVertices);
    }

    // Wait for the GPU to finish its current frame before we destroy the window
    renderer->waitIdle();
}

void Application::handleEvents() {
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        if (event.type == SDL_QUIT) {
            isRunning = false;
        }
        // Catch the window resize event
        if (event.type == SDL_WINDOWEVENT) {
            if (event.window.event == SDL_WINDOWEVENT_RESIZED || event.window.event == SDL_WINDOWEVENT_SIZE_CHANGED) {
                renderer->framebufferResized = true;
            }
        }
        if (event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_SPACE) {
            Logger::log(LogLevel::INFO, "[Application] Spacebar pressed! Ready to submit action to Haskell.");
            // bridge->submitAction("PLAY ..."); 
        }
    }
}

void Application::onHaskellAction(const std::string& action) {
    Logger::log(LogLevel::INFO, "[Application] Received Action from Haskell: ", action);
}
