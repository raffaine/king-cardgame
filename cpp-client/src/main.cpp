#include <SDL2/SDL.h>
#include <SDL2/SDL_vulkan.h>
#include <iostream>
#include <stdexcept>

// The Haskell headers
#include <HsFFI.h>
#include "ClientFFI_stub.h"

// The callback that Haskell will trigger
extern "C" void onActionRequired(char* action_type) {
    std::cout << "[Haskell Engine] Action required: " << action_type << std::endl;
    // We can interact with global C++ state here to flag the UI
}

int main(int argc, char *argv[]) {
    // 1. Boot the pure game state engine
    hs_init(&argc, &argv);
    std::cout << "Haskell RTS Initialized." << std::endl;

    start_client(
        (void*)"tcp://127.0.0.1:5555",
        (void*)"tcp://127.0.0.1:5556",
        (void*)"Alice",
        (void*)"pass",
        (HsFunPtr)&onActionRequired
    );

    // 2. Initialize the SDL2 Rendering Shell
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        std::cerr << "SDL_Init Error: " << SDL_GetError() << std::endl;
        hs_exit();
        return 1;
    }

    // Notice the SDL_WINDOW_VULKAN flag! This tells SDL to prepare the surface for Vulkan.
    SDL_Window* window = SDL_CreateWindow(
        "King - Vulkan Client",
        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
        1280, 720,
        SDL_WINDOW_VULKAN | SDL_WINDOW_SHOWN
    );

    if (!window) {
        std::cerr << "Window creation failed: " << SDL_GetError() << std::endl;
        SDL_Quit();
        hs_exit();
        return 1;
    }

    std::cout << "Vulkan Window Created Successfully." << std::endl;

    // --- Vulkan Instance Initialization goes here ---
    // (e.g., vkCreateInstance, SDL_Vulkan_CreateSurface)

    // 3. The Main Render/Input Loop
    bool running = true;
    SDL_Event event;

    while (running) {
        // Handle OS events and Input
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                running = false;
            }
            // Example Input Mapping:
            if (event.type == SDL_KEYDOWN && event.key.keysym.sym == SDLK_SPACE) {
                std::cout << "Spacebar pressed! Submitting action to Haskell..." << std::endl;
                running = false;
                // submit_action((char*)"PLAY Alice sec-A 10H");
            }
        }

        // --- Vulkan Render Pass goes here ---
    }

    // 4. Teardown and Cleanup
    SDL_DestroyWindow(window);
    SDL_Quit();
    hs_exit();

    return 0;
}
